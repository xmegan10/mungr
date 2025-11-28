#' @title Impute Missing Values From MungrCleaner
#'
#' @description This generic function for fills missing (NA) values
#'   in data columns using various strategies (e.g., mean, median, mode, or a custom value).
#'
#' @param x An object containing data to be processed (e.g., a MungrCleaner object).
#' @param ... Additional arguments passed to the S3 method.
#' @return The updated MungrCleaner object with imputed values in its
#'   data component and a log entry detailing the changes.
#' @export
impute_missing <- function(x, ...) UseMethod("impute_missing")

#' @param x An object of class MungrCleaner containing the data to clean.
#' @param col_name Character. The name of a single column to process. If argument equals ""
#'   (default), all numeric, character, and factor columns are processed.
#' @param imp_type Character. The imputation strategy to use. Options are:
#'   "mean", "median", "mode" (for numeric columns),
#'   "custom_num" (fills numeric fields with num_val), or
#'   "custom_char" (fills character/factor fields with char_val).
#'   Default is "mean".
#' @param num_val Numeric. The custom value to use for numeric columns when
#'   imp_type is "custom_num". Default is 0.
#' @param char_val Character or Factor. The custom value to use for character/factor
#'   columns when imp_type is "custom_char". Default is "Unknown".
#' @param ... Additional arguments (not used).
#'
#' @details
#' The imputation process targets numeric, character, and factor columns.
#' Fills numeric columns with NAs based on imp_types: "mean", "median", "mode" (most frequent non-NA value),
#' or "custom_num" (value provided in custom_num argument). Fills character/factor columns when
#' imp_type is "custom_char" - NAs will then be replaced with the value provided in "char_val".
#'
#' @examples
#' # Assume MungrCleaner and dependencies are defined
#'
#' # --- 1. Example Data Setup ---
#' messy_df <- data.frame(
#'   Age = c(25, 30, NA, 40, 30),
#'   Income = c(50000, 60000, 70000, NA, 80000),
#'   Status = c("A", NA, "B", "A", "B"),
#'   stringsAsFactors = FALSE
#' )
#' cleaner <- MungrCleaner(messy_df)
#'
#' # --- 2. Run Imputation ---
#'
#' # A. Impute numeric columns using the median (Income NA is filled with median 70000)
#' cleaner_median <- impute_missing(cleaner, imp_type = "median")
#'
#' # B. Impute character columns using a custom string
#' cleaner_custom_char <- impute_missing(cleaner, imp_type = "custom_char",
#'                                       char_val = "Missing Status")
#'
#' # C. Impute only the 'Age' column using a custom numeric value
#' cleaner_custom_num_age <- impute_missing(cleaner, col_name = "Age",
#'                                          imp_type = "custom_num", num_val = -1)
#'
#' # --- 3. Expected Results ---
#' # The 'Age' column's NA is now -1.
#' # The 'Income' column's NA is now 70000
#' # The 'Status' column's NA is now "Missing Status"
#'
#' print(cleaner_cleaned$data)
#'
#' @importFrom dplyr mutate across all_of
#' @importFrom tidyr replace_na
#' @usage \method{impute_missing}{MungrCleaner}(x, col_name = "", imp_type = "mean", num_val = 0, char_val = "Unknown", ...)
#' @export
#' @rdname impute_missing

impute_missing.MungrCleaner <- function(x, col_name = "",imp_type = "mean", num_val = 0, char_val = "Unknown", ...) {

  df <- x$data
  cols_changed <- character()

  if(col_name == ""){
    cols_to_process <- names(df)
  } else {
    if(!col_name %in% names(df)) stop(paste("Column", col_name, "not found."))
    cols_to_process <- col_name
  }

  num_cols <- cols_to_process[sapply(df[cols_to_process], is.numeric)]
  char_cols <- cols_to_process[sapply(df[cols_to_process], is.character) | sapply(df[cols_to_process], is.factor)]

  # Helper function for Mode
  get_mode <- function(v) {
    v <- v[!is.na(v)]
    if(length(v) == 0) return(NA)
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }

  # --- A. Numeric Imputation ---
  if(length(num_cols) > 0 && imp_type != ""){

    impute_func <- switch(imp_type,
                          "mean" = function(.) ifelse(is.na(.), mean(., na.rm = TRUE), .),
                          "median" = function(.) ifelse(is.na(.), median(., na.rm = TRUE), .),
                          "mode" = function(.) ifelse(is.na(.), get_mode(.), .),
                          "custom_num" = function(.) tidyr::replace_na(., num_val),
                          NULL)

    if (!is.null(impute_func)) {
      df <- df |>
        dplyr::mutate(dplyr::across(!!dplyr::all_of(num_cols), impute_func))
      cols_changed <- c(cols_changed, paste0(num_cols, " (Num Imputed)"))
    }
  }

  # --- B. Character Imputation (Custom Imputation Only) ---
  if (length(char_cols) > 0 && imp_type == "custom_char") {

    for (col in char_cols){
      if (is.factor(df[[col]])){
        df[[col]] <- as.character(df[[col]])
        warning(sprintf("Column '%s' was converted to 'character' to safely impute with '%s'.",
        col,
        as.character(char_val)))
      }
    }

    if (is.character(char_val) || is.factor(char_val)) {
      df <- df |>
        dplyr::mutate(dplyr::across(!!dplyr::all_of(char_cols), ~ tidyr::replace_na(., as.character(char_val))))
      cols_changed <- c(cols_changed, paste0(char_cols, " (Char Imputed)"))
    }
  }

  x$data <- df

  # Log
  if (length(cols_changed) > 0) {
    msg <- sprintf("Imputed Missing: %d columns changed: [%s]",
                   length(cols_changed), paste(unique(cols_changed), collapse = ", "))
    x$log <- c(x$log, msg)
  } else {
    x$log <- c(x$log, "Imputed Missing: No changes made.")
  }

  return(x)
}
