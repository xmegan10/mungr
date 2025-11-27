#fill NAs with a specific value or imput with mean/median/mode based on num_val = argument and char_val = "Unknown"
impute_missing <- function(x, ...) UseMethod("impute_missing")

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
