#' @title Clean Text from MungrCleaner
#'
#' @description This generic function standardizes text in
#'   character columns by trimming whitespace, converting empty strings
#'   to NA, and standardizing cases.
#'
#' @param x An object containing data to be processed (e.g., a MungrCleaner object).
#' @param ... Additional arguments passed to the S3 method.
#' @return The updated MungrCleaner object.
#' @export
clean_text <- function(x, ...) UseMethod("clean_text")

#' @param x An object of class MungrCleaner containing the data to clean.
#' @param case Character. Specifies the case conversion to apply to character
#'   columns. Options are "lower", "upper", or "title". If
#'   NULL (default), no case conversion is performed.
#' @param trim Logical. If TRUE (default), leading and trailing whitespace
#'   is removed from all character columns, and any resulting empty strings ("")
#'   are converted to NA.
#' @param ... Additional arguments (not used).
#' @return The updated MungrCleaner object with cleaned character columns
#'   in its $data component and an entry in the processing $log.
#'
#' @details
#' The cleaning process is applied only to columns of type character.
#' The order of operations is always: Trim and Replace Empty to Whitespace is trimmed to
#' empty strings replaced with NA values to case conversion transformation.
#'
#'
#' @examples
#' # Assume MungrCleaner and clean_text generic are defined.
#'
#' # --- 1. Example Data Setup ---
#' messy_df <- data.frame(
#'   Text_Case = c(" Apple ", "banana", "Cherry"),
#'   Text_Empty = c(" hello ", " ", ""),
#'   stringsAsFactors = FALSE
#' )
#' cleaner <- MungrCleaner(messy_df)
#'
#' # --- 2. Run Cleaning ---
#' # Trim whitespace, convert empty to NA, and force lowercase
#' cleaner_cleaned <- clean_text(cleaner, case = "lower", trim = TRUE)
#'
#' # --- 3. Expected Results ---
#' # The 'Text_Empty' column's " " and "" rows are now NA.
#' # 'Text_Case' is now "apple", "banana", "cherry".
#'
#' print(cleaner_cleaned$data)
#'
#'
#' @importFrom dplyr mutate across where na_if
#' @importFrom stringr str_trim str_to_title
#' @usage \method{clean_text}{MungrCleaner}(x, case = NULL, trim = TRUE, ...)
#' @export
#' @rdname clean_text

clean_text.MungrCleaner <- function(x, case = NULL, trim = TRUE, ...) {

  df <- x$data

  #---Trim Whitespace & Fix Empty Strings---
  if (trim) {
    df <- df |>
      dplyr::mutate(dplyr::across(where(is.character), stringr::str_trim)) |>
      dplyr::mutate(dplyr::across(where(is.character), ~ dplyr::na_if(., "")))
  }

  #---Case Conversion (Optional)---
  if (!is.null(case)) {
    case_func <- switch(tolower(case),
                        "lower" = tolower,
                        "upper" = toupper,
                        "title" = stringr::str_to_title,
                        NULL)

    if (!is.null(case_func)) {
      df <- df |>
        dplyr::mutate(dplyr::across(where(is.character), case_func))
    }
  }

  x$data <- df

  #---Log---
  action_msg <- "Text cleaned (Empty -> NA)"

  if (trim) action_msg <- paste0(action_msg, ", Trimmed")
  if (!is.null(case)) action_msg <- paste0(action_msg, ", Case: ", case)
  x$log <- c(x$log, paste0(action_msg, ")"))

  return(x)
}

