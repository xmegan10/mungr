#' @title Quick Clean from MungrCleaner
#'
#' @description This generic function applies a standardized,
#'   multi-step sequence of data cleaning and type standardization functions to
#'   a MungrCleaner object.
#'
#' @param x An object containing data to be processed (e.g., a MungrCleaner object).
#' @param ... Additional arguments passed to underlying standardization functions
#'   (standardize_strcols and standardize_numcols).
#' @return The updated MungrCleaner object, which contains the cleaned data
#'   frame and a detailed log of all steps performed.
#' @export
quick_clean <- function(x, ...) UseMethod("quick_clean")

#' @param x An object of class MungrCleaner.
#' @param clean_txt Logical. If TRUE, runs \code{\link{clean_text}} to trim
#'   whitespace and convert empty strings ("") to NA. Default is TRUE.
#' @param std_coltypes Logical. If TRUE, runs \code{\link{standardize_strcols}}
#'   and \code{\link{standardize_numcols}} to convert string columns to Date/Numeric/Factor
#'   and refine numeric columns to Integer/Logical/Factor. Default is TRUE.
#' @param impute Logical. If TRUE, runs \code{\link{impute_missing}} to fill
#'   remaining NA/empty values. Default is TRUE.
#' @param text_case Character. The case to apply during text cleaning. Passed to
#'   clean_text. Options include "title" (default), "lower", or "upper".
#' @param imp_strategy Character. The imputation strategy for numeric columns.
#'   Passed to impute_missing. Options are "median" (default),
#'   "mean", or "mode".
#' @param ... Additional arguments passed directly to the underlying standardization
#'   methods (e.g., num_threshold or cat_threshold).
#'
#' @details
#' The quick_clean function runs the core cleaning methods in a fixed sequence:
#'
#' 1.  \strong{Text Cleaning}: \code{\link{clean_text}} is applied with \code{trim = TRUE}
#'     and the specified \code{text_case}.
#' 2.  \strong{Type Standardization}: \code{\link{standardize_strcols}} and
#'     \code{\link{standardize_numcols}} are run to coerce column types based on content.
#' 3.  \strong{Imputation}: \code{\link{impute_missing}} is run using the specified
#'     num_strategy for numeric columns and "Unknown" for character columns.
#'
#' Arguments passed via \code{...} (e.g., \code{date_threshold = 0.8}) will be forwarded
#' to the standardization steps.
#'
#' @examples
#' # Assume MungrCleaner and its methods are defined
#'
#' # --- 1. Example Data Setup ---
#' messy_df <- data.frame(
#'   ID = c("id_1", "id_2", "id_3", NA),
#'   Category = c("apple", " banana ", "Apple", ""), # Case and spacing issues
#'   Value = c("100", NA, "300", "400"), # String-numeric mix
#'   Date = c("2022-01-01", "2022-01-02", NA, "2022-01-04")
#' )
#' cleaner <- MungrCleaner(messy_df)
#'
#' # --- 2. Run Quick Clean ---
#' # Defaults: text_case="title", num_strategy="median"
#' # The standardization steps will handle the Value and Date columns.
#' cleaner_qc <- quick_clean(cleaner)
#'
#' # --- 3. Expected Results ---
#' # - ID: NA imputed with median (if type is numeric, but here it stays string)
#' # - Category: "apple", "Banana", "Apple" (title case, trimmed)
#' # - Value: Converted to numeric; NA imputed with median(100, 300, 400) = 300.
#' # - Date: Converted to Date; NA imputed with median/mode (mode isn't numeric here, so it stays NA/imputed with strategy based on the underlying method logic).
#'
#' print(cleaner_qc$data)
#' print(cleaner_qc$log)
#'
#' @usage \method{quick_clean}{MungrCleaner}(x, clean_txt = TRUE, std_coltypes = TRUE, impute = TRUE, text_case = "title", imp_strategy = "mean", ...)
#' @export
#' @rdname quick_clean

quick_clean.MungrCleaner <- function(x,
                                     clean_txt = TRUE,
                                     std_coltypes = TRUE,
                                     impute = TRUE,
                                     text_case = "title",
                                     imp_strategy = "mean",
                                     ...) {

  x$log <- c(x$log, "===Quick Clean===")

  # 1. Text Cleaning
  if (clean_txt) {
    # We call the method defined in step 1
    x <- clean_text(x, case = text_case, trim = TRUE)
  }

  # Type Standardization
  if (std_coltypes) {
    # Check if methods exist before calling to prevent errors
    if (exists("standardize_strcols.MungrCleaner")) {
      x <- standardize_strcols(x, ...)
    }
    if (exists("standardize_numcols.MungrCleaner")) {
      x <- standardize_numcols(x, ...)
    }
  }

  # 3. Imputation
  if (impute) {
    x <- impute_missing(x, imp_type = imp_strategy, char_val = "Unknown")
  }

  x$log <- c(x$log, "===End Quick Clean===")

  # Return the clean dataframe directly (User friendliness)
  return(x)
}
