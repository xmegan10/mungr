#' Load a Matrix
#'
#' This function loads a file as a matrix. It assumes that the first column
#' contains the rownames and the subsequent columns are the sample identifiers.
#' Any rows with duplicated row names will be dropped with the first one being
#' kepted.
#'
#' @param infile Path to the input file
#' @return A matrix of the infile
#' @export
#'
#combines all methods into one. Allow users to quickly clean dataframe without worrying about pipelines. Users can choose which functions to keep
quick_clean <- function(x, ...) UseMethod("quick_clean")

quick_clean.MungrCleaner <- function(x,
                                     clean_txt = TRUE,
                                     std_coltypes = TRUE,
                                     impute = TRUE,
                                     text_case = "title",
                                     num_strategy = "median",
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
    x <- impute_missing(x, num_val = num_strategy, char_val = "Unknown")
  }

  x$log <- c(x$log, "===End Quick Clean===")

  # Return the clean dataframe directly (User friendliness)
  return(x)
}
