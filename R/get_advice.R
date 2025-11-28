#' @title Get Advice from MungrCleaner
#'
#' @description This generic function analyzes a MungrCleaner object data frame and identifies common data quality issues
#'    (missing values, empty strings, case inconsistency, and inappropriate
#'    cardinality). It prints suggested cleaning actions to the console.
#'
#' @param x An object containing data to be processed (e.g., a \code{MungrCleaner} object).
#' @param ... Additional arguments passed to the S3 method.
#' @return The function is primarily called for its side effect of printing advice.
#'   It invisibly returns the input object \code{x}.
#' @export
get_advice <- function(x, ...) UseMethod("get_advice")

#' @param na_threshold Numeric. The proportion of missing NA values
#'   above which a column is flagged as CRITICAL, suggesting it should be
#'   dropped. Default value is 0.5.
#' @param factor_card_perc Numeric. The ratio of unique values to total rows
#'   above which a column (currently character/factor) is flagged as having
#'   High Cardinality, suggesting it should be treated as an ID or free text.
#'   Default value is 0.7.
#' @param string_card_perc Numeric. The ratio of unique values to total rows
#'   below which a column (currently character/factor) is flagged as having
#'   Low Cardinality, suggesting it should be converted to a factor.
#'   Default is 0.1.
#' @param ... Additional arguments (not used).
#' @return Invisibly returns the input object \code{x} with no modifications
#'   to the data or log.
#' @details
#' The function checks for the following issues for each column: missing data serverity,
#' empty strings, case sensitiviy, high cardinality (flags potential ID/free-text columns),
#' low cardinality (flags potential factor/categorical columns), and zero variance
#'
#' @examples
#' # Example data demonstrating various issues
#' messy_df <- data.frame(
#'   ID = c(1, 2, 3, 4, 5, 6),
#'   Category = c("A", "a", "B", "C", "C", NA),
#'   Notes = c(letters[1:5], ""), # Example of empty string
#'   Useless = rep(10, 6)
#' )
#'
#' cleaner <- MungrCleaner(data_messy)
#'
#' # Get default advice
#' get_advice(cleaner)
#'
#' # Get advice with a stricter missing threshold (e.g., 20%)
#' get_advice(cleaner, na_threshold = 0.2)
#' @usage \method{get_advice}{MungrCleaner}(x, na_threshold = 0.5, factor_card_perc = 0.7, string_card_perc = 0.1, ...)
#'
#'
#' @export
#' @rdname get_advice
#'

get_advice.MungrCleaner <- function(x, na_threshold = 0.5, factor_card_perc = 0.7, string_card_perc = 0.1,...){
  df <- x$data
  n_rows <- nrow(df)
  issues_found <- FALSE

  if (factor_card_perc < 0.5) {
    warning(sprintf("Cardinality percentage (%.0f%%) for identifying factor/string columns with high amount of variance (suggesting possible char-type) is low. Consider setting a value closer to 70%% (0.7) or higher.", factor_card_perc * 100))
  }

  if (string_card_perc > 0.2) {
    warning(sprintf("Cardinality percentage (%.0f%%) for identifying string columns with little variance (suggesting possible factor-type) is high. Consider setting a value closer to 10%% (0.1) or lower.", string_card_perc * 100))
  }

  for(col_name in names(df)){
    col_data <- df[[col_name]]
    advice_list <- character()

    #---check missing data severity---
    n_na <- sum(is.na(col_data))
    if(n_na >0){
      percentage_is_na <- n_na/n_rows

      if(percentage_is_na > na_threshold){
        advice_list <- c(advice_list, sprintf("CRITICAL: %.0f%% missing. Consider dropping this column.", percentage_is_na *100))
      }else{
        advice_list <- c(advice_list, sprintf("Warning: %.0f%% missing. Consider imputing missing values with a custom value. If this is a numeric-type column, consider imputing values with mean, median, or mode", percentage_is_na*100))
      }
    }

    #---check empty strings (characters only)---
    if (is.character(col_data) || is.factor(col_data)) {
      col_data_char <- as.character(col_data)
      n_empty <- sum(col_data_char == "", na.rm = TRUE)

      if (n_empty > 0) {
        advice_list <- c(advice_list, "Consistency Issue: Mixed empty strings and NAs. If empty fields aren't intentional, replace empty strings with this code: df[df == ''] <- NA")
      }

      #---check case sensitivity---
      # If we lower-case everything, do we have fewer unique items?
      n_uniq_original <- length(unique(col_data_char))
      n_uniq_lower <- length(unique(tolower(col_data_char)))

      if (n_uniq_lower < n_uniq_original) {
        diff <- n_uniq_original - n_uniq_lower
        advice_list <- c(advice_list, sprintf("Formatting issue: Case inconsistency detected (%d duplicates). You have the same values counted as unique from each other due to case sensitivity. If this is not intentional, run: toupper() or tolower()", diff))
      }


      #---Check for High Cardinality (Too many categories)---
      # If a high percentage (card_perc) of the rows are unique, it's likely an ID or free text, not a category
      if (n_uniq_original > (n_rows * factor_card_perc)) {
        advice_list <- c(advice_list, sprintf("Datatype: There's a lot of unique categories; %.0f%% of your values are unique. This looks like an ID or free text.", (factor_card_perc)*100))
      }

      #---Check for Unique Values (Column may be a category)---
      uniq_ratio <- n_uniq_lower / n_rows
      if (uniq_ratio <= string_card_perc && n_uniq_lower > 1){
        advice_list <- c(advice_list, sprintf("Datatype: This column has low cardinality; %.1f%% of your column has variance. You may want to change this column to a factor-type.", uniq_ratio*100))
      }
    }
    #---Check for Zero Variance (Useless column)---
    # If there is only 1 unique value (and it's not just all NAs)
    if (length(unique(na.omit(col_data))) == 1) {
      advice_list <- c(advice_list, "Redundancy: Column has zero variance (same value for entire column). You might want to drop it.")
    }

    #---Final results---
    if (length(advice_list) > 0) {
      issues_found <- TRUE
      cat(sprintf("Column [%s]:\n", col_name))
      # Print advice with a bullet point
      cat(sprintf("  -> %s", advice_list), sep = "\n")
      cat("\n")
    }

  } #---end of for loop---
  if (!issues_found) {
    cat("No obvious data issues found. Good job!\n")
  }
  return(x)
}
