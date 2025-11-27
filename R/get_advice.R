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
#'
#gives you general advice about your dataframe
get_advice <- function(x, ...) UseMethod("get_advice")

get_advice.MungrCleaner <- function(x, na_threshold = .5, factor_card_perc = .7, string_card_perc = 0.1,...){
  df <- x$data
  n_rows <- nrow(df)
  issues_found <- FALSE

  if (factor_card_perc < 0.5) {
    warning(sprintf("Cardinality percentage (%.0f%%) for identifying factor/string columns with high amount of variance (suggesting possible char-type) is low. Consider setting a value closer to 70%% (0.7) or higher.", card_perc * 100))
  }

  if (string_card_perc > 0.2) {
    warning(sprintf("Cardinality percentage (%.0f%%) for identifying string columns with little variance (suggesting possible factor-type) is high. Consider setting a value closer to 10%% (0.1) or lower.", card_perc * 100))
  }

  for(col_name in names(df)){
    col_data <- df[[col_name]]
    advice_list <- character()

    #Check Missing Data Severity
    n_na <- sum(is.na(col_data))
    if(n_na >0){
      percentage_is_na <- n_na/n_rows

      if(percentage_is_na > na_threshold){
        advice_list <- c(advice_list, sprintf("CRITICAL: %.0f%% missing. Consider dropping this column.", percentage_is_na *100))
      }else{
        advice_list <- c(advice_list, sprintf("Warning: %.0f%% missing. Consider imputing missing values with a custom value. If this is a numeric-type column, consider imputing values with mean, median, or mode", percentage_is_na*100))
      }
    }

    #Check empty strings (characters only)
    if (is.character(col_data) || is.factor(col_data)) {
      col_data_char <- as.character(col_data)
      n_empty <- sum(col_data_char == "", na.rm = TRUE)

      if (n_empty > 0) {
        advice_list <- c(advice_list, "Consistency Issue: Mixed empty strings and NAs. If empty fields aren't intentional, replace empty strings with this code: df[df == ''] <- NA")
      }

      # Check Case Sensitivity
      # If we lower-case everything, do we have fewer unique items? e.g., "Male", "male" -> "male" (2 items becomes 1)
      n_uniq_original <- length(unique(col_data_char))
      n_uniq_lower <- length(unique(tolower(col_data_char)))

      if (n_uniq_lower < n_uniq_original) {
        diff <- n_uniq_original - n_uniq_lower
        advice_list <- c(advice_list, sprintf("Formatting issue: Case inconsistency detected (%d duplicates). You have the same values counted as unique from each other due to case sensitivity. If this is not intentional, run: toupper() or tolower()", diff))
      }


      #Check for High Cardinality (Too many categories)
      # If a high percentage (card_perc) of the rows are unique, it's likely an ID or free text, not a category
      if (n_uniq_original > (n_rows * factor_card_perc)) {
        advice_list <- c(advice_list, sprintf("Datatype: There's a lot of unique categories; %.0f%% of your values are unique. This looks like an ID or free text.", (factor_card_perc)*100))
      }

      #Check for Unique Values (Column may be a category)
      uniq_ratio <- n_uniq_lower / n_rows
      if (uniq_ratio <= string_card_perc && n_uniq_lower > 1){
        advice_list <- c(advice_list, sprintf("Datatype: This column has low cardinality; %.1f%% of your column has variance. You may want to change this column to a factor-type.", uniq_ratio*100))
      }
    }
    #Check for Zero Variance (Useless column)
    # If there is only 1 unique value (and it's not just all NAs)
    if (length(unique(na.omit(col_data))) == 1) {
      advice_list <- c(advice_list, "Redundancy: Column has zero variance (same value for entire column). You might want to drop it.")
    }

    #Final results
    if (length(advice_list) > 0) {
      issues_found <- TRUE
      cat(sprintf("Column [%s]:\n", col_name))
      # Print advice with a bullet point
      cat(sprintf("  -> %s", advice_list), sep = "\n")
      cat("\n")
    }

  } #end of for loop
  if (!issues_found) {
    cat("No obvious data issues found. Good job!\n")
  }
  return(x)
}
