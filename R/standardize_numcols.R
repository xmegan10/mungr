#' @title Standardize Numeric Columns from MungrCleaner
#'
#' @description This generic function refines the data type of
#'   numeric columns within a MungrCleaner object, converting them into a Boolean type,
#'   factor (Categorical) type, or integer (Memory Optimization) type
#'   based on their content.
#'
#' @param x An object containing data to be processed (e.g., a MungrCleaner object).
#' @param ... Additional arguments passed to the S3 method.
#' @return The updated MungrCleaner object with converted columns in its
#'   data component and a log entry detailing the changes.
#' @export
standardize_numcols <- function(x, ...) UseMethod("standardize_numcols")

#' @param x An object of class MungrCleaner containing the data to clean.
#' @param col_name Character. The name of a single column to process. If argument equals "",
#'   (default), all numeric columns are processed.
#' @param convert_int Logical. Whether to convert double-precision numeric columns
#'   that contain only whole numbers and fit within R's integer limits to integer
#'   type to save memory. Default is TRUE.
#' @param cat_threshold Numeric. The maximum ratio of unique non-NA values to
#'   total non-NA values for a column to be converted to a factor (e.g., 0.1 means unique values must be less than 10% of the total).
#'   Default is 0.1.
#' @param ... Additional arguments (not used).
#'
#' @details
#' The conversion process is sequential and mutually exclusive:
#' if a column is converted to an earlier type (e.g., Logical), subsequent checks
#' (Factor, Integer) are skipped. The order of checks is:
#' Logical to Factor to Integer.
#'
#' @examples
#' # Assume MungrCleaner and standardize_numcols generic are defined.
#'
#' # --- 1. Example Data Setup ---
#' messy_df <- data.frame(
#'   Is_Active = c(1, 0, 1, 0, 1, 0), # Potential Boolean
#'   Category_ID = c(101, 102, 101, 102, 101, 102), # Potential Factor
#'   Price_Double = c(100.0, 250.0, 100.0, 150.0, 150.0, 200.0), # Potential Integer
#'   stringsAsFactors = FALSE
#' )
#'
#' # Assume MungrCleaner object creation
#' cleaner <- MungrCleaner(messy_df)
#'
#' # --- 2. Run Standardization with Defaults ---
#' # Thresholds: cat_threshold = 0.1 (10%)
#' cleaner_standardized <- standardize_numcols(cleaner)
#'
#' str(cleaner_standardized$data)
#'
#' # --- 3. Expected Results ---
#' # $ Is_Active: logi  TRUE FALSE TRUE FALSE TRUE FALSE (Converted to Logical)
#' # $ Category_ID: Factor w/ 2 levels "101","102" (Converted to Factor: 2 unique values / 6 rows = 0.33 > 0.1,
#' #                but if threshold was e.g. 0.4, it would convert)
#' # $ Price_Double: int  100 250 100 150 150 200 (Converted to Integer)
#'
#' @usage \method{standardize_numcols}{MungrCleaner}(x, col_name = "", convert_int = TRUE, cat_threshold = 0.1, ...)
#' @export
#' @rdname standardize_numcols

standardize_numcols.MungrCleaner <- function(x, col_name = "", convert_int = TRUE, cat_threshold = 0.1, ...){
  #get dataframe
  df <- x$data
  cols_changed <- character()

  if(col_name == ""){
    cols_to_process <- names(df)
  } else {
    if(!col_name %in% names(df)) stop(paste("Column", col_name, "not found."))
    cols_to_process <- col_name
  }

  #start for loop
  for(col_name in names(df)){
    #check character columns
    if(is.numeric(df[[col_name]])){
      vals <- df[[col_name]]
      non_na_vals <- vals[!is.na(vals)]

      if(length(non_na_vals) == 0) next #skip empty columns
      converted <- FALSE

      #---convert numeric to boolean---
        u_vals <- unique(non_na_vals)

        if (all(u_vals %in% c(0,1))){
          df[[col_name]] <- as.logical(df[[col_name]])
          cols_changed <- c(cols_changed, paste0(col_name, " (-> Bool)"))
          converted <- TRUE
        }
      if (converted) next

      #---convert numeric to categorical factor---

      n_unique <- length(unique(non_na_vals))
      n_total <- length(non_na_vals)

      #ensure column doesn't contain just 1 unique or all unique
      unique_ratio <- n_unique/n_total

      if(unique_ratio <= cat_threshold && n_unique > 1){
        df[[col_name]] <- as.factor(df[[col_name]])
        cols_changed <- c(cols_changed, paste0(col_name, " (-> Factor)"))
        converted <- TRUE
      }

      if (converted) next
      #---convert doubles to ints to save memory---
      if(convert_int && is.double(df[[col_name]])){
        if(all(non_na_vals %% 1 == 0)) {
          if(max(non_na_vals) < 2e9 && min(non_na_vals > -2e9)){
            df[[col_name]] <- as.integer(df[[col_name]])
            cols_changed <- c(cols_changed, paste0(col_name, "(-> Int)"))
          }
        }
      }
    }
  } #end of loop inside if statement

  #return new df
  x$data <- df

  #log the results
  if (length(cols_changed) > 0) {
    msg <- sprintf("Standardized Num Column Types: Converted %d columns: [%s]", length(cols_changed), paste(cols_changed, collapse = ", "))
    x$log <- c(x$log, msg)
  } else {
    x$log <- c(x$log, "Standardized Num Column Types: No columns met conversion conditions.")
  }

  return(x)

}
