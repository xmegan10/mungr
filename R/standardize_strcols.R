#' @title Standardize String Columns from MungrCleaner
#'
#' @description This generic function refines the data type of
#'   string columns within a MungrCleaner object, converting them to
#'   a Boolean type, numerical type, date type, or factor (Categorical) type
#'   based on their content.
#'
#' @param x An object containing data to be processed (e.g., a MungrCleaner object).
#' @param ... Additional arguments passed to the S3 method.
#' @return The updated MungrCleaner object with converted columns in its
#'   data component and a log entry detailing the changes..
#' @export
standardize_strcols <- function(x, ...) UseMethod("standardize_strcols")


#' @param x An object of class MungrCleaner containing the data to clean.
#' @param col_name Character. The name of a single column to process. If argument equals "",
#'   (default), all character and factor columns are processed.
#' @param num_threshold Numeric. The minimum proportion of non-NA/non-empty
#'   values that must successfully convert to numeric (after cleaning symbols)
#'   for the column to be converted (e.g., 0.7 means more than 70% of non-empty values must be successfully converted to numeric).
#'   Default is 0.7.
#' @param date_threshold Numeric. The minimum proportion of non-NA/non-empty
#'   values that must successfully parse into a date object for the column to be
#'   converted (e.g., 0.5 means more than 50% of non-empty values must parse into a date object). Default is 0.5.
#' @param cat_threshold Numeric. The maximum ratio of unique non-NA/non-empty
#'   values to total non-NA/non-empty values for a column to be converted to a
#'   factor (e.g., 0.1 means unique values must be less than 10% of the total). Default is 0.1.
#' @param date_format Character. The format to assume for date parsing. Must be
#'   one of "ymd", "mdy", or "dmy". Default is "ymd".
#' @param ... Additional arguments (not used).
#'
#' @details
#' The conversion process is sequential and mutually exclusive:
#' if a column meets the threshold for an earlier type (e.g., Boolean), subsequent
#' checks (Numeric, Date, Factor) are skipped. The strict order of evaluation is:
#' Boolean to Numeric to Date to Factor.
#'
#' @examples
#' # Assume MungrCleaner and standardize_strcols generic are defined.
#'
#' # --- 1. Example Data Setup ---
#' messy_df <- data.frame(
#'   A_Bool = c("T", "F", "1", "0", "YES", "NO"),
#'   B_Price = c("$100.50", "200", "300", "400", "500", "ERROR"), # 1 failure
#'   C_Date = c("2023-01-01", "2023-01-02", "2023-01-03",
#'              "2023-01-04", "2023-01-05", "N/A"), # 1 failure
#'   D_Cat = c("Red", "Blue", "Red", "Blue", "Red", "Blue"),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Assume MungrCleaner object creation
#' cleaner <- MungrCleaner(messy_df)
#'
#' # --- 2. Run Standardization with Defaults ---
#' # Defaults: num_threshold = 0.7 (70%), date_threshold = 0.5 (50%), cat_threshold = 0.1 (10%)
#' cleaner_standardized <- standardize_strcols(cleaner)
#'
#' # --- 3. Expected Results ---
#' # A_Bool: Converted to Logical (100% success).
#' # B_Price: Numeric check ratio is 5/6 (83.3%) >= 0.7. Converted to numeric (removes '$').
#' # C_Date: Date check ratio is 5/6 (83.3%) >= 0.5. Converted to Date.
#' # D_Cat: Unique ratio is 2/6 (33.3%) > 0.1. Stays Character (unless cat_threshold is set higher).
#'
#' str(cleaner_standardized$data)
#'
#' @importFrom lubridate ymd mdy dmy
#' @usage \method{standardize_strcols}{MungrCleaner}(x, col_name = "", num_threshold = 0.7, date_threshold = 0.5, cat_threshold = 0.1, date_format = "ymd", ...)
#' @export
#' @rdname standardize_strcols

standardize_strcols.MungrCleaner <- function(x, col_name = "", num_threshold = 0.7, date_threshold = 0.5, cat_threshold = 0.1, date_format = "ymd",...){
  #get dataframe
  df <- x$data
  cols_changed <- character()

  if(col_name == ""){
    cols_to_process <- names(df)
  } else {
    if(!col_name %in% names(df)) stop(paste("Column", col_name, "not found."))
    cols_to_prcocess <- col_name
  }

  #select date format
  date_parser <- switch(date_format,
                        "ymd" = lubridate::ymd,
                        "mdy" = lubridate::mdy,
                        "dmy" = lubridate::dmy,
                        lubridate::ymd)

  #start for loop
  for(col_name in names(df)){
    #check character columns
    if(is.character(df[[col_name]]) || is.factor(df[[col_name]])){
      vals <- as.character(df[[col_name]])
      non_na_vals <- vals[!is.na(vals) & vals != ""]

      if(length(non_na_vals) == 0) next #skip empty columns
      converted <- FALSE

      #---convert strings to boolean type---
      upper_vals <- toupper(unique(non_na_vals))
      valid_bools <- c("TRUE", "FALSE", "T", "F", "1", "0", "YES", "NO")

      if (all(upper_vals %in% valid_bools)) {
        try_bool <- as.logical(vals)

        if (sum(is.na(try_bool)) / length(vals) < 0.1) {
          df[[col_name]] <- try_bool
          cols_changed <- c(cols_changed, paste0(col_name, " (-> Bool)"))
          converted <- TRUE
        }
      }

      if (converted) next

      #---convert string cols to numerical type---
      clean_vals <- gsub("[,%$]","", non_na_vals)
      num_vals <- suppressWarnings(as.numeric(clean_vals))

      ratio <- (sum(!is.na(num_vals))/length(non_na_vals))

      if(ratio >= num_threshold) {
        df[[col_name]] <- suppressWarnings(as.numeric(gsub("[,%$]","",df[[col_name]])))
        cols_changed <- c(cols_changed, paste0(col_name, " (-> Num)"))
        converted <- TRUE
      }

      if (converted) next

      #---convert string cols to date object---

      #try parsing
      try_dates <- suppressWarnings(date_parser(non_na_vals))

      date_ratio <- sum(!is.na(try_dates)) / length(non_na_vals)

      if(date_ratio >= date_threshold){
        df[[col_name]] <- suppressWarnings(date_parser(df[[col_name]]))
        cols_changed <- c(cols_changed, paste0(col_name, " (-> Date)"))
        converted <- TRUE
      }

      if (converted) next

      #---convert string cols to categorical object---
      uniq_ratio <- length(unique(non_na_vals)) / length(non_na_vals)

      if (uniq_ratio <= cat_threshold && length(unique(non_na_vals)) > 1) {
        df[[col_name]] <- as.factor(df[[col_name]])
        cols_changed <- c(cols_changed, paste0(col_name, " (-> Factor)"))
        converted <- TRUE
      }

    } # End is.character check
  } # End Loop

  # 3. Update Object
  x$data <- df

  # 4. Log results
  if (length(cols_changed) > 0) {
    msg <- sprintf("Standardize Str Col Types: Converted %d columns: [%s]",
                   length(cols_changed), paste(cols_changed, collapse = ", "))
    x$log <- c(x$log, msg)
  } else {
    x$log <- c(x$log, "Standardize Str Col Types: No columns met conversion thresholds.")
  }

  return(x)
}
