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

#reassigns column types based on simple patterns in each column.
standardize_strcols <- function(x, ...) UseMethod("standardize_strcols")

standardize_strcols.MungrCleaner <- function(x, col_name = "", convert_to_numtype = TRUE, convert_to_cattype = TRUE, num_threshold = 0.7, date_threshold = 0.5, cat_threshold = 0.1, date_format = "ymd",...){
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
      #----
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
      #----
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
      #----

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
      #----
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
