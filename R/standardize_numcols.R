standardize_numcols <- function(x, ...) UseMethod("standardize_numcols")

standardize_numcols.MungrCleaner <- function(x, col_name = "", convert_bool = TRUE, convert_factor = TRUE, convert_int = TRUE, cat_threshold = 0.1, ...){
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
      if(convert_bool){
        u_vals <- unique(non_na_vals)

        if (all(u_vals %in% c(0,1))){
          df[[col_name]] <- as.logical(df[[col_name]])
          cols_changed <- c(cols_changed, paste0(col_name, " (-> Bool)"))
          converted <- TRUE
        }
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
