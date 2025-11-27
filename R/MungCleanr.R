MungrCleaner <- function(df){
  if(!is.data.frame(df)) stop("Input must be a dataframe.")

  obj <- list(
    data = as_tibble(df),
    log = character()
  )
  class(obj) <- "MungrCleaner"
  return(obj)
}

print.MungrCleaner <- function(x,...){
  cat("MungrCleaner History\n")
  if(length(x$log) > 0) {
    cat("Status: Cleaned", length(x$log), "steps.\n")
  }else{
    cat("Status: No changes yet\n")
  }
  invisible(x)
}


