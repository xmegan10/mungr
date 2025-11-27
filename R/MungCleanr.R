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


