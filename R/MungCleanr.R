#' @title MungrCleaner S3 Class
#'
#' @description The constructor for the S3 class. It initializes (1) a data cleaning pipeline object along with the original dataframe
#'  and (2) a history log attribute.
#'   The input data is converted to a data frame for consistent handling
#'   with tidyverse functions.
#'
#' @param df A data frame or object that can be coerced to a data frame intended for cleaning.
#' @return An object of the MungrCleaner class, which includes two attributes: data and log.
#' These attributes can be accessed using the object name and $.
#' }
#' @examples
#' # Create a simple dataframe
#' data <- data.frame(ID = 1:3, Value = c(10, 20, 30))
#'
#' # Initialize the MungrCleaner object
#' cleaner <- MungrCleaner(data)
#'
#' # Check the class
#' class(cleaner)
#'
#' @export
#' @importFrom tibble as_tibble
#'


MungrCleaner <- function(df){
  if(!is.data.frame(df)) stop("Input must be a dataframe.")

  obj <- list(
    data = as_tibble(df),
    log = character()
  )
  class(obj) <- "MungrCleaner"
  return(obj)
}

#' @title Print Method for MungrCleaner Objects
#'
#' @description Prints the cleaning history log of the MungrCleaner object.
#'
#' @param x An object of MungrCleaner.
#' @param ... Further arguments passed to or from other methods.
#' @return Invisibly returns the input object. Prints history log in console.
#' @export
#'
print.MungrCleaner <- function(x,...){
  cat("MungrCleaner History\n")
  if(length(x$log) > 0) {
    cat("Status: Cleaned", length(x$log), "steps.\n")
  }else{
    cat("Status: No changes yet\n")
  }
  invisible(x)
}


