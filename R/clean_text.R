#empty fields replaced by NA, optional choice to upper or lower case all strings, choice to remove trailing white spaces
clean_text <- function(x, ...) UseMethod("clean_text")

clean_text.MungrCleaner <- function(x, case = NULL, trim = TRUE, ...) {

  df <- x$data

  # Trim Whitespace & Fix Empty Strings
  if (trim) {
    df <- df |>
      dplyr::mutate(dplyr::across(where(is.character), stringr::str_trim)) |>
      dplyr::mutate(dplyr::across(where(is.character), ~ dplyr::na_if(., "")))
  }

  # Case Conversion (Optional)
  if (!is.null(case)) {
    # Select function based on string input
    case_func <- switch(tolower(case),
                        "lower" = tolower,
                        "upper" = toupper,
                        "title" = stringr::str_to_title,
                        NULL)

    if (!is.null(case_func)) {
      df <- df |>
        dplyr::mutate(dplyr::across(where(is.character), case_func))
    }
  }

  x$data <- df

  # 3. Log
  action_msg <- "Text cleaned (Empty -> NA)"

  if (trim) action_msg <- paste0(action_msg, ", Trimmed")
  if (!is.null(case)) action_msg <- paste0(action_msg, ", Case: ", case)
  x$log <- c(x$log, paste0(action_msg, ")"))

  return(x)
}
