window_around_cursor <- function(context) {

  cursor_pos <- context$selection[[1]]$range$start
  cursor_line <- cursor_pos[1]
  cursor_column <- cursor_pos[2]

  window_size <- getOption('fnmate_window') %||% 20
  start_line <- max(cursor_line - window_size, 1)
  end_line <- min(cursor_line + window_size, length(context$contents))
  text_window <-
    context$contents[seq(start_line, end_line)] %>%
    paste0(collapse = "\n")

  cursor_line <- cursor_line - start_line + 1
  index <- row_col_to_index(text_window, cursor_line, cursor_column)

  list(text = text_window, index = index)

}

#' Generate a function definition file from a function call in RStudio
#'
#' @title rs_fnmate
#' @param context from the rstudioapi
#'
#' @return nothing.
#' @export
rs_fnmate <- function(context = rstudioapi::getActiveDocumentContext()) {

  if (identical(context$id, "#console")) {
    message("fnmate does not work on console text.")
    return(invisible(NULL))
  }

  cursor_context <- window_around_cursor(context)

  fnmate_fn.R(cursor_context$text, cursor_context$index)
}

#' Generate a function definition below from a function call in RStudio
#'
#' @title rs_fnmate_below
#' @param context from the rstudioapi
#'
#' @return nothing.
#' @export
rs_fnmate_below <- function(context = rstudioapi::getActiveDocumentContext()) {

  if (identical(context$id, "#console")) {
    message("fnmate does not work on console text.")
    return(invisible(NULL))
  }

  end_row <- length(context$contents)

  cursor_context <- window_around_cursor(context)
  function_text <- fnmate_below(cursor_context$text, cursor_context$index)

  rstudioapi::insertText(location = rstudioapi::document_position(row = end_row +1,
                                                                  col = 1),
                         text = function_text,
                         id = context$id)

}
