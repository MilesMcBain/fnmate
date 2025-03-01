window_around_cursor <- function(context) {
  cursor_pos <- context$selection[[1]]$range$start
  cursor_line <- cursor_pos[1]
  cursor_column <- cursor_pos[2]

  window_size <- getOption("fnmate_window") %||% 20
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
  function_text <- paste0("\n", fnmate_below(cursor_context$text, cursor_context$index))

  rstudioapi::insertText(
    location = rstudioapi::document_position(
      row = end_row + 1,
      col = 1
    ),
    text = function_text,
    id = context$id
  )
}

##' Generate function definition and paste it to the clipboard
##'
##' @title rs_fnmate_clip
##' @param context from the rstudioapi
##' @return nothing.
##' @author Miles McBain
##' @export
rs_fnmate_clip <- function(context = rstudioapi::getActiveDocumentContext()) {
  if (identical(context$id, "#console")) {
    message("fnmate does not work on console text.")
    return(invisible(NULL))
  }

  cursor_context <- window_around_cursor(context)

  fnmate_output <- fn_defn_from_cursor(cursor_context$text,
    cursor_context$index,
    external = TRUE
  )

  message(
    "fnmate wrote the definition for `",
    fnmate_output$fn_name,
    "()` to the clipboard."
  )

  clipr::write_clip(fnmate_output$fn_defn, object_type = "character")
}


##' Jump to a function definition in the local project
##'
##' Search for a local function definion matching the function the cursor is
##' on, and jump to it, if found.
##'
##' @param context the active document context from [rstudioapi]
##'
##' @author Miles McBain
##' @return nothing
#' @export
rs_fn_defn_jump <- function(context = rstudioapi::getActiveDocumentContext()) {
  cursor_context <- window_around_cursor(context)

  truncated_input <- truncate_to_chunk_boundary(
    cursor_context$text,
    cursor_context$index
  )
  text <- truncated_input$text
  index <- truncated_input$index
  assert_length_1(text)

  target <- locate_fn_target(text, index)

  expression <- as.list(rlang::parse_expr(target))

  fn_name <- expression[[1]]
  jump_fn_definiton(fn_name, jump_start_file = context$path)
}


