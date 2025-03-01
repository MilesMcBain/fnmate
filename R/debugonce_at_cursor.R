# borrowed from https://github.com/ropensci/targets/blob/546debc2fd372c7fd961aa492bd20a37f9d3fd31/R/rstudio_addin_tar_load.R#L10

#' @title RStudio addin to call [debugonce()] on the symbol at the cursor.
#' @description For internal use only. Not a user-side function.
#' @export
#' @keywords internal
#' @param context RStudio API context from
#'   `rstudioapi::getActiveDocumentContext()`.
rstudio_addin_debugonce <- function(context = NULL) {
  context <- context %||% rstudioapi::getActiveDocumentContext()
  target <- rstudio_symbol_at_cursor(context)
  if (!is.null(target)) {
    cat("Running debugonce on", target)
    env <- list(target = as.symbol(target))
    eval(substitute(debugonce(target)))
  }
}

if_any <- function(condition, x, y) {
  if (any(condition)) {
    x
  } else {
    y
  }
}

# borrowed from https://github.com/ropensci/targets/blob/546debc2fd372c7fd961aa492bd20a37f9d3fd31/R/utils_rstudio.R#L4
rstudio_symbol_at_cursor <- function(context) {
  if (identical(context$id, "#console")) {
    return(NULL)
  }
  cursor_pos <- context$selection[[1L]]$range$start
  cursor_line <- cursor_pos[1L]
  cursor_column <- cursor_pos[2L]
  r_symbol_pattern <- "[.A-Za-z][.A-Za-z0-9_]+"
  line_symbols <- gregexpr(
    text = context$contents[cursor_line],
    pattern = r_symbol_pattern
  )
  match_starts <- line_symbols[[1L]]
  match_ends <- match_starts + attr(x = line_symbols[[1]], "match.length") - 1L
  match_index <- which(
    cursor_column >= match_starts & cursor_column <= match_ends
  )
  if_any(
    identical(length(match_index), 0L),
    cat("Could not find object name at cursor position."),
    substr(
      context$contents[cursor_line],
      start = match_starts[match_index],
      stop = match_ends[match_index]
    )
  )
}
