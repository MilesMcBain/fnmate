globalVariables(".fnmate_env")
.fnmate_env <- new.env()

##' Create a definition in file for the function used at index.
##'
##' Given some text and an index into the text, this function creates a new file
##' containing a definition for the function used at the index. The file name is
##' the same as the function. An error is thrown if no function is found.
##'
##' By default the function is created in the './R' folder. This can be changed
##' with the `fnmate_folder` option.
##'
##' By default the function body contains only NULL. This can be changed with
##' the `fnmate_placeholder` option.
##'
##' This function is not intended to be used directly but by the fnmate front
##' end - either Emacs or RStudio.
##'
##' @title fnmate_fn.R
##' @param text some text from a source file
##' @param index an index into text indicating the cursor position.
##' @return Nothing, file created as side effect.
##' @author Miles McBain
##' @export
fnmate_fn.R <- function(text, index) {

  truncated_input <- truncate_to_chunk_boundary(text, index)
  text <- truncated_input$text
  index <- truncated_input$index
  assert_length_1(text)
  fnmate_target <- fn_defn_from_cursor(text, index, external = TRUE)
  write_fn_file(fnmate_target$fn_name,
                fnmate_target$fn_defn)

}

##' Create a definition below for the function used at index.
##'
##' Given some text and an index into the text, this function generates text
##' containing a definition for the function used at the index. An error is
##' thrown if no function is found.
##'
##' By default the function is created in the './R' folder. This can be changed
##' with the `fnmate_folder` option.
##'
##' By default the function body contains only NULL. This can be changed with
##' the `fnmate_placeholder` option.
##'
##' This function is not intended to be used directly but by the fnmate front
##' end - either Emacs or RStudio.
##'
##' @title fnmate_below
##' @param text some text from a source file
##' @param index an index into text indicating the cursor position.
##' @return text containing function definition.
##' @author Miles McBain
##' @export
fnmate_below <- function(text, index) {

  truncated_input <- truncate_to_chunk_boundary(text, index)
  text <- truncated_input$text
  index <- truncated_input$index
  assert_length_1(text)
  fn_defn_from_cursor(text, index, external = FALSE)$fn_defn

}

fn_defn_from_cursor <- function(text, index, external = TRUE) {
  target <- locate_fn_target(text, index)

  expression <- as.list(rlang::parse_expr(target))

  fn_name <- expression[[1]]
  fn_args <- expression[-1]
  fn_arg_names <- names(expression[-1]) %||% as.list(rep("", length(fn_args)))
  fn_arg_classes <- purrr::map_chr(fn_args, class)

  ## args that are just values or calls need to have names generated.
  unnamed_args <- fn_arg_classes != "name" & fn_arg_names == ""
  name_is_arg <- fn_arg_classes == "name" & fn_arg_names == ""
  fn_arg_names[unnamed_args] <- paste0("nameme", seq(sum(unnamed_args)))
  fn_arg_names[name_is_arg] <- as.character(fn_args[name_is_arg])
  fn_arg_list <-
    purrr::pmap_chr(list(name = fn_arg_names,
                         arg = fn_args,
                         name_is_arg = name_is_arg),
                    function(name, arg, name_is_arg) {

                      if (!name_is_arg) paste0(name, " = ", deparse_one_string(arg))
                      else name

                    })

  body <- build_fn_body(fn_name, fn_arg_list)
  if (external) {
    roxygen <- build_external_roxygen(fn_arg_names)
  } else {
    roxygen <- build_internal_roxygen()
  }

  fn_text <- paste0(c(roxygen, body), collapse = "\n")

  list(fn_name = fn_name,
       fn_defn = fn_text)
}

write_fn_file <- function(fn_name, fn_defn, fn_folder = getOption("fnmate_folder") %||% "R") {

  if (!dir.exists(fn_folder)) dir.create(fn_folder, recursive = TRUE)

  target_file <- file.path(fn_folder,paste0(fn_name,".R"))

  ## If file already exists, bail without writing, but set it up so that if user
  ## calls again existing file will be overwritten.
  if (file.exists(target_file) &&
      (.fnmate_env$previous_call %||% "") != fn_name) {
    .fnmate_env$previous_call = fn_name
    message(target_file, " already exists. Call fnmate again on this function to overwrite file.")
    return(invisible(NULL))
  }

  readr::write_file(x = fn_defn, file = target_file)
  message("fnmate Wrote ", target_file)
  .fnmate_env$previous_call <- NULL

  invisible(fn_defn)
}

build_fn_body <- function(fn_name, fn_arg_list) {

  definition <- glue::glue("{fn_name} <- function(")

  args <- paste0(fn_arg_list, collapse = ", ")

  ## wrap in case too wide
  args <- strwrap(args, width = 80 - nchar(definition))

  ## indent any wrapped lines
  if (length(args > 1)) {
    to_indent <- seq_along(args)[-1]
    args[to_indent] <- paste0(strrep(" ", nchar(definition)),
                              args[to_indent])
    args <- paste0(args, collapse = "\n")
  }

  content <- getOption("fnmate_placeholder") %||% "NULL"

  glue::glue(definition, args, ") {{\n\n  {content}\n\n}}\n",
             .trim = FALSE)
}


build_internal_roxygen <- function() {

  NULL

}

build_external_roxygen <- function(fn_arg_names) {

  head <- glue::glue(
                  "##' .. content for \\description{{}} (no empty lines) ..\n",
                  "##'\n",
                  "##' .. content for \\details{{}} ..\n",
                  "##'\n",
                  "##' @title")

  params <-
    purrr::map_chr(fn_arg_names, ~glue::glue("##' @param {.x}")) %>%
    paste0(collapse="\n")

  tail <-
    glue::glue(
            "##' @return\n",
            "##' @author {system2(\"git\", args = c(\"config\", \"user.name\"), stdout = TRUE)}\n",
            "##' @export")

  paste0(c(head, params, tail), collapse = "\n")

}

locate_fn_target <- function(text, index) {

  function_open_pattern <- "[A-Za-z.][A-Za-z0-9_.]+\\s*\\("

  matches <-
    gregexpr(function_open_pattern, text)[[1]] %>%
    purrr::keep(~. <= index)

  if (identical(matches, -1)) stop("fnmate couldn't find a parsable function at cursor.")

  match_row_col <-
    purrr::map(matches, ~index_to_row_col(text, .x))

  fn_candidate_spans <-
    purrr::map(matches,
               ~parse_from_idx(text, .x)) %>%
    purrr::map(first_fn_expr)

  candidates <-
   !purrr::map_lgl(fn_candidate_spans, is.null)

  candidate_matches <-
    matches[candidates]

  match_row_col <-
    match_row_col[candidates]

  index_row_col <- index_to_row_col(text, index)

  fn_candidate_spans <-
    fn_candidate_spans %>%
    purrr::discard(is.null)

  fn_candidate_text_coords <-
    purrr::map2(fn_candidate_spans,
                match_row_col,
         function(candidate_span, candidate_index){
           candidate_span$line1 <-
            candidate_index$row
           candidate_span$col1 <-
             candidate_index$col
           candidate_span$line2 <-
             candidate_span$line2 + candidate_index$row - 1
           candidate_span$col2 <-
             ifelse(candidate_span$line1 == candidate_span$line2,
                    candidate_span$col2 + candidate_index$col -1,
                    candidate_span$col2)
           candidate_span
         })

  fn_target_location <-
    fn_candidate_text_coords %>%
    purrr::keep(~span_contains(.x, index_row_col)) %>%
    utils::tail(1)

  if (length(fn_target_location) == 0) stop("fnmate couldn't find a parsable function at cursor.")

  fn_target_location <- fn_target_location[[1]]

  substring(text,
            row_col_to_index(text,
                             fn_target_location$line1,
                             fn_target_location$col1),
            row_col_to_index(text,
                             fn_target_location$line2,
                             fn_target_location$col2))
}

parse_safely <- purrr::safely(parse)

parse_from_idx <- function(text, index) {
  target_text <- substring(text, index)
  tstfile = srcfile(tempfile())
  parse_safely(text = target_text,
               keep.source = TRUE,
               srcfile = tstfile)
  utils::getParseData(tstfile)
}

first_fn_expr <- function(parse_data) {

  if (!root_is_complete_function(parse_data)) return(NULL)
  first_function_parent_expression <- first_function_parent(parse_data)
  first_function_parent_expression

}

root_is_complete_function <- function(parse_data) {
  parse_data_not_expr <- parse_data[parse_data$token != "expr", ]
  parse_data_not_expr$token[[1]] == "SYMBOL_FUNCTION_CALL"
}

first_function_parent <- function(parse_data) {
  symbol_parent_id <-
    parse_data[parse_data$token == "SYMBOL_FUNCTION_CALL",]$parent[[1]]

  expression_parent_id <- 
    parse_data[parse_data$id == symbol_parent_id, ]$parent[[1]]

  ## The expr we want is actually the grandparent. The parent is an expr for
  ## the function name symbol. The grandparent is the full multi-line/arg
  ## expression.
  parent_id <-
    parse_data[parse_data$id == expression_parent_id, ]
}

index_to_row_col <- function(text, index) {

  line_ends<- gregexpr("\\n", text)[[1]]
  line_num <- sum((line_ends < index)) + 1
  ## + 1 since first line doesn't have a \n before it.

  col_num <- suppressWarnings(
    index - max(max(line_ends[line_ends < index]), 0)
  )  ## if there are no line ends inner max returns -inf

  list(row = line_num, col = col_num)

}

row_col_to_index <- function(text, row, col) {
  line_end_locs <- gregexpr("\\n", text)[[1]]
  ifelse(row == 1,
         col,
         line_end_locs[row - 1] + col)
}

span_contains <- function(span, index) {
  within_line_span <-
    span$line1 <= index$row &&
    span$line2 >= index$row

  single_line_span <- span$line1 == span$line2
  on_first_line <- index$row == span$line1
  on_last_line <- index$row == span$line2

  if(single_line_span) within_col_span <- index$col >= span$col1 && index$col <= span$col2  
  else if (on_first_line) within_col_span <- index$col >= span$col1
  else if (on_last_line) within_col_span <- index$col <= span$col2
  else within_col_span <- within_line_span

  within_line_span && within_col_span
}

`%||%` <- function(x, y) if (is.null(x)) y else x

## annoyingly one can't stop deparse from auto wrapping expressions, one can only set the limit very high.
## In case an expression is wrapped, put it back on one line.
deparse_one_string <- function(code) {
  char_vec <- deparse(code, width.cutoff = 500)
  char_vec_clean <- gsub("^\\s+", "\\s", char_vec)
  paste0(char_vec_clean, collapse = "")
} 

truncate_to_chunk_boundary <- function(text, index) {

  tripple_ticks <- gregexpr("```", text, perl = TRUE)[[1]]

  upper_fence <- tripple_ticks[head(which(tripple_ticks > index), 1)]
  lower_fence <- tripple_ticks[tail(which(tripple_ticks < index), 1)]

  if (length(upper_fence) == 0) {
    upper_fence <- nchar(text)
  } else {
    upper_fence <- upper_fence - 1
  }

  if (length(lower_fence) == 0) {
    lower_fence <-  1
  } else {
    lower_fence <- lower_fence + 3
  }

  list(
    index = index - (lower_fence - 1),
    text = substring(text,
                     first = lower_fence,
                     last = upper_fence)
  )

}

assert_length_1 <- function(text) {

  if(length(text) !=  1)
    stop("text is expected to be a length 1 character vector. Its length was: ", length(text))

}
