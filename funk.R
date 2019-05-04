locate_funkey_target <- function(text, index){

  function_open_pattern <- "[A-Za-z.][A-Za-z0-9_.]+\\s*\\("

  ## edge case: cursor on fun call closing paren
  ## move index just inside fun call
  if (substring(text,index,index) == ")") index <- index - 1

  matches <-
    gregexpr(function_open_pattern, text)[[1]] %>%
    purrr::keep(~. < index)

  bisection <- purrr::map(matches,
                          ~substring(text, .x, index))
  funkey_target <-
    which.max(!purrr::map_lgl(bisection,
                              root_is_complete_function))

}

parse_safely <- purrr::safely(parse)

root_is_complete_function <- function(fn_text){
  tstfile = srcfile(tempfile())
  parse_safely(text = fn_text,
               keep.source = TRUE,
               srcfile = tstfile)
  parse_data <- getParseData(tstfile)

  parse_data[grepl("SYMBOL", parse_data$token),]$token[[1]] == "SYMBOL_FUNCTION_CALL"
}
