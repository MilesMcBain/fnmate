locate_funkey_target <- function(text, index){

  function_open_pattern <- "[A-Za-z.][A-Za-z0-9_.]+\\s*\\("

  matches <-
    gregexpr(function_open_pattern, text)[[1]] %>%
    purrr::keep(~. < index)

  match_row_col <-
    purrr::map(matches, ~index_to_row_col(text, .x))
  
  funkey_candidate_spans <-
    purrr::map(matches,
               ~parse_from_idx(text, .x)) %>%
    purrr::map(first_fn_expr)

  candidates <-
   !purrr::map_lgl(funkey_candidate_spans, is.null)

  candidate_matches <-
    matches[candidates]

  match_row_col <-
    match_row_col[candidates]

  funkey_candidate_spans <-
    funkey_candidate_spans %>%
    purrr::discard(is.null) %>%
    purrr::map2(match_row_col,
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
                    candidate_index$col)

           candidate_span
         })


}

parse_safely <- purrr::safely(parse)

parse_from_idx <- function(text, index){
  target_text <- substring(text, index)
  tstfile = srcfile(tempfile())
  parse_safely(text = target_text,
               keep.source = TRUE,
               srcfile = tstfile)
  getParseData(tstfile)
}

first_fn_expr <- function(parse_data){

  if (!root_is_complete_function(parse_data)) return(NULL)
  first_function_parent_id <- parse_data[1,]

}

root_is_complete_function <- function(parse_data){
  parse_data[grepl("SYMBOL", parse_data$token),]$token[[1]] == "SYMBOL_FUNCTION_CALL"
}

index_to_row_col <- function(text, index){

  line_ends<- gregexpr("\\n", text)[[1]]
  line_num <- sum((line_ends < index)) + 1
  ## + 1 since first line doesn't have a \n before it.

  col_num <- index - max(max(line_ends[line_ends < index]), 0)
  ## if there are no line ends inner max returns -inf

  list(row = line_num, col = col_num)

}
