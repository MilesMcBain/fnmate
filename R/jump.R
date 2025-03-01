jump_fn_definiton <- function(fn_name, jump_start_file) {
  fn_match <- get_search_fn()(fn_name, jump_start_file)

  if (length(fn_match) == 0) {
    return(NULL)
  }

  rstudioapi::navigateToFile(
    fn_match$file,
    fn_match$row,
    fn_match$col
  )
}

get_search_fn <- function() {
  search_tool <- getOption("fnmate_searcher") %||% "rg"

  switch(
    search_tool,
    "rg" = ripgrep,
    "git_grep" = git_grep,
    function(fn_name) stop("unsupported definition search tool:", search_tool)
  )
}

ripgrep <- function(fn_name, jump_start_file) {
  search_regex <- fnmate_quote_regex(
    glue::glue("\\b{fn_name}\\s*(?:<-|=)\\s*")
  )
  other_args <- c("-m1", "--vimgrep")
  result <- system2("rg", args = c(other_args, search_regex), stdout = TRUE)

  if (length(result) < 1) {
    return(list())
  }

  result_components <- process_grep_results(result, fn_name, jump_start_file)

  list(
    file = result_components[[1]],
    row = result_components[[2]],
    col = result_components[[3]]
  )
}

git_grep <- function(fn_name, jump_start_file) {
  search_regex <- fnmate_quote_regex(
    glue::glue("\\b{fn_name}\\s*(<-|=)\\s*")
  )
  search_pathspec <- fnmate_quote_regex("*.R")
  other_args <- c("grep", "-nE", "--untracked", search_regex, "--", search_pathspec)
  result <- system2("git", args = other_args, stdout = TRUE)

  if (length(result) < 1) {
    return(list())
  }

  result_components <- process_grep_results(result, fn_name, jump_start_file)

  list(
    file = result_components[[1]],
    row = result_components[[2]],
    col = 1 # git grep doesn't return the actual match column
  )
}

# Powershell seems not to like quoting the regex, while
# As seen in issue #13 MacOS  seems to need it.
# Linux seems not to care.
fnmate_quote_regex <- function(regex) {
  if (getOption("fnmate_quote_jump_regex") %||% FALSE) {
    glue::glue("'{regex}'")
  } else {
    regex
  }
}

process_grep_results <- function(grep_results, fn_name, jump_start_file) {
  if (length(grep_results) == 0) {
    rlang::abort(
      glue::glue("Could locate definition for {fn_name}."),
      class = "could_not_find_jump_target"
    )
  }

  results_pieces <- strsplit(grep_results, ":")
  results_paths <- vapply(results_pieces, \(x) x[[1]], character(1))

  destination_result <-
    if (length(results_paths) > 1) {
      # if we got multiple matching paths warn:
      rlang::warn(
        glue::glue(
          "Matched multiple definitions for {fn_name} in paths: {paste0(results_paths, collapse = ', ')}"
        ),
        class = "matched_multiple_definitions_for_jump"
      )
      # prioritise a result that is a descendent of jump_start_file path.
      jump_start_folder <-
        fs::path_dir(jump_start_file)

      preferred_results <-
        fs::path_filter(
            results_paths,
            glob = getOption("fnmate_preferred_jump_paths", "")
        )
        # By default there will be none of these.

      descendant_results <-
        fs::path_has_parent(
          results_paths,
          jump_start_folder
        )

      # by pushing up the descendent results to head of list
      jump_target <- c(
        preferred_results,
        results_pieces[descendant_results],
        results_pieces[!descendant_results]
      ) |>
        _[[1]]

      rlang::inform(
        glue::glue("Jumping to {jump_target[[1]]}")
      )

      jump_target

    } else {
      results_pieces[[1]]
    }

  destination_result
}
