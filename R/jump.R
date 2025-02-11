jump_fn_definiton <- function(fn_name) {
  fn_match <- get_search_fn()(fn_name)

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

  switch(search_tool,
    "rg" = ripgrep,
    "git_grep" = git_grep,
    function(fn_name) stop("unsupported definition search tool:", search_tool)
  )
}

ripgrep <- function(fn_name) {
  search_regex <- fnmate_quote_regex(
    glue::glue("\\b{fn_name}\\s*(?:<-|=)\\s*")
  )
  other_args <- c("-m1", "--vimgrep")
  result <- system2("rg", args = c(other_args, search_regex), stdout = TRUE)

  if (length(result) < 1) {
    message(cli::format_message(c(
      " " = "",
      "x" = "No function called {.fn {fn_name}} was found."
    )))
    return(list())
  }

  if (length(result) == 1) {
    result_components <- strsplit(result, ":")[[1]]

    message(cli::format_message(c(
      " " = "",
      "v" = "Found {.fn {fn_name}} in {.pkg {result_components[[1]]}}",
      "v" = "Opening {cli::col_green(result_components[[1]])}"
    )))

    return(list(
      file = result_components[[1]],
      row = result_components[[2]],
      col = result_components[[3]]
    ))
  }

  # prioritise the first function in the R folder.
  if (length(grep("^R/", result, value = TRUE)[[1]]) > 0) {
    selected_result <- grep("^R/", result, value = TRUE)[[1]]
  } else {
    selected_result <- result[[1]]
  }

  all_files <- sapply(strsplit(result, ":"), "[[", 1)
  selected_components <- strsplit(selected_result, ":")[[1]]

  warning(cli::format_warning(c(
    " " = "",
    "!" =  "There were {length(result)} files with the function {.fn {fn_name}} identified to return",
    "!" = "The files identified are {.pkg {all_files}}",
    "v" = "Opening {cli::col_green(selected_components[[1]])}"
  )))

  list(
    file = selected_components[[1]],
    row = selected_components[[2]],
    col = selected_components[[3]]
  )
}

git_grep <- function(fn_name) {
  search_regex <- fnmate_quote_regex(
    glue::glue("\\b{fn_name}\\s*(<-|=)\\s*")
  )
  search_pathspec <- fnmate_quote_regex("*.R")
  other_args <- c("grep", "-nE", "--untracked", search_regex, "--", search_pathspec)
  result <- system2("git", args = other_args, stdout = TRUE)

  if (length(result) < 1) {
    message(cli::format_message(c(
      " " = "",
      "x" = "No function called {.fn {fn_name}} was found."
    )))
    return(list())
  }

  if (length(result) == 1) {
    result_components <- strsplit(result, ":")[[1]]

    message(cli::format_message(c(
      " " = "",
      "v" = "Found {.fn {fn_name}} in {.pkg {result_components[[1]]}}",
      "v" = "Opening {cli::col_green(result_components[[1]])}"
    )))

    return(list(
      file = result_components[[1]],
      row = result_components[[2]],
      col = 1
    ))
  }
  # prioritise the first function in the R folder.
  if (length(grep("^R/", result, value = TRUE)[[1]]) > 0) {
    selected_result <- grep("^R/", result, value = TRUE)[[1]]
  } else {
    selected_result <- result[[1]]
  }
  all_files <- sapply(strsplit(result, ":"), "[[", 1)
  selected_components <- strsplit(selected_result, ":")[[1]]

  warning(cli::format_warning(c(
    " " = "",
    "!" =  "There were {length(result)} files with the function {.code {fn_name}} identified to return",
    "!" = "The files identified are {.pkg {all_files}}",
    "v" = "Opening {cli::col_green(selected_components[[1]])}"
  )))

  list(
    file = selected_components[[1]],
    row = selected_components[[2]],
    col = 1
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
