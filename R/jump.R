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
        return(list())
    }

    result_components <- strsplit(result, ":")[[1]]

    list(
        file = result_components[[1]],
        row = result_components[[2]],
        col = result_components[[3]]
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
        return(list())
    }

    result_components <- strsplit(result, ":")[[1]]

    list(
        file = result_components[[1]],
        row = result_components[[2]],
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