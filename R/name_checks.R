is_loaded_function <- function(fn_name) {
  matching_function_loaded <-
    mget(as.character(fn_name), envir = .GlobalEnv, inherits = TRUE, ifnotfound = list(NULL)) |>
    unlist() |>
    not_null()

  matching_function_loaded
}

is_banned_name <- function(fn_name) {
  as.character(fn_name) %in% getOption("fnmate_banned_names", character(0))
}

not_null <- function(x) {
  !is.null(x)
}
