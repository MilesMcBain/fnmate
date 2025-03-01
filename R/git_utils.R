get_git_user_name <- function() {
  tryCatch(
    with(
      gert::git_config(),
      value[name == "user.name"]
    ),
    error = function(e) {
      "<author>"
    }
  )
}
