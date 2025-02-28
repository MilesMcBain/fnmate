get_git_user_name <- function() {
  with(
    gert::git_config(),
    value[name == "user.name"]
  )
}
