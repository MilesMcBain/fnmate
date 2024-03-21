get_git_user_name <- function() {
  tryCatch(
    with(gert::git_config(),
      value[name == "user.name"]
    ),
    error = function(cond){
      message("No Git repo found. Roxygen @author will be left blank.")
      ""
    }
  )
}