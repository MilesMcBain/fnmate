test_that("function write guards", {
  temp_r_path <- file.path(tempdir(), "fnmate_test")
  on.exit(unlink(temp_r_path, recursive = TRUE))

  withr::with_options(
    list(
      fnmate_folder = temp_r_path,
      fnmate_banned_names = c("tar_target", "banned_name")
    ),
    {
      # Write a function, then attempt to write it again
      write_fn_file(
        "test_fn",
        "test_fn <- function(arg1) {arg1}"
      )

      # get last modified date of file
      last_modified <- file.info(file.path(temp_r_path, "test_fn.R"))$mtime

      # Can't write existing file on first try
      expect_message(
        write_fn_file(
          "test_fn",
          "test_fn <- function(arg1) {arg1}"
        ),
        class = "target_file_already_exists"
      )
      # Same last mod date
      expect_equal(
        last_modified,
        file.info(file.path(temp_r_path, "test_fn.R"))$mtime
      )

      # expect written on second shot:
      write_fn_file(
        "test_fn",
        "test_fn <- function(arg1) {arg1}"
      )
      # written again more recently
      expect_true(
        file.info(file.path(temp_r_path, "test_fn.R"))$mtime > last_modified
      )

      # try to write a function in environment, in this case we use one from
      # testthat
      # First time we are blocked. Second time it is allowed.
      expect_message(
        write_fn_file(
          "expect_true",
          "expect_true <- function(arg1) {arg1}"
        ),
        class = "function_name_already_in_environment"
      )

      expect_false(
        file.exists(file.path(temp_r_path, "expect_true.R"))
      )

      write_fn_file(
        "expect_true",
        "expect_true <- function(arg1) {arg1}"
      )
      expect_true(
        file.exists(file.path(temp_r_path, "expect_true.R"))
      )

      # can't write a function in the banned list
      expect_message(
        write_fn_file(
          "banned_name",
          "banned_name <- function(arg1) {arg1}"
        ),
        class = "function_name_is_banned"
      )
      expect_false(
        file.exists(file.path(temp_r_path, "banned_name.R"))
      )

    }
  )


})
