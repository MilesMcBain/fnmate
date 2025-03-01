test_that("grep results for jump", {

  # a result string has form:
  # <path>:<line>:<column|match>
  # with column for rg, but match for git grep

  # 1 result
  results_1 <-
    "R/test_function.R:2:1"

  expect_equal(
    process_grep_results(results_1, "test_function", "R/home_function.R"),
    c("R/test_function.R", "2", "1")
  )

  # n results
  # should warn and prefer one on descendent path
  results_n <-
    c(
      "tests/testhat/test_function.R",
      "plans/_targets.R",
      "R/test_function.R"
    )

  expect_warning(
    jump_target <- process_grep_results(results_n, "test_function", "R/home_function.R"),
    class = "matched_multiple_definitions_for_jump"
  )

  expect_equal(
    fs::path_dir(jump_target),
    fs::path_dir("R/home_function.R")
  )

  # can adjust jump target with a glob for preferred paths
  withr::with_options(
    list(
      fnmate_preferred_jump_paths = "*/testhat/*.R"
    ),
    {
      expect_warning(
        jump_target <- process_grep_results(results_n, "test_function", "R/home_function.R"),
        class = "matched_multiple_definitions_for_jump"
      )
      expect_equal(
        fs::path_dir(jump_target),
        fs::path_dir("tests/testhat/test_function.R")
      )
    }
  )


})
