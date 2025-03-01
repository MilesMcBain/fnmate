test_that("text around cursor for addin works", {

  context_mock <-
    structure(list(
      id = "AA6B186",
      path = "",
      contents = c(
        "drake_plan(",
        "  ",
        "  a = some_func(param1, param2, param3 = 3, 4),",
        "  ",
        "  target2 = my.func2(foo = list(1,2,3), 4, target3),",
        "  ",
        "  target3 = do_the_thing(arg1,",
        "                         arg2,",
        "                         func3(arg4),",
        "                         arg3,",
        "                         arg4,",
        "                         really_bloody_long_arg,",
        "                         really_bloody_longer_arg),",
        "  ",
        "  texty_one = fn3(\"text\", \"\\\"text in text\\\"\"),",
        "  ",
        "  targ = a_thing(arg1, arg2),",
        "  ",
        "  ",
        "  ",
        "  ",
        "  ",
        "  ",
        "  ",
        "  ",
        "  ",
        "  ",
        "  ",
        ")",
        ""
      ),
      selection = structure(list(list(
        range = structure(list(
          start = structure(c(row = 9,
                              column = 28), class = "document_position"),
          end = structure(c(row = 9,
                            column = 28), class = "document_position")
        ), class = "document_range"),
        text = ""
      )), .Names = "", class = "document_selection")
    ),
    class = "document_context")

  ## window around func3(arg4):
  options(fnmate_window = 1)
  expect_equal(window_around_cursor(context_mock)$text,
               "                         arg2,\n                         func3(arg4),\n                         arg3,")

  options(fnmate_window = NULL)

  context_mock2 <-
    structure(list(
      id = "AA6B186",
      path = "",
      contents = c(
        "c(",
        "foo",
        ")",
        ""
      ),
      selection = structure(list(list(
        range = structure(list(
          start = structure(c(row = 2,
                              column = 1), class = "document_position"),
          end = structure(c(row = 2,
                            column = 1), class = "document_position")
        ), class = "document_range"),
        text = ""
      )), .Names = "", class = "document_selection")
    ),
    class = "document_context")

  expect_equal(window_around_cursor(context_mock2)$text,
               "c(\nfoo\n)\n")
  cursor_context <- window_around_cursor(context_mock)

  # definition for func3
  # with roxygen by default
  function_def <- fn_defn_from_cursor(cursor_context$text, cursor_context$index)
  expect_true(
    grepl("func3 <- function\\(arg4\\)", function_def$fn_defn)
  )
  expect_true(
    grepl("#' @param", function_def$fn_defn)
  )

  # definition for func3 without roxygen:
    withr::with_options(
      list(
        fnmate_generate_roxygen = FALSE
      ),
      {
        function_def <- fn_defn_from_cursor(cursor_context$text, cursor_context$index)
        expect_false(
          grepl("#' @param", function_def$fn_defn)
        )
      }
    )

})
