test_that("index / row col conversion works", {
  text <- "lineA1.\n line 2 lineB2.\nCline 3 line 3 line 3.D"

  indexes <- list(
    index_A = as.vector(regexpr("A", text)),
    index_B = as.vector(regexpr("B", text)),
    index_C = as.vector(regexpr("C", text)),
    index_D = as.vector(regexpr("D", text))
  )

  rows <- list(
    row_A = 1,
    row_B = 2,
    row_C = 3,
    row_D = 3
    )

  cols <- list(
    col_A = indexes$index_A,
    col_B = as.vector(regexpr("B", strsplit(text,"\n")[[1]][2])),
    col_C = as.vector(regexpr("C", strsplit(text,"\n")[[1]][3])),
    col_D = as.vector(regexpr("D", strsplit(text,"\n")[[1]][3]))
  )

  purrr::pwalk(.l = list (rows, cols, indexes),
               .f = ~ expect_equal(row_col_to_index(text, ..1, ..2),
                                   ..3))

  purrr::pwalk(.l = list (rows, cols, indexes),
               .f = ~ expect_equal(index_to_row_col(text, ..3),
                                   list(row = ..1, col = ..2)))

})
