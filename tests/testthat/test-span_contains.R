test_that("span containment", {

  multi_line_span_a <- list(line1 = 2,
                          line2 = 4,
                          col1 = 10,
                          col2 = 40)

  multi_line_span_b<- list(line1 = 2,
                           line2 = 4,
                           col1 = 40,
                           col2 = 10)

  expect_true(
    span_contains(multi_line_span_a,
                  list(row = 3,
                       col = 1)))

  expect_true(
    span_contains(multi_line_span_a,
                  list(row = 3,
                       col = 50)))

  expect_true(
    span_contains(multi_line_span_a,
                  list(row = 3,
                       col = 10)))

  expect_true(
    span_contains(multi_line_span_b,
                  list(row = 3,
                       col = 50)))

  expect_false(
    span_contains(multi_line_span_a,
                  list(row = 4,
                       col = 41)))
  expect_false(
    span_contains(multi_line_span_a,
                  list(row = 2,
                       col = 1)))

  one_line_span <- list(line1 = 1,
                        line2 = 1,
                        col1 = 5,
                        col2 = 20)

  expect_true(
    span_contains(one_line_span,
                  list(row = 1,
                       col = 15)))

  expect_true(
    span_contains(one_line_span,
                  list(row = 1,
                       col = 20)))

  expect_true(
    span_contains(one_line_span,
                  list(row = 1,
                       col = 5)))

  expect_false(
    span_contains(one_line_span,
                  list(row = 1,
                       col = 21)))

  expect_false(
    span_contains(one_line_span,
                  list(row = 1,
                       col = 1)))

})
