context("Rmd chunks")

test_that("test that Rmd chunks are truncated", {

  chunk_text <-
"a <- some_function(1, 2, 3)\n\n\n```\n\n```{r}\nsome_function2(a, b, c)\n\n\nsome_function3(x, y, z)\n```\n\nsome_function3(3, 2, 1)\n\n"

  match_22 <- truncate_to_chunk_boundary(chunk_text, 22)

  expect_equal(match_22,
               list(
                 index = 22,
                 text = "a <- some_function(1, 2, 3)\n\n\n"
               )
               )

  match_66 <- truncate_to_chunk_boundary(chunk_text, 66)
  expect_equal(match_66,
               list(
                 index = 28,
                 text = "{r}\nsome_function2(a, b, c)\n\n\nsome_function3(x, y, z)\n"
               )
               )
  expect_equal(substring(chunk_text,
                         66,
                         69),
               substring(match_66$text,
                         match_66$index,
                         match_66$index + 3)
               )

  match_120 <- truncate_to_chunk_boundary(chunk_text, 120)
  expect_equal(match_120,
               list(
                 index = 25,
                 text = "\n\nsome_function3(3, 2, 1)\n\n"
                 )
               )
  expect_equal(substring(chunk_text,
                         120,
                         123),
               substring(match_120$text,
                         match_120$index,
                         match_120$index + 3)
               )

})
