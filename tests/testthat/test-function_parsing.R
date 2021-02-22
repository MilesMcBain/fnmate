test_that("function_parsing", {

  complete_fn_parse_data <- parse_from_idx("my.func2(foo = list(1,2,3), 4, target3)", 1)

  incomplete_fn_parse_data <- parse_from_idx("drake_plan(a = some_func(param1, param2, param3 = 3, 4), target2 = my.func2(foo = list(1,2,3), 4, target3),", 68)

  no_fn_parse_data <- parse_from_idx("a <- b; b <- c", 6)

  expect_true(!is.null(first_fn_expr(complete_fn_parse_data)))

  expect_true(!is.null(first_fn_expr(incomplete_fn_parse_data)))

  expect_null(first_fn_expr(no_fn_parse_data))

})

test_that("target_function_location", {

test_text <- 
"drake_plan(

  a = some_func(param1, param2, param3 = 3, 4),

  target2 = my.func2(foo = list(1,2,3), 4, target3),

  target3 = do_the_thing(arg1,
                         arg2,
                         func3(arg4),
                         arg3,
                         arg4,
                         really_bloody_long_arg,
                         really_bloody_longer_arg),

  texty_one = fn3(\"text\", \"\\\"text in text\\\"\"),

  targ = a_thing(arg1, arg2),











  )"

## arg4
expect_equal(locate_fn_target(test_text, 273),
             "do_the_thing(arg1,\n                         arg2,\n                         func3(arg4),\n                         arg3,\n                         arg4,\n                         really_bloody_long_arg,\n                         really_bloody_longer_arg)"
             )

## blank
expect_equal(locate_fn_target(test_text, 255),
             "do_the_thing(arg1,\n                         arg2,\n                         func3(arg4),\n                         arg3,\n                         arg4,\n                         really_bloody_long_arg,\n                         really_bloody_longer_arg)"
             )

## text in text
expect_equal(locate_fn_target(test_text, 411),
             "fn3(\"text\", \"\\\"text in text\\\"\")")

## 4
expect_equal(locate_fn_target(test_text, 103),
             "my.func2(foo = list(1,2,3), 4, target3)")

neg_test_text <- "foo <- 1985
bar <- foo^2 %>%
  .*0.01 %>%
  some_function(arg1,
                arg2,
                arg3,"
## foo
expect_error(locate_fn_target(neg_test_text, 20),
             "fnmate couldn't find")

## some_function
expect_error(locate_fn_target(neg_test_text, 50),
             "fnmate couldn't find")

## arg2
expect_error(locate_fn_target(neg_test_text, 82),
             "fnmate couldn't find")

## function parsing in pipes

test_chain <-
  "rdeck(
    initial_bounds = st_bbox(impacted_area_shape),
    picking_radius = 2
  ) %>%
    add_area_of_interest_layer(impacted_area_shape) %>%
    add_isochrone_layer(data_isochrones) %>%
    # add_brigades_layer(brigades) %>%
    add_stations_layer(slocation)"

expect_equal(
  locate_fn_target(test_chain, 15),
  "rdeck(
    initial_bounds = st_bbox(impacted_area_shape),
    picking_radius = 2
  )"
)

expect_equal(
  locate_fn_target(test_chain, 160),
  "add_isochrone_layer(data_isochrones)"
)

expect_equal(
  locate_fn_target(test_chain, 250),
  "add_stations_layer(slocation)"
)
})