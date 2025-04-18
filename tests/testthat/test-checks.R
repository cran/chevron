# check_all_colnames ----

test_that("check_all_colnames works as expected", {
  res <- expect_silent(check_all_colnames(mtcars, c("x", "y")))
  expect_identical(res, " Expected column names: x, y not in mtcars")

  res <- expect_silent(check_all_colnames(mtcars, c("mpg")))
  expect_null(res)
})

# check_one_colnames ----

test_that("check_one_colnames works as expected", {
  res <- expect_silent(check_one_colnames(mtcars, c("x", "y")))
  expect_identical(res, " At least one of: x, y is expected to be a column name of mtcars")

  res <- expect_silent(check_one_colnames(mtcars, c("mpg", "x")))
  expect_null(res)
})
