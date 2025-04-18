# assert_valid_var ----

test_that("assert_valid_var.character works as expected", {
  x <- c("a", NA)
  expect_silent(assert_valid_var(x, na_ok = TRUE))
  expect_error(assert_valid_var(x, na_ok = FALSE))

  x <- "test"
  expect_error(assert_valid_var(x, min_chars = 5))

  expect_error(assert_valid_var(character(0)))
  expect_silent(assert_valid_var(character(0), empty_ok = TRUE))
})

test_that("assert_valid_var.factor works as expected", {
  x <- factor(c("a", NA))
  expect_silent(assert_valid_var(x, na_ok = TRUE))
  expect_error(assert_valid_var(x, na_ok = FALSE))

  x <- factor("test")
  expect_error(assert_valid_var(x, min_chars = 5))

  expect_error(assert_valid_var(factor()))
  expect_silent(assert_valid_var(factor(), empty_ok = TRUE))
})


test_that("assert_valid_var.logical works as expected", {
  x <- factor(c(TRUE, NA))
  expect_silent(assert_valid_var(x, na_ok = TRUE))
  expect_error(assert_valid_var(x, na_ok = FALSE))

  expect_error(assert_valid_var(logical()))
  expect_silent(assert_valid_var(logical(), empty_ok = TRUE))
})

test_that("assert_valid_var.numeric works as expected", {
  x <- c(1, NA)
  expect_silent(assert_valid_var(x, na_ok = TRUE))
  expect_error(
    assert_valid_var(x, na_ok = FALSE),
    "Assertion on 'x' failed: Contains missing values (element 2).",
    fixed = TRUE
  )

  expect_error(
    assert_valid_var(numeric()),
    "Assertion on 'numeric()' failed: Must have length >= 1, but has length 0.",
    fixed = TRUE
  )

  expect_silent(assert_valid_var(numeric(), empty_ok = TRUE))

  x <- c(5, 6.6)
  expect_error(
    assert_valid_var(x, integerish = TRUE),
    "Assertion on 'x' failed: Must be of type 'integerish', but element 2 is not close to an integer.",
    fixed = TRUE
  )
  x <- c(5, 6.0)
  expect_silent(assert_valid_var(x, integerish = TRUE))
})

test_that("assert_valid_var.POSIXct works as expected", {
  x <- as.POSIXct("2020-01-01", "UTC")
  expect_silent(assert_valid_var(x, na_ok = TRUE))

  x <- as.POSIXct("2020-01-01", "")
  expect_error(assert_valid_var(x), "Non standard timezone detected for x !")
})


# assert_valid_variable ----

test_that("assert_valid_variable works as expected", {
  expect_silent(assert_valid_variable(iris, "Species"))
  expect_silent(assert_valid_variable(iris, "Species", types = "factor"))
  expect_error(
    assert_valid_variable(iris, "Species", types = "character"),
    "`iris$Species` is not of type character",
    fixed = TRUE
  )

  expect_silent(assert_valid_variable(iris, c("Sepal.Width", "Sepal.Length"), types = "numeric"))
  expect_error(
    assert_valid_variable(iris, c("Sepal.Width", "Species"), types = "numeric"),
    "`iris$Species` is not of type numeric",
    fixed = TRUE
  )
  expect_silent(assert_valid_variable(iris, c("Sepal.Width", "Species"), types = c("numeric", "factor")))
})

# assert_valid_var_pair ----

test_that("assert_valid_var_pair works as expected with character", {
  df1 <- data.frame(
    id = c("1", "2", "3"),
    arm = c("A", "C", "B")
  )

  df2 <- data.frame(
    id = c("1", "2", "3"),
    arm = c("B", "C", "A")
  )

  expect_silent(assert_valid_var_pair(df1, df2, "arm"))

  df3 <- data.frame(
    id = c("1", "2", "3"),
    arm = c("A", "C", "A")
  )

  expect_error(
    assert_valid_var_pair(df1, df3, "arm"),
    "`df1` and `df3` should contain the same levels in variable `arm`!",
    fixed = TRUE
  )
})

test_that("assert_valid_var_pair works as expected with factor", {
  df1 <- data.frame(
    id = c("1", "2", "3"),
    arm = factor(c("A", "C", "B"), levels = c("B", "C", "A"))
  )

  df2 <- data.frame(
    id = c("1", "2", "3"),
    arm = factor(c("B", "A", "A"), levels = c("B", "C", "A"))
  )

  expect_silent(assert_valid_var_pair(df1, df2, "arm"))

  # Missing level returns an error
  df3 <- data.frame(
    id = c("1", "2", "3"),
    arm = factor(c("B", "A", "A"), levels = c("B", "C", "A", "X"))
  )

  expect_error(
    assert_valid_var_pair(df1, df3, "arm"),
    "`df1` and `df3` should contain the same levels in variable `arm`!",
    fixed = TRUE
  )

  # Different level order caused an error
  df4 <- data.frame(
    id = c("1", "2", "3"),
    arm = factor(c("A", "C", "B"), levels = c("C", "B", "A"))
  )

  expect_error(
    assert_valid_var_pair(df1, df4, "arm"),
    "`df1` and `df4` should contain the same levels in variable `arm`!",
    fixed = TRUE
  )
})
