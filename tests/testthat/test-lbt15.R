# lbt15 functions ----

test_that("lbt15 functions with default argument value return expected result with test data", {
  pre_data <- lbt15_pre(syn_data)
  raw_res <- lbt04_main(pre_data)
  res <- lbt04_post(raw_res)
  expect_snapshot(cat(export_as_txt(res, lpp = 100)))
})

# lbt15 ----

test_that("lbt15 can handle all NA values", {
  skip_on_os("windows")
  proc_data <- syn_data
  proc_data$adlb <- proc_data$adlb %>%
    mutate(
      ATOXGR = NA_character_
    )

  res <- expect_silent(run(lbt15, proc_data))
  expect_snapshot(cat(export_as_txt(res, lpp = 100)))
})

test_that("lbt15 can handle missing levels", {
  skip_on_os("windows")
  proc_data <- syn_data
  proc_data$adlb <- proc_data$adlb %>%
    mutate(
      ATOXGR = ""
    )

  res <- expect_silent(run(lbt15, proc_data))
  expect_snapshot(cat(export_as_txt(res, lpp = 100)))
})

test_that("lbt15 can handle missing levels in baseline", {
  skip_on_os("windows")
  proc_data <- syn_data
  proc_data$adlb <- proc_data$adlb %>%
    mutate(
      BTOXGR = ""
    )

  res <- expect_silent(run(lbt15, proc_data))
  expect_snapshot(cat(export_as_txt(res, lpp = 100)))
})

test_that("lbt15 can handle some NA values", {
  skip_on_os("windows")
  proc_data <- syn_data
  proc_data$adlb[1:2, "ATOXGR"] <- NA

  res <- expect_silent(run(lbt15, proc_data))
  expect_snapshot(cat(export_as_txt(res, lpp = 100)))
})

test_that("lbt15 can handle some NA values in baseline", {
  skip_on_os("windows")
  proc_data <- syn_data
  proc_data$adlb[1:2, "BTOXGR"] <- NA

  res <- expect_silent(run(lbt15, proc_data))
  expect_snapshot(cat(export_as_txt(res, lpp = 100)))
})

test_that("lbt15 fails on incomlete data", {
  skip_on_os("windows")
  proc_data <- syn_data
  proc_data$adlb <- proc_data$adlb %>%
    mutate(PARCAT1 = NULL)

  expect_error(run(lbt15, proc_data))
})
