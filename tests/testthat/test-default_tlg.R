test_that("ael01_nollt function with default argument value return expected result with test data", {
  pre_data <- ael01_nollt_pre(syn_data)
  raw_res <- ael01_nollt_main(pre_data)
  res <- std_postprocessing(raw_res)
  expect_snapshot(cat(export_as_txt(res, lpp = 100)))
})

test_that("aet01 function with default argument value return expected result with test data", {
  pre_data <- aet01_pre(syn_data)
  raw_res <- aet01_main(pre_data)
  res <- aet01_post(raw_res)
  expect_snapshot(cat(export_as_txt(res, lpp = 100)))
})

test_that("aet01_aesi function with default argument value return expected result with test data", {
  pre_data <- aet01_aesi_pre(syn_data)
  raw_res <- aet01_aesi_main(pre_data)
  res <- aet01_aesi_post(raw_res)
  expect_snapshot(cat(export_as_txt(res, lpp = 100)))
})

test_that("aet02 functions with default argument value return expected result with test data", {
  pre_data <- aet02_pre(syn_data)
  raw_res <- aet02_main(pre_data)
  res <- aet02_post(raw_res)
  expect_snapshot(cat(export_as_txt(res, lpp = 100)))
})

test_that("aet03 functions with default argument value return expected result with test data", {
  pre_data <- aet03_pre(syn_data)
  raw_res <- aet03_main(pre_data)
  res <- aet03_post(raw_res)
  expect_snapshot(cat(export_as_txt(res, lpp = 100)))
})

test_that("aet04 functions with default argument value return expected result with test data", {
  pre_data <- aet04_pre(syn_data)
  raw_res <- aet04_main(pre_data)
  res <- aet04_post(raw_res)
  expect_snapshot(cat(export_as_txt(res, lpp = 100)))
})

test_that("aet05 functions with default argument value return expected result with test data", {
  pre_data <- aet05_pre(syn_data)
  raw_res <- aet05_main(pre_data)
  res <- aet05_post(raw_res)
  expect_snapshot(cat(export_as_txt(res, lpp = 100)))
})

test_that("aet05_all functions with default argument value return expected result with test data", {
  pre_data <- aet05_all_pre(syn_data)
  raw_res <- aet05_main(pre_data)
  res <- aet05_post(raw_res)
  expect_snapshot(cat(export_as_txt(res, lpp = 100)))
})

test_that("aet10 functions with default argument value return expected result with test data", {
  pre_data <- aet10_pre(syn_data)
  raw_res <- aet10_main(pre_data)
  res <- aet10_post(raw_res)
  expect_snapshot(cat(export_as_txt(res, lpp = 100)))
})

test_that("cmt02_pt functions with default argument value return expected result with test data", {
  pre_data <- cmt02_pt_pre(syn_data)
  raw_res <- cmt02_pt_main(pre_data)
  res <- cmt02_pt_post(raw_res)
  expect_snapshot(cat(export_as_txt(res, lpp = 100)))
})

test_that("coxt01 functions with default argument value return expected result with test data", {
  proc_data <- dunlin::log_filter(syn_data, PARAMCD == "CRSD", "adtte")
  proc_data <- dunlin::log_filter(proc_data, ARMCD != "ARM C", "adsl")
  proc_data$adtte$ARM <- droplevels(proc_data$adtte$ARM)
  pre_data <- coxt01_pre(proc_data)
  raw_res <- coxt01_main(pre_data)
  res <- coxt01_post(raw_res)
  expect_snapshot(cat(export_as_txt(res, lpp = 100)))
})

test_that("coxt02 functions with default argument value return expected result with test data", {
  proc_data <- dunlin::log_filter(syn_data, PARAMCD == "CRSD", "adtte")
  pre_data <- coxt01_pre(proc_data)
  raw_res <- coxt02_main(pre_data)
  res <- coxt01_post(raw_res)
  expect_snapshot(cat(export_as_txt(res, lpp = 100)))
})

test_that("dmt01 functions with default argument value return expected result with test data", {
  pre_data <- dmt01_pre(syn_data)
  raw_res <- dmt01_main(pre_data)
  res <- dmt01_post(raw_res)
  expect_snapshot(cat(export_as_txt(res, lpp = 100)))
})

test_that("dst01 functions with default argument value return expected result with test data", {
  pre_data <- dst01_pre(syn_data)
  raw_res <- dst01_main(pre_data)
  res <- dst01_post(raw_res)
  expect_snapshot(cat(export_as_txt(res, lpp = 100)))
})

test_that("dtht01 functions with default argument value return expected result with test data", {
  pre_data <- dtht01_pre(syn_data)
  raw_res <- dtht01_main(pre_data)
  res <- dtht01_post(raw_res)
  expect_snapshot(cat(export_as_txt(res, lpp = 100)))
})
