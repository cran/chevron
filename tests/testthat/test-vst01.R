# vst12 functions ----

test_that("vst01 functions with default argument value return expected result with test data", {
  pre_data <- vst01_pre(syn_data)
  raw_res <- vst01_main(pre_data)
  res <- cfbt01_post(raw_res)
  expect_snapshot(cat(export_as_txt(res, lpp = 200)))
})

# vst01 ----

test_that("run vst01 works as expected", {
  res <- expect_silent(run(vst01, syn_data))
  expect_snapshot(cat(export_as_txt(res, lpp = 200)))
})
