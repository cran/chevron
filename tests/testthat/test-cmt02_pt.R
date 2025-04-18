test_that("cmt02_pt can handle all NA values", {
  proc_data <- syn_data
  proc_data$adcm <- proc_data$adcm %>%
    mutate(
      CMDECOD = NA_character_
    )

  res <- expect_silent(run(cmt02_pt, proc_data))
  expect_snapshot(cat(export_as_txt(res, lpp = 100)))
})

test_that("cmt02_pt can handle some NA values", {
  skip_on_os("windows")
  proc_data <- syn_data
  proc_data$adcm[1:2, c("CMDECOD")] <- NA

  res <- expect_silent(run(cmt02_pt, proc_data))
  expect_snapshot(cat(export_as_txt(res, lpp = 100)))
})
