# pdt02 function ----

test_that("pdt02 function with default argument value return expected result with test data", {
  pre_data <- pdt02_pre(syn_data)
  raw_res <- pdt02_main(pre_data)
  res <- pdt02_post(raw_res)
  expect_snapshot(cat(export_as_txt(res, lpp = 100, colwidths = c(40, 10, 10, 10))))
})

# pdt02 ----

test_that("pdt02 can handle all NA values", {
  skip_on_os("windows")
  proc_data <- syn_data
  proc_data$addv <- proc_data$addv %>%
    mutate(
      DVREAS = NA_character_,
      DVTERM = NA_character_,
    )

  res <- expect_silent(run(pdt02, proc_data))
  expect_snapshot(cat(export_as_txt(res, lpp = 100, colwidths = c(40, 10, 10, 10))))
})

test_that("pdt02 can handle some NA values", {
  skip_on_os("windows")
  new_dvreas <- c(NA_character_, "", as.character(syn_data$addv$DVREAS[-c(1, 2)]))
  new_dvterm <- c(NA_character_, "", as.character(syn_data$addv$DVTERM[-c(1, 2)]))

  proc_data <- syn_data
  proc_data$addv <- proc_data$addv %>%
    mutate(
      DVREAS = new_dvreas,
      DVTERM = new_dvterm
    )

  res <- expect_silent(run(pdt02, proc_data))
  expect_snapshot(cat(export_as_txt(res, lpp = 100, colwidths = c(40, 10, 10, 10))))
})
