test_that("aet10 can handle NA values", {
  proc_data <- syn_data

  proc_data$adae <- proc_data$adae %>%
    mutate(
      AEDECOD = with_label(NA_character_, var_labels_for(syn_data$adae, "AEDECOD"))
    )

  res <- expect_silent(run(aet10, proc_data))
  expect_snapshot(cat(export_as_txt(res, lpp = 100)))
})

test_that("aet10 can handle some NA values", {
  skip_on_os("windows")
  new_aedecod <- c(NA, "", as.character(syn_data$adae$AEDECOD[-c(1, 2)]))

  proc_data <- syn_data

  proc_data$adae <- proc_data$adae %>%
    mutate(
      AEDECOD = with_label(.env$new_aedecod, var_labels_for(syn_data$adae, "AEDECOD"))
    )

  res <- expect_silent(run(aet10, proc_data))
  expect_snapshot(cat(export_as_txt(res, lpp = 100)))
})
