# kmg01 functions ----

test_that("kmg01 works as expected", {
  filter_data <- dunlin::log_filter(syn_data, PARAMCD == "OS", "adtte")
  pre_data <- expect_silent(kmg01_pre(filter_data, dataset = "adtte"))
  raw_res <- expect_silent(kmg01_main(pre_data, dataset = "adtte"))
  expect_true(grid::is.grob(raw_res))
})

# kmg01 ----

test_that("kmg01 works as expected with custom color set", {
  col <- c(
    "A: Drug X" = "black",
    "B: Placebo" = "blue",
    "C: Combination" = "gray"
  )

  filter_data <- dunlin::log_filter(syn_data, PARAMCD == "OS", "adtte")
  res <- expect_silent(run(kmg01, filter_data, dataset = "adtte", col = col))
  expect_true(grid::is.grob(res))
  res <- expect_silent(run(kmg01, filter_data, dataset = "adtte", col = unname(col)))
  expect_true(grid::is.grob(res))
})

test_that("kmg01 works if change pvalue, ties and conf level", {
  filter_data <- dunlin::log_filter(syn_data, PARAMCD == "OS", "adtte")
  res <- expect_silent(run(kmg01, filter_data,
    dataset = "adtte",
    pval_method = "log-rank",
    ties = "efron",
    conf_level = 0.99
  ))
  expect_true(grid::is.grob(res))
})


test_that("kmg01 works if change annotation position", {
  filter_data <- dunlin::log_filter(syn_data, PARAMCD == "OS", "adtte")
  res <- expect_silent(run(kmg01, filter_data,
    dataset = "adtte", annot_surv_med = FALSE,
    position_coxph = c(0.4, 0.5), position_surv_med = c(1, 0.7)
  ))
  expect_true(grid::is.grob(res))
})


test_that("kmg01 works for stratified anlaysis", {
  filter_data <- dunlin::log_filter(syn_data, PARAMCD == "OS", "adtte")
  res <- expect_silent(run(kmg01, filter_data,
    dataset = "adtte", annot_surv_med = FALSE,
    position_coxph = c(0.4, 0.5), position_surv_med = c(1, 0.7),
    strat = c("STRATA1", "STRATA2")
  ))
  expect_true(grid::is.grob(res))
})
