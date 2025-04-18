# fstg01 ----

#' @describeIn fstg01 Main TLG Function
#'
#' @details
#'  * No overall value.
#'  * Keep zero count rows by default.
#'
#' @inheritParams gen_args
#' @param dataset (`string`) the name of a table in the `adam_db` object.
#' @param arm_var (`string`) the arm variable name used for group splitting.
#' @param rsp_var (`string`) the response variable name to flag whether each subject is a binary response or not.
#' @param subgroups (`character`) the subgroups variable name to list baseline risk factors.
#' @param strata_var (`character`) required if stratified analysis is performed.
#' @param stat_var (`character`) the names of statistics to be reported in `tabulate_rsp_subgroups`.
#' @param ... Further arguments passed to `g_forest` and `extract_rsp_subgroups` (a wrapper for
#'  `h_odds_ratio_subgroups_df` and `h_proportion_subgroups_df`). For details, see the documentation in `tern`.
#'  Commonly used arguments include `col_symbol_size`, `col`, `vline`, `groups_lists`, `conf_level`,
#'  `method`, `label_all`, etc.
#' @returns the main function returns a `grob` object.
#'
#' @note
#'  * `adam_db` object must contain the table specified by `dataset` with `"PARAMCD"`, `"ARM"`,
#'  `"AVALC"`, and the columns specified by `subgroups` which is denoted as
#'  `c("SEX", "AGEGR1", "RACE")` by default.
#'  * If the plot is too large to be rendered in the output, please provide `gp`, `width_row_names`,
#'  `width_columns` and `width_forest` manually to make it fit. See `tern::g_forest` for more details.
#'
#' @returns a `gTree` object.
#'
#' @export
#'
fstg01_main <- function(adam_db,
                        dataset = "adrs",
                        arm_var = "ARM",
                        rsp_var = "IS_RSP",
                        subgroups = c("SEX", "AGEGR1", "RACE"),
                        strata_var = NULL,
                        stat_var = c("n_tot", "n", "n_rsp", "prop", "or", "ci"),
                        ...) {
  assert_all_tablenames(adam_db, c("adsl", dataset))
  df_lbl <- paste0("adam_db$", dataset)
  assert_string(arm_var)
  assert_string(rsp_var)
  assert_character(subgroups, null.ok = TRUE)
  assert_character(strata_var, null.ok = TRUE)
  assert_character(stat_var, null.ok = TRUE)
  assert_valid_variable(adam_db[[dataset]], arm_var, types = list("factor"), n.levels = 2, label = df_lbl)
  assert_valid_variable(adam_db[[dataset]], c("USUBJID", "PARAMCD"),
    types = list(c("character", "factor")),
    label = df_lbl
  )
  assert_valid_variable(adam_db[[dataset]], rsp_var, types = list("logical"), label = df_lbl)
  assert_valid_variable(adam_db[[dataset]], c(subgroups, strata_var),
    types = list(c("factor")), na_ok = TRUE,
    label = df_lbl
  )
  assert_single_value(adam_db[[dataset]]$PARAMCD, label = df_lbl)

  variables <- list(
    arm = arm_var,
    rsp = rsp_var,
    subgroups = subgroups,
    strata = strata_var
  )

  df <- execute_with_args(extract_rsp_subgroups,
    variables = variables,
    data = adam_db[[dataset]],
    ...
  )
  result <- basic_table() %>%
    tabulate_rsp_subgroups(df, vars = stat_var)
  execute_with_args(
    g_forest,
    tbl = result,
    ...,
    font_size = 7
  )
}

#' @describeIn fstg01 Preprocessing
#'
#' @inheritParams fstg01_main
#' @returns the preprocessing function returns a `list` of `data.frame`.
#'
#' @export
#'
fstg01_pre <- function(adam_db, ...) {
  adam_db$adrs <- adam_db$adrs %>%
    mutate(
      ARM = droplevels(.data$ARM),
      IS_RSP = .data$AVALC %in% c("CR", "PR")
    )

  adam_db
}

# `fstg01` Pipeline ----

#' `FSTG01` Subgroup Analysis of Best Overall Response.
#'
#' The template produces the subgroup analysis of best overall response graphic.
#'
#' @include chevron_tlg-S4class.R
#' @export
#'
#' @examples
#' library(dplyr)
#' library(dunlin)
#'
#' proc_data <- log_filter(
#'   syn_data,
#'   PARAMCD == "BESRSPI" & ARM %in% c("A: Drug X", "B: Placebo"), "adrs"
#' )
#' run(fstg01, proc_data,
#'   subgroups = c("SEX", "AGEGR1", "RACE"),
#'   conf_level = 0.90, dataset = "adrs"
#' )
fstg01 <- chevron_g(
  main = fstg01_main,
  preprocess = fstg01_pre,
  dataset = c("adsl", "adrs")
)
