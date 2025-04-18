# rspt01 ----

#' @describeIn rspt01 Main TLG function
#'
#' @inheritParams gen_args
#' @param dataset (`string`) the name of a table in the `adam_db` object.
#' @param ref_group (`string`) The name of the reference group, the value should
#'  be identical to the values in `arm_var`, if not specified, it will by default
#'  use the first level or value of `arm_var`.
#' @param odds_ratio (`flag`) should the odds ratio be calculated, default is `TRUE`
#' @param perform_analysis (`string`) option to display statistical comparisons using stratified analyses,
#'  or unstratified analyses, or both, e.g. `c("unstrat", "strat")`. Only unstratified will be displayed by default
#' @param strata (`string`) stratification factors, e.g. `strata = c("STRATA1", "STRATA2")`, by default as NULL
#' @param conf_level (`numeric`) the level of confidence interval, default is 0.95.
#' @param methods (`list`) a named list, use a named list to control, for example:
#' `methods = list(prop_conf_method = "wald",
#'                diff_conf_method = "wald",
#'                strat_diff_conf_method = "ha",
#'                diff_pval_method = "fisher",
#'                strat_diff_pval_method = "schouten")`
#' `prop_conf_method` controls the methods of calculating proportion confidence interval,
#' `diff_conf_method` controls the methods of calculating unstratified difference confidence interval,
#' `strat_diff_conf_method` controls the methods of calculating stratified difference confidence interval,
#' `diff_pval_method` controls the methods of calculating unstratified p-value for odds ratio,
#' `strat_diff_pval_method` controls the methods of calculating stratified p-value for odds ratio,
#' see more details in `tern`
#' @returns the main function returns an `rtables` object.
#'
#' @details
#' * No overall value.
#'
#' @export
#'
rspt01_main <- function(adam_db,
                        dataset = "adrs",
                        arm_var = "ARM",
                        ref_group = NULL,
                        odds_ratio = TRUE,
                        perform_analysis = "unstrat",
                        strata = NULL,
                        conf_level = 0.95,
                        methods = list(),
                        ...) {
  assert_string(dataset)
  assert_all_tablenames(adam_db, "adsl", dataset)
  assert_string(arm_var)
  assert_string(ref_group, null.ok = TRUE)
  assert_flag(odds_ratio)
  assert_subset(perform_analysis, c("unstrat", "strat"))
  assert_character(
    strata,
    null.ok = !"strat" %in% perform_analysis,
    min.len = as.integer(!"strat" %in% perform_analysis)
  )
  df_label <- sprintf("adam_db$%s", dataset)
  assert_valid_variable(
    adam_db$adsl, c("USUBJID", arm_var),
    types = list(c("character", "factor"))
  )
  assert_valid_variable(
    adam_db[[dataset]], c("USUBJID", arm_var, "RSP_LAB"),
    types = list(c("character", "factor")), label = df_label
  )
  assert_valid_variable(adam_db[[dataset]], "IS_RSP", types = list("logical"), label = df_label)
  assert_valid_variable(
    adam_db[[dataset]], c("PARAMCD", "PARAM"),
    types = list(c("character", "factor")), label = df_label
  )
  assert_single_value(adam_db[[dataset]]$PARAMCD, label = sprintf("adam_db$%s$PARAMCD", dataset))
  assert_valid_var_pair(adam_db$adsl, adam_db[[dataset]], arm_var)
  assert_subset(ref_group, lvls(adam_db[[dataset]][[arm_var]]))

  ref_group <- ref_group %||% lvls(adam_db[[dataset]][[arm_var]])[1]

  lyt <- rspt01_lyt(
    arm_var = arm_var,
    rsp_var = "IS_RSP",
    ref_group = ref_group,
    odds_ratio = odds_ratio,
    perform_analysis = perform_analysis,
    strata = strata,
    conf_level = conf_level,
    methods = methods
  )

  tbl <- build_table(lyt, adam_db[[dataset]], alt_counts_df = adam_db$adsl)

  tbl
}

#' `rspt01` Layout
#'
#' @inheritParams gen_args
#'
#' @keywords internal
#'
rspt01_lyt <- function(arm_var,
                       rsp_var,
                       ref_group,
                       odds_ratio,
                       perform_analysis,
                       strata,
                       conf_level,
                       methods) {
  lyt01 <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by(var = arm_var, ref_group = ref_group) %>%
    estimate_proportion(
      vars = rsp_var,
      conf_level = conf_level,
      method = methods[["prop_conf_method"]] %||% "waldcc",
      table_names = "est_prop"
    )

  for (perform in perform_analysis) {
    lyt01 <- lyt01 %>%
      proportion_lyt(
        arm_var = arm_var,
        odds_ratio = odds_ratio,
        strata = if (perform == "strat") strata else NULL,
        conf_level = conf_level,
        methods = methods,
        rsp_var = rsp_var
      )
  }

  lyt <- lyt01 %>%
    estimate_multinomial_response(
      var = "RSP_LAB",
      conf_level = conf_level,
      method = methods[["prop_conf_method"]] %||% "waldcc"
    )

  lyt
}

#' @describeIn rspt01 Preprocessing
#'
#' @inheritParams gen_args
#' @returns the preprocessing function returns a `list` of `data.frame`.
#' @export
#'
rspt01_pre <- function(adam_db, ...) {
  adam_db$adrs <- adam_db$adrs %>%
    mutate(RSP_LAB = tern::d_onco_rsp_label(.data$AVALC)) %>%
    mutate(IS_RSP = .data$AVALC %in% c("CR", "PR"))
  adam_db
}

#' @describeIn rspt01 Postprocessing
#'
#' @inheritParams gen_args
#' @returns the postprocessing function returns an `rtables` object or an `ElementaryTable` (null report).
#' @export
#'
rspt01_post <- function(tlg, prune_0 = TRUE, ...) {
  if (prune_0) {
    tlg <- smart_prune(tlg)
  }
  std_postprocessing(tlg)
}

#' `RSPT01` Binary Outcomes Summary.
#'
#' `RSPT01` template may be used to summarize any binary outcome or response variable at
#' a single time point. Typical application for oncology
#'
#' @include chevron_tlg-S4class.R
#' @export
#'
#' @examples
#' library(dplyr)
#' library(dunlin)
#'
#' proc_data <- log_filter(syn_data, PARAMCD == "BESRSPI", "adrs")
#'
#' run(rspt01, proc_data)
#'
#' run(rspt01, proc_data,
#'   odds_ratio = FALSE, perform_analysis = c("unstrat", "strat"),
#'   strata = c("STRATA1", "STRATA2"), methods = list(diff_pval_method = "fisher")
#' )
rspt01 <- chevron_t(
  main = rspt01_main,
  preprocess = rspt01_pre,
  postprocess = rspt01_post,
  dataset = c("adsl", "adrs")
)
