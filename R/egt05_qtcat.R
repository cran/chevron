# egt05_qtcat ----

#' @describeIn egt05_qtcat Main TLG function
#'
#' @inheritParams gen_args
#' @param summaryvars (`character`) variables to be analyzed. The label attribute of the corresponding column in `adeg`
#'   table of `adam_db` is used as name.
#' @returns the main function returns an `rtables` object.
#'
#' @details
#'  * The `Value at Visit` column, displays the categories of the specific `"PARAMCD"` value for patients.
#'  * The `Change from Baseline` column, displays the categories of the specific `"PARAMCD"` value
#'  change from baseline for patients.
#'  * Remove zero-count rows unless overridden with `prune_0 = FALSE`.
#'  * Split columns by arm, typically `"ACTARM"`.
#'  * Does not include a total column by default.
#'  * Sorted based on factor level; by chronological time point given by `"AVISIT"`
#'  or user-defined visit incorporating `"ATPT"`.
#'  Re-level to customize order.
#'  * Please note that it is preferable to convert `summaryvars` to factor.
#'
#' @note
#'  * `adam_db` object must contain an `adeg` table with column specified in `visitvar`.
#'  For `summaryvars`, please make sure `AVALCAT1` and `CHGCAT1` columns existed in input data sets.
#'
#' @export
#'
egt05_qtcat_main <- function(adam_db,
                             arm_var = "ACTARM",
                             lbl_overall = NULL,
                             summaryvars = c("AVALCAT1", "CHGCAT1"),
                             row_split_var = NULL,
                             visitvar = "AVISIT",
                             page_var = NULL,
                             ...) {
  assert_all_tablenames(adam_db, c("adsl", "adeg"))
  assert_string(arm_var)
  assert_string(lbl_overall, null.ok = TRUE)
  assert_character(summaryvars)
  assert_character(row_split_var, null.ok = TRUE)
  assert_string(visitvar)
  assert_string(page_var, null.ok = TRUE)
  assert_valid_var_pair(adam_db$adsl, adam_db$adeg, arm_var)
  assert_valid_variable(adam_db$adeg, "USUBJID", empty_ok = TRUE, types = list(c("character", "factor")))
  assert_valid_variable(adam_db$adsl, c("USUBJID", arm_var), types = list(c("character", "factor")))
  assert_valid_variable(adam_db$adeg, c("PARAM", "PARAMCD"), types = list(c("character", "factor")), na_ok = FALSE)
  assert_valid_variable(adam_db$adeg, visitvar, types = list("character", "factor"))
  assert_disjunct(row_split_var, c("PARAMCD", "PARAM", visitvar))
  assert_valid_variable(adam_db$adeg, summaryvars, types = list(c("factor", "character")), na_ok = TRUE)
  assert_subset(page_var, c(row_split_var, "PARAMCD"))

  lbl_overall <- render_safe(lbl_overall)
  lbl_avisit <- var_labels_for(adam_db$adeg, visitvar)
  lbl_param <- var_labels_for(adam_db$adeg, "PARAM")
  summaryvars_lbls <- var_labels_for(adam_db$adeg, summaryvars) # Value at visit / change from baseline
  row_split_lbl <- var_labels_for(adam_db$adeg, row_split_var)

  lyt <- egt05_qtcat_lyt(
    arm_var = arm_var,
    lbl_overall = lbl_overall,
    lbl_avisit = lbl_avisit,
    lbl_param = lbl_param,
    lbl_cat = "Category",
    summaryvars = summaryvars,
    summaryvars_lbls = summaryvars_lbls,
    row_split_var = row_split_var,
    row_split_lbl = row_split_lbl,
    visitvar = visitvar,
    page_var = page_var
  )

  build_table(
    lyt,
    df = adam_db$adeg,
    alt_counts_df = adam_db$adsl
  )
}

#' `EGT05_QTCAT` Layout
#'
#' @inheritParams gen_args
#'
#' @param lbl_avisit (`string`) label of the `visitvar` variable.
#' @param lbl_param (`string`) label of the `PARAM` variable.
#' @param lbl_cat (`string`) label of the Category of `summaryvars` variable. Default as `Category`.
#' @param summaryvars (`character`) the variables to be analyzed. `AVALCAT1` and `CHGCAT1` by default.
#' @param summaryvars_lbls (`character`) the label of the variables to be analyzed.
#' @param visitvar (`string`) typically `"AVISIT"` or user-defined visit incorporating `"ATPT"`.
#' @returns a `PreDataTableLayouts` object.
#'
#' @keywords internal
#'
egt05_qtcat_lyt <- function(arm_var,
                            lbl_overall,
                            lbl_avisit,
                            lbl_param,
                            lbl_cat,
                            summaryvars,
                            summaryvars_lbls,
                            row_split_var,
                            row_split_lbl,
                            visitvar,
                            page_var) {
  page_by <- get_page_by(page_var, c(row_split_var, "PARAMCD"))
  label_pos <- ifelse(page_by, "hidden", "topleft")
  basic_table(show_colcounts = TRUE) %>%
    split_cols_by_with_overall(arm_var, lbl_overall) %>%
    split_rows_by_recursive(
      row_split_var,
      split_label = row_split_lbl,
      label_pos = head(label_pos, -1L), page_by = head(page_by, -1L)
    ) %>%
    split_rows_by(
      var = "PARAMCD",
      labels_var = "PARAM",
      split_fun = drop_split_levels,
      label_pos = tail(label_pos, 1L),
      split_label = lbl_param,
      page_by = tail(page_by, 1L)
    ) %>%
    split_rows_by(
      visitvar,
      split_fun = drop_split_levels,
      split_label = lbl_avisit,
      label_pos = "topleft"
    ) %>%
    summarize_vars_allow_na(
      vars = summaryvars,
      var_labels = summaryvars_lbls,
      inclNAs = FALSE
    ) %>%
    append_topleft(paste0(stringr::str_dup(" ", sum(!page_by) * 2 + 2), lbl_cat))
}

#' @describeIn egt05_qtcat Preprocessing
#'
#' @inheritParams gen_args
#' @returns the preprocessing function returns a `list` of `data.frame`.
#'
#' @export
#'
egt05_qtcat_pre <- function(adam_db, ...) {
  adam_db$adeg <- adam_db$adeg %>%
    filter(.data$ANL01FL == "Y") %>%
    mutate(
      AVALCAT1 = reformat(.data$AVALCAT1, empty_rule),
      CHGCAT1 = reformat(.data$CHGCAT1, empty_rule),
      AVISIT = reorder(.data$AVISIT, .data$AVISITN),
      AVISIT = with_label(.data$AVISIT, "Analysis Visit")
    )
  adam_db
}

#' @describeIn egt05_qtcat Postprocessing
#'
#' @inheritParams gen_args
#' @returns the postprocessing function returns an `rtables` object or an `ElementaryTable` (null report).
#'
#' @export
#'
egt05_qtcat_post <- function(tlg, prune_0 = TRUE, ...) {
  if (prune_0) tlg <- smart_prune(tlg)
  std_postprocessing(tlg)
}

#' `EGT05_QTCAT` ECG Actual Values and Changes from Baseline by Visit Table.
#'
#' The `EGT05_QTCAT` table summarizes several electrocardiogram parameters and their evolution
#' throughout the study.
#'
#' @include chevron_tlg-S4class.R
#' @export
#'
#' @examples
#' run(egt05_qtcat, syn_data)
egt05_qtcat <- chevron_t(
  main = egt05_qtcat_main,
  preprocess = egt05_qtcat_pre,
  postprocess = egt05_qtcat_post,
  dataset = c("adsl", "adeg")
)
