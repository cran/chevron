# lbt06 ----

#' @describeIn lbt06 Main TLG function
#'
#' @inheritParams gen_args
#' @param arm_var (`string`) the arm variable used for arm splitting.
#' @returns the main function returns an `rtables` object.
#'
#' @details
#'  * Only count `"LOW"` or `"HIGH"` values for `ANRIND` and `BNRIND`.
#'  * Lab test results with missing `ANRIND` values are excluded.
#'  * Split columns by arm, typically `ACTARM`.
#'  * Keep zero count rows by default.
#'
#' @note
#'  * `adam_db` object must contain an `adlb` table with columns `"AVISIT"`, `"ANRIND"`, `"BNRIND"`,
#'  `"ONTRTFL"`, and `"PARCAT2"`, and column specified by `arm_var`.
#'
#' @export
#'
lbt06_main <- function(adam_db,
                       arm_var = "ACTARM",
                       lbl_overall = NULL,
                       page_var = "PARAMCD",
                       ...) {
  assert_all_tablenames(adam_db, c("adsl", "adlb"))
  assert_string(arm_var)
  assert_string(lbl_overall, null.ok = TRUE)
  assert_subset(page_var, "PARAMCD")
  assert_valid_variable(adam_db$adlb, c(arm_var, "PARAMCD", "PARAM", "AVISIT"), types = list("characater", "factor"))
  assert_valid_variable(adam_db$adlb, c("ANRIND", "BNRIND"), types = list(c("character", "factor")))
  assert_valid_variable(adam_db$adlb, c("USUBJID"), types = list(c("character", "factor")))
  assert_valid_variable(adam_db$adsl, c("USUBJID"), types = list(c("character", "factor")))
  assert_valid_var_pair(adam_db$adsl, adam_db$adlb, arm_var)

  lbl_overall <- render_safe(lbl_overall)
  lbl_param <- var_labels_for(adam_db$adlb, "PARAM")
  lbl_visit <- var_labels_for(adam_db$adlb, "AVISIT")
  lbl_anrind <- var_labels_for(adam_db$adlb, "ANRIND")
  lbl_bnrind <- var_labels_for(adam_db$adlb, "BNRIND")

  lyt <- lbt06_lyt(
    arm_var = arm_var,
    lbl_overall = lbl_overall,
    lbl_param = lbl_param,
    lbl_visit = lbl_visit,
    lbl_anrind = lbl_anrind,
    lbl_bnrind = lbl_bnrind,
    visitvar = "AVISIT",
    anrind_var = "ANRIND",
    bnrind_var = "BNRIND",
    page_var = page_var
  )

  tbl <- build_table(lyt, adam_db$adlb, alt_counts_df = adam_db$adsl)

  tbl
}

#' `lbt06` Layout
#'
#' @inheritParams gen_args
#'
#' @param lbl_param (`string`) text label of the `PARAM` variable.
#' @param lbl_visit (`string`) text label of the `AVISIT` variable.
#' @param lbl_anrind (`string`) text label of the `ANRIND` variable.
#' @param lbl_bnrind (`string`) text label of the `BNRIND` variable.
#' @param anrind_var (`string`) the variable for analysis reference range indicator.
#' @param bnrind_var (`string`) the variable for baseline reference range indicator.
#'
#' @keywords internal
#'
lbt06_lyt <- function(arm_var,
                      lbl_overall,
                      lbl_param,
                      lbl_visit,
                      lbl_anrind,
                      lbl_bnrind,
                      visitvar,
                      anrind_var,
                      bnrind_var,
                      page_var) {
  page_by <- !is.null(page_var)
  label_pos <- ifelse(page_by, "hidden", "topleft")
  basic_table(show_colcounts = TRUE) %>%
    split_cols_by_with_overall(arm_var, lbl_overall) %>%
    split_rows_by(
      var = "PARAMCD",
      labels_var = "PARAM",
      split_fun = drop_split_levels,
      label_pos = label_pos,
      split_label = lbl_param,
      page_by = page_by
    ) %>%
    split_rows_by(
      var = visitvar,
      split_fun = drop_split_levels,
      label_pos = "topleft",
      split_label = lbl_visit
    ) %>%
    count_abnormal_by_baseline(
      var = anrind_var,
      abnormal = c(Low = "LOW", High = "HIGH"),
      variables = list(id = "USUBJID", baseline = bnrind_var),
      .indent_mods = 4L
    ) %>%
    append_topleft(paste0(stringr::str_dup(" ", 2L * (2 - page_by)), lbl_anrind)) %>%
    append_topleft(paste0(stringr::str_dup(" ", 2L * (7 - page_by)), lbl_bnrind))
}

#' @describeIn lbt06 Preprocessing
#'
#' @inheritParams gen_args
#' @returns the preprocessing function returns a `list` of `data.frame`.
#' @export
#'
lbt06_pre <- function(adam_db, ...) {
  adam_db$adlb <- adam_db$adlb %>%
    filter(
      .data$ONTRTFL == "Y",
      .data$PARCAT2 == "SI"
    ) %>%
    mutate(
      across(all_of(c("ANRIND", "BNRIND")), ~ reformat(.x, missing_rule)),
      AVISIT = reorder(.data$AVISIT, .data$AVISITN),
      AVISIT = with_label(.data$AVISIT, "Visit"),
      ANRIND = with_label(.data$ANRIND, "Abnormality at Visit"),
      BNRIND = with_label(.data$BNRIND, "Baseline Status")
    )

  adam_db
}

#' @describeIn lbt06 Postprocessing
#'
#' @inheritParams gen_args
#' @returns the postprocessing function returns an `rtables` object or an `ElementaryTable` (null report).
#' @export
#'
lbt06_post <- function(tlg, prune_0 = FALSE, ...) {
  if (prune_0) {
    tlg <- smart_prune(tlg)
  }
  std_postprocessing(tlg)
}

#' `LBT06` Table 1 (Default) Laboratory Abnormalities by Visit and Baseline Status Table 1.
#'
#' The `LBT06` table produces the standard laboratory abnormalities by visit and
#' baseline status summary.
#'
#' @include chevron_tlg-S4class.R
#' @export
#'
#' @examples
#' run(lbt06, syn_data)
lbt06 <- chevron_t(
  main = lbt06_main,
  preprocess = lbt06_pre,
  postprocess = lbt06_post,
  dataset = c("adsl", "adlb")
)
