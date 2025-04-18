# egt02_1 ----

#' @describeIn egt02_1 Main TLG function
#'
#' @inheritParams gen_args
#' @param exclude_base_abn (`flag`) whether baseline abnormality should be excluded.
#' @returns the main function returns an `rtables` object
#'
#' @details
#'   * Only count LOW or HIGH values.
#'   * Results of "LOW LOW" are treated as the same as "LOW", and "HIGH HIGH" the same as "HIGH".
#'   * Does not include a total column by default.
#'   * Does not remove zero-count rows unless overridden with `prune_0 = TRUE`.
#'
#' @note
#'   * `adam_db` object must contain an `adeg` table with the `"PARAM"`, `"ANRIND"` and `"BNRIND"` columns.
#'
#' @export
#'
egt02_1_main <- function(adam_db,
                         arm_var = "ACTARM",
                         lbl_overall = NULL,
                         exclude_base_abn = FALSE,
                         ...) {
  assert_all_tablenames(adam_db, c("adsl", "adeg"))
  assert_string(arm_var)
  assert_string(lbl_overall, null.ok = TRUE)
  assert_flag(exclude_base_abn)
  assert_valid_variable(adam_db$adeg, c("PARAM"), types = list(c("character", "factor")), na_ok = FALSE)
  assert_valid_variable(adam_db$adeg, c("ANRIND", "BNRIND"), types = list(c("character", "factor")), na_ok = TRUE)
  assert_valid_var_pair(adam_db$adsl, adam_db$adeg, arm_var)
  assert_valid_variable(adam_db$adeg, "USUBJID", empty_ok = TRUE, types = list(c("character", "factor")))
  assert_valid_variable(adam_db$adsl, c("USUBJID", arm_var), types = list(c("character", "factor")))

  lbl_overall <- render_safe(lbl_overall)

  lyt <- egt02_lyt(
    arm_var = arm_var,
    lbl_overall = lbl_overall,
    lbl_vs_assessment = "Assessment",
    lbl_vs_abnormality = "Abnormality",
    exclude_base_abn = exclude_base_abn
  )

  tbl <- build_table(lyt, adam_db$adeg, alt_counts_df = adam_db$adsl)

  tbl
}

#' `egt02` Layout
#'
#' @inheritParams gen_args
#' @param lbl_vs_assessment (`string`) the label of the assessment variable.
#' @param lbl_vs_abnormality (`string`) the label of the abnormality variable.
#' @param exclude_base_abn (`flag`) whether to exclude subjects with baseline abnormality from numerator and
#'   denominator.
#' @returns a `PreDataTableLayouts` object.
#'
#' @keywords internal
#'
egt02_lyt <- function(arm_var = "ACTARM",
                      lbl_overall,
                      lbl_vs_assessment = "Assessment",
                      lbl_vs_abnormality = "Abnormality",
                      exclude_base_abn) {
  basic_table(show_colcounts = TRUE) %>%
    split_cols_by_with_overall(arm_var, lbl_overall) %>%
    split_rows_by("PARAM", split_fun = drop_split_levels, label_pos = "topleft", split_label = lbl_vs_assessment) %>%
    count_abnormal(
      "ANRIND",
      abnormal = list(Low = "LOW", High = "HIGH"),
      variables = list(id = "USUBJID", baseline = "BNRIND"),
      exclude_base_abn = exclude_base_abn
    ) %>%
    append_topleft(paste0(" ", lbl_vs_abnormality))
}

#' @describeIn egt02_1 Preprocessing
#'
#' @inheritParams gen_args
#' @returns the preprocessing function returns a `list` of `data.frame`.
#' @export
#'
egt02_pre <- function(adam_db, ...) {
  adam_db$adeg <- adam_db$adeg %>%
    mutate(ANRIND = factor(.data$ANRIND, levels = c("LOW", "NORMAL", "HIGH"))) %>%
    filter(!is.na(.data$ANRIND)) %>%
    filter(.data$ONTRTFL == "Y")

  adam_db
}

#' @describeIn egt02_1 Postprocessing
#'
#' @inheritParams gen_args
#' @returns the postprocessing function returns an `rtables` object or an `ElementaryTable` (null report).
#' @export
#'
egt02_post <- function(tlg, ...) {
  std_postprocessing(tlg)
}

#' `EGT02` ECG Abnormalities Table.
#'
#' ECG Parameters outside Normal Limits Regardless of Abnormality at Baseline Table.
#'
#' @include chevron_tlg-S4class.R
#' @export
#'
#' @examples
#' run(egt02_1, syn_data)
egt02_1 <- chevron_t(
  main = egt02_1_main,
  preprocess = egt02_pre,
  postprocess = egt02_post,
  dataset = c("adsl", "adeg")
)

# egt02_2 ----

#' @describeIn egt02_2 Main TLG function
#'
#' @inherit egt02_1_main
#'
#' @export
#'
egt02_2_main <- modify_default_args(egt02_1_main, exclude_base_abn = TRUE)

#' `EGT02_2` ECG Abnormalities Table.
#'
#' ECG Parameters outside Normal Limits Among Patients without Abnormality at Baseline Table.
#'
#' @include chevron_tlg-S4class.R
#' @export
#'
#' @examples
#' run(egt02_2, syn_data)
egt02_2 <- chevron_t(
  main = egt02_2_main,
  preprocess = egt02_pre,
  postprocess = egt02_post,
  dataset = c("adsl", "adeg")
)
