# aet10 ----

#' @describeIn aet10 Main TLG function
#'
#' @inheritParams gen_args
#' @returns the main function returns an `rtables` object
#' @details
#'  * Numbers represent absolute numbers of subject and fraction of `N`, or absolute number of event when specified.
#'  * Remove zero-count rows unless overridden with `prune_0 = FALSE`.
#'  * Split columns by arm.
#'  * Does not include a total column by default.
#'  * Sort Dictionary-Derived Code (`AEDECOD`) by highest overall frequencies.
#'  * Missing values in `AEDECOD` are labeled by `No Coding Available`.
#'
#' @note
#'  * `adam_db` object must contain an `adae` table with the columns `"AEDECOD"`.
#'
#' @export
#'
aet10_main <- function(adam_db,
                       arm_var = "ACTARM",
                       lbl_overall = NULL,
                       ...) {
  assert_all_tablenames(adam_db, "adsl", "adae")
  assert_string(arm_var)
  assert_string(lbl_overall, null.ok = TRUE)
  assert_valid_variable(adam_db$adsl, c("USUBJID", arm_var), types = list(c("character", "factor")))
  assert_valid_variable(adam_db$adae, c(arm_var, "AEBODSYS", "AEDECOD"), types = list(c("character", "factor")))
  assert_valid_variable(adam_db$adae, "USUBJID", empty_ok = TRUE, types = list(c("character", "factor")))
  assert_valid_var_pair(adam_db$adsl, adam_db$adae, arm_var)

  lbl_overall <- render_safe(lbl_overall)
  lbl_aedecod <- var_labels_for(adam_db$adae, "AEDECOD")

  lyt <- aet10_lyt(
    arm_var = arm_var,
    lbl_overall = lbl_overall,
    lbl_aedecod = lbl_aedecod
  )

  tbl <- build_table(lyt, adam_db$adae, alt_counts_df = adam_db$adsl)

  tbl
}

#' `aet10` Layout
#'
#' @inheritParams gen_args
#' @param lbl_aedecod (`character`) text label for `AEDECOD`.
#' @returns a `PreDataTableLayouts` object.
#' @keywords internal
#'
aet10_lyt <- function(arm_var,
                      lbl_overall,
                      lbl_aedecod) {
  basic_table(show_colcounts = TRUE) %>%
    split_cols_by_with_overall(arm_var, lbl_overall) %>%
    count_occurrences(
      vars = "AEDECOD",
      .indent_mods = -1L
    ) %>%
    append_topleft(paste0("\n", lbl_aedecod))
}

#' @describeIn aet10 Preprocessing
#'
#' @inheritParams gen_args
#' @returns the preprocessing function returns a `list` of `data.frame`.
#' @export
#'
aet10_pre <- function(adam_db, ...) {
  adam_db$adae <- adam_db$adae %>%
    filter(.data$ANL01FL == "Y") %>%
    mutate(AEDECOD = with_label(reformat(.data$AEDECOD, nocoding), "MedDRA Preferred Term"))
  adam_db
}

#' @describeIn aet10 Postprocessing
#'
#' @inheritParams gen_args
#' @param atleast given cut-off in numeric format, default is `0.05`
#' @returns the postprocessing function returns an `rtables` object or an `ElementaryTable` (null report).
#' @export
#'
aet10_post <- function(tlg, atleast = 0.05, ...) {
  assert_number(atleast, lower = 0, upper = 1)
  tbl_sorted <- tlg %>%
    sort_at_path(
      path = c("AEDECOD"),
      scorefun = score_occurrences
    )

  tlg_prune <- prune_table(
    tt = tbl_sorted,
    prune_func = keep_rows(
      has_fraction_in_any_col(
        atleast = atleast
      )
    )
  )

  std_postprocessing(tlg_prune)
}

#' `AET10` Table 1 (Default) Most Common (xx%) Adverse Events Preferred Terms Table 1.
#'
#' The `AET10` table Include Adverse Events occurring with user-specified threshold X% in at least
#'  one of the treatment groups. Standard table summarized by preferred term (PT).
#' Order the data by total column frequency from most to least frequently reported PT (regardless of SOC).
#'
#' @include chevron_tlg-S4class.R
#' @export
#'
#' @examples
#' run(aet10, syn_data)
aet10 <- chevron_t(
  main = aet10_main,
  preprocess = aet10_pre,
  postprocess = aet10_post,
  dataset = c("adsl", "adae")
)
