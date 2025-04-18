# aet01 ----

#' @describeIn aet01 Main TLG function
#'
#' @inheritParams gen_args
#' @param anl_vars Named (`list`) of (`character`) variables the safety variables to be summarized.
#' @param show_wd (`flag`) whether to display the number of patients withdrawn from study due to an adverse event and
#'   the number of death.
#' @param anl_lbls (`character`) of analysis labels.
#' @returns the main function returns an `rtables` object.
#'
#' @details
#'  * Does not remove rows with zero counts by default.
#'
#' @note
#'  * `adam_db` object must contain an `adsl` table with the `"DTHFL"` and `"DCSREAS"` columns.
#'  * `adam_db` object must contain an `adae` table with the columns passed to `anl_vars`.
#'
#' @export
#'
aet01_main <- function(adam_db,
                       arm_var = "ACTARM",
                       lbl_overall = NULL,
                       anl_vars = list(
                         safety_var = c(
                           "FATAL", "SER", "SERWD", "SERDSM",
                           "RELSER", "WD", "DSM", "REL", "RELWD", "RELDSM", "SEV"
                         )
                       ),
                       anl_lbls = "Total number of {patient_label} with at least one",
                       show_wd = TRUE,
                       ...) {
  assert_all_tablenames(adam_db, "adsl", "adae")
  assert_string(arm_var)
  assert_string(lbl_overall, null.ok = TRUE)
  assert_list(anl_vars, types = "character", names = "unique")
  assert_character(anl_lbls, min.chars = 1L)
  assert_flag(show_wd)
  assert_valid_variable(adam_db$adsl, c("USUBJID", arm_var), types = list(c("character", "factor")), empty_ok = TRUE)
  assert_valid_variable(
    adam_db$adsl,
    c("DTHFL", "DCSREAS"),
    types = list(c("character", "factor")),
    min_chars = 0L, empty_ok = TRUE
  )
  assert_valid_variable(adam_db$adae, c(arm_var), types = list(c("character", "factor")))
  assert_valid_variable(adam_db$adae, "USUBJID", empty_ok = TRUE, types = list(c("character", "factor")))
  assert_valid_variable(adam_db$adae, unlist(anl_vars), types = list("logical"), na_ok = TRUE, empty_ok = TRUE)
  assert_valid_var_pair(adam_db$adsl, adam_db$adae, arm_var)

  lbl_overall <- render_safe(lbl_overall)
  anl_lbls <- render_safe(anl_lbls)
  if (length(anl_lbls) == 1) {
    anl_lbls <- rep(anl_lbls, length(anl_vars))
  }
  lbl_vars <- lapply(
    anl_vars,
    var_labels_for,
    df = adam_db$adae
  )

  lyts <- aet01_lyt(
    arm_var = arm_var,
    lbl_overall = lbl_overall,
    anl_vars = anl_vars,
    anl_lbls = anl_lbls,
    lbl_vars = lbl_vars
  )

  if (show_wd) {
    rbind(
      build_table(lyts$ae1, adam_db$adae, alt_counts_df = adam_db$adsl),
      build_table(lyts$adsl, adam_db$adsl, alt_counts_df = adam_db$adsl),
      build_table(lyts$ae2, adam_db$adae, alt_counts_df = adam_db$adsl)
    )
  } else {
    rbind(
      build_table(lyts$ae1, adam_db$adae, alt_counts_df = adam_db$adsl),
      build_table(lyts$ae2, adam_db$adae, alt_counts_df = adam_db$adsl)
    )
  }
}

#' `aet01` Layout
#'
#' @inheritParams aet01_main
#' @param anl_vars Named (`list`) of analysis variables.
#' @param anl_lbls (`character`) of labels.
#' @param lbl_vars Named (`list`) of analysis labels.
#' @returns a `PreDataTableLayouts` object.
#' @keywords internal
#'
aet01_lyt <- function(arm_var,
                      lbl_overall,
                      anl_vars,
                      anl_lbls,
                      lbl_vars) {
  lyt_base <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by_with_overall(arm_var, lbl_overall)

  lyt_ae1 <- lyt_base %>%
    analyze_num_patients(
      vars = "USUBJID",
      .stats = c("unique", "nonunique"),
      .labels = c(
        unique = render_safe("Total number of {patient_label} with at least one AE"),
        nonunique = "Total number of AEs"
      ),
      .formats = list(unique = format_count_fraction_fixed_dp, nonunique = "xx"),
      show_labels = "hidden"
    )

  lyt_adsl <- lyt_base %>%
    count_patients_with_event(
      "USUBJID",
      filters = c("DTHFL" = "Y"),
      denom = "N_col",
      .labels = c(count_fraction = "Total number of deaths"),
      table_names = "TotDeath"
    ) %>%
    count_patients_with_event(
      "USUBJID",
      filters = c("DCSREAS" = "ADVERSE EVENT"),
      denom = "N_col",
      .labels = c(count_fraction = render_safe("Total number of {patient_label} withdrawn from study due to an AE")),
      table_names = "TotWithdrawal"
    )

  lyt_ae2 <- lyt_base %>%
    count_patients_recursive(
      anl_vars = anl_vars,
      anl_lbls = anl_lbls,
      lbl_vars = lbl_vars
    )

  list(
    ae1 = lyt_ae1,
    ae2 = lyt_ae2,
    adsl = lyt_adsl
  )
}

#' @describeIn aet01 Preprocessing
#'
#' @inheritParams aet01_main
#' @returns the preprocessing function returns a `list` of `data.frame`.
#'
#' @export
#'
aet01_pre <- function(adam_db, ...) {
  adam_db$adae <- adam_db$adae %>%
    filter(.data$ANL01FL == "Y") %>%
    mutate(
      FATAL = with_label(.data$AESDTH == "Y", "AE with fatal outcome"),
      SER = with_label(.data$AESER == "Y", "Serious AE"),
      SEV = with_label(.data$ASEV == "SEVERE", "Severe AE (at greatest intensity)"),
      REL = with_label(.data$AREL == "Y", "Related AE"),
      WD = with_label(.data$AEACN == "DRUG WITHDRAWN", "AE leading to withdrawal from treatment"),
      DSM = with_label(
        .data$AEACN %in% c("DRUG INTERRUPTED", "DOSE INCREASED", "DOSE REDUCED"),
        "AE leading to dose modification/interruption"
      ),
      SERWD = with_label(.data$SER & .data$WD, "Serious AE leading to withdrawal from treatment"),
      SERDSM = with_label(.data$SER & .data$DSM, "Serious AE leading to dose modification/interruption"),
      RELSER = with_label(.data$SER & .data$REL, "Related Serious AE"),
      RELWD = with_label(.data$REL & .data$WD, "Related AE leading to withdrawal from treatment"),
      RELDSM = with_label(.data$REL & .data$DSM, "Related AE leading to dose modification/interruption"),
      CTC35 = with_label(.data$ATOXGR %in% c("3", "4", "5"), "Grade 3-5 AE"),
      CTC45 = with_label(.data$ATOXGR %in% c("4", "5"), "Grade 4/5 AE")
    )

  adam_db$adsl <- adam_db$adsl %>%
    mutate(DCSREAS = reformat(.data$DCSREAS, missing_rule))

  adam_db
}

#' @describeIn aet01 Postprocessing
#'
#' @inheritParams gen_args
#' @returns the postprocessing function returns an `rtables` object or an `ElementaryTable` (null report).
#'
#' @export
#'
aet01_post <- function(tlg, prune_0 = FALSE, ...) {
  if (prune_0) {
    tlg <- smart_prune(tlg)
  }
  std_postprocessing(tlg)
}

#' `AET01` Table 1 (Default) Overview of Deaths and Adverse Events Summary Table 1.
#'
#' @include chevron_tlg-S4class.R
#' @export
#'
#' @examples
#' run(aet01, syn_data, arm_var = "ARM")
aet01 <- chevron_t(
  main = aet01_main,
  preprocess = aet01_pre,
  postprocess = aet01_post,
  dataset = c("adsl", "adae")
)
