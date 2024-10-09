## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

library(dplyr)
library(dunlin)
library(chevron)

## ----eval=FALSE---------------------------------------------------------------
#  std_data <- list(adsl = adsl, adae = adae)
#  run(object = aet01_nollt, adam_db = std_data)

## -----------------------------------------------------------------------------
proc_data <- syn_data
proc_data$adsl <- proc_data$adsl %>%
  mutate(RACE = case_when(
    ARMCD == "ARM A" ~ "ASIAN",
    ARMCD == "ARM B" & !.data$RACE %in% c("WHITE", "ASIAN") ~ "ASIAN",
    TRUE ~ RACE
  ))

## ----eval=FALSE---------------------------------------------------------------
#  run(dmt01, proc_data)

## -----------------------------------------------------------------------------
proc_data$adsl$RACE <- as.factor(proc_data$adsl$RACE)
run(dmt01, proc_data)

## ----eval=FALSE---------------------------------------------------------------
#  proc_data <- log_filter(syn_data, PARAMCD == "OS", "adtte")
#  
#  # method 1
#  run(kmg01, proc_data, dataset = "adtte", draw = TRUE)
#  
#  # method 2
#  res <- run(kmg01, proc_data, dataset = "adtte")
#  grid::grid.newpage()
#  grid::grid.draw(res)

## -----------------------------------------------------------------------------
tbl <- run(dmt01, syn_data) # table with column counts
tbl@col_info@display_columncounts <- FALSE
tbl # no column counts now

## -----------------------------------------------------------------------------
run(aet01, syn_data, arm_var = "ARM")

## -----------------------------------------------------------------------------
proc_data <- syn_data
proc_data$adae <- proc_data$adae %>%
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
    CTC45 = with_label(.data$ATOXGR %in% c("4", "5"), "Grade 4/5 AE"),
    RELCTC35 = with_label(.data$ATOXGR %in% c("3", "4", "5") & .data$AEREL == "Y", "Related Grade 3-5")
  )

proc_data$adsl <- proc_data$adsl %>%
  mutate(DCSREAS = reformat(.data$DCSREAS, missing_rule))

run(aet01, proc_data, anl_vars = list(safety_var = c("FATAL", "SER", "RELSER", "RELCTC35")), auto_pre = FALSE)

## -----------------------------------------------------------------------------
run(aet01_aesi, syn_data)

## -----------------------------------------------------------------------------
run(aet01_aesi, syn_data, aesi_vars = c("RESLWD", "RELSER"))

## -----------------------------------------------------------------------------
run(aet02, syn_data)

## -----------------------------------------------------------------------------
run(aet02, syn_data, row_split_var = c("AEBODSYS", "AEHLT"))

## -----------------------------------------------------------------------------
run(aet02, syn_data, row_split_var = NULL)

## -----------------------------------------------------------------------------
run(aet03, syn_data)

## -----------------------------------------------------------------------------
run(aet04, syn_data)

## -----------------------------------------------------------------------------
run(aet04, syn_data, prune_0 = FALSE)

## -----------------------------------------------------------------------------
grade_groups <- list(
  "Grade 1-2" = c("1", "2"),
  "Grade 3-5" = c("3", "4", "5")
)

run(aet04, syn_data, grade_groups = grade_groups, prune_0 = FALSE)

## -----------------------------------------------------------------------------
proc_data <- log_filter(syn_data, PARAMCD == "AETTE1", "adsaftte")

run(aet05, proc_data)

## -----------------------------------------------------------------------------
run(aet05, syn_data, conf_level = 0.90, conf_type = "exact")

## -----------------------------------------------------------------------------
proc_data <- log_filter(syn_data, PARAMCD == "AETOT1" | PARAMCD == "AEREPTTE", "adsaftte")

run(aet05_all, proc_data)

## -----------------------------------------------------------------------------
run(aet05_all, syn_data, conf_level = 0.90, conf_type = "exact")

## -----------------------------------------------------------------------------
run(aet10, syn_data)

## -----------------------------------------------------------------------------
run(aet10, syn_data, atleast = 0.08)

## -----------------------------------------------------------------------------
run(cmt01a, syn_data)

## -----------------------------------------------------------------------------
run(cmt01a, syn_data, row_split_var = "ATC1")

## -----------------------------------------------------------------------------
run(cmt01a, syn_data, sort_by_freq = TRUE)

## -----------------------------------------------------------------------------
run(cmt01a, syn_data, summary_labels = list(TOTAL = cmt01_label, ATC2 = cmt01_label[1]))

## -----------------------------------------------------------------------------
run(cmt02_pt, syn_data)

## -----------------------------------------------------------------------------
proc_data <- log_filter(syn_data, PARAMCD == "OS", "adtte")
proc_data <- log_filter(proc_data, ARMCD != "ARM C", "adsl")
run(coxt01, proc_data, time_var = "AVAL", event_var = "EVENT")

## -----------------------------------------------------------------------------
run(coxt01, proc_data, covariates = "AAGE", interaction = TRUE)

## -----------------------------------------------------------------------------
run(coxt01, proc_data, covariates = c("RACE", "AAGE"))

## -----------------------------------------------------------------------------
run(coxt01, proc_data, covariates = c("SEX", "AAGE"), strata = c("RACE"), conf_level = 0.90)

## -----------------------------------------------------------------------------
proc_data <- log_filter(syn_data, PARAMCD == "OS", "adtte")
run(coxt02, proc_data, time_var = "AVAL", event_var = "EVENT")

## -----------------------------------------------------------------------------
run(coxt02, proc_data, covariates = c("RACE", "AAGE"))

## -----------------------------------------------------------------------------
run(coxt02, proc_data, covariates = c("SEX", "AAGE"), strata = c("RACE"), conf_level = 0.90, ties = "efron")

## -----------------------------------------------------------------------------
run(dmt01, syn_data)

## -----------------------------------------------------------------------------
run(dmt01, syn_data, lbl_overall = NULL)

## -----------------------------------------------------------------------------
run(dmt01, syn_data, summaryvars = c("AGE", "AGEGR1", "SEX", "ETHNIC", "RACE", "BBMISI"), lbl_overall = NULL)

## -----------------------------------------------------------------------------
proc_data <- syn_data
proc_data$adsl <- proc_data$adsl %>%
  mutate(
    SEX = reformat(.data$SEX, rule(Male = "M", Female = "F")),
    BBMIGR1 = factor(case_when(
      BBMISI < 15 ~ "Very severely underweight",
      BBMISI >= 15 & BBMISI < 16 ~ "Severely underweight",
      BBMISI >= 16 & BBMISI < 18.5 ~ "Underweight",
      BBMISI >= 18.5 & BBMISI < 25 ~ "Normal (healthy weight)",
      BBMISI >= 25 & BBMISI < 30 ~ "Overweight",
      BBMISI >= 30 & BBMISI < 35 ~ "Obese Class I (Moderately obese)",
      BBMISI >= 35 & BBMISI < 40 ~ "Obese Class II (Severely obese)",
      BBMISI >= 40 ~ "Obese Class III (Very severely obese)"
    ), levels = c(
      "Very severely underweight",
      "Severely underweight",
      "Underweight",
      "Normal (healthy weight)",
      "Overweight",
      "Obese Class I (Moderately obese)",
      "Obese Class II (Severely obese)",
      "Obese Class III (Very severely obese)"
    ))
  )

run(dmt01, proc_data, summaryvars = c("AGE", "AGEGR1", "SEX", "ETHNIC", "RACE", "BBMIGR1"), auto_pre = FALSE)

## -----------------------------------------------------------------------------
proc_data <- syn_data
diabpbl <- proc_data$advs %>%
  filter(ABLFL == "Y" & PARAMCD == "DIABP") %>%
  mutate(DIABPBL = AVAL) %>%
  select("STUDYID", "USUBJID", "DIABPBL")

proc_data$adsl <- proc_data$adsl %>%
  mutate(SEX = reformat(.data$SEX, rule(Male = "M", Female = "F"))) %>%
  left_join(diabpbl, by = c("STUDYID", "USUBJID"))

run(dmt01, proc_data, summaryvars = c("AGE", "AGEGR1", "SEX", "ETHNIC", "RACE", "DIABPBL"), auto_pre = FALSE)

## -----------------------------------------------------------------------------
run(dst01, syn_data, lbl_overall = NULL)

## -----------------------------------------------------------------------------
run(dst01, syn_data, detail_vars = list(Discontinued = c("DCSREASGP", "DCSREAS")), lbl_overall = NULL)

## -----------------------------------------------------------------------------
run(dst01, syn_data, trt_status_var = "EOTSTT", lbl_overall = NULL)

## -----------------------------------------------------------------------------
run(dst01, syn_data, detail_vars = list(Discontinued = "DCSREAS", Ongoing = "STDONS"))

## -----------------------------------------------------------------------------
run(dst01, syn_data)

## -----------------------------------------------------------------------------
run(dtht01, syn_data, other_category = TRUE)

## -----------------------------------------------------------------------------
run(dtht01, syn_data, time_since_last_dose = TRUE)

## -----------------------------------------------------------------------------
run(egt01, syn_data)

## -----------------------------------------------------------------------------
run(egt02_1, syn_data)

## -----------------------------------------------------------------------------
run(egt02_2, syn_data)

## -----------------------------------------------------------------------------
proc_data <- log_filter(syn_data, PARAMCD == "HR", "adeg")
run(egt03, proc_data)

## -----------------------------------------------------------------------------
run(egt05_qtcat, syn_data)

## -----------------------------------------------------------------------------
run(egt05_qtcat, syn_data, summaryvars = c("AVALCAT1"))

## -----------------------------------------------------------------------------
run(ext01, syn_data)

## -----------------------------------------------------------------------------
t_lb_chg <- run(lbt01, syn_data)
head(t_lb_chg, 20)

## -----------------------------------------------------------------------------
run(lbt04, syn_data)

## -----------------------------------------------------------------------------
run(lbt05, syn_data)

## -----------------------------------------------------------------------------
run(lbt06, syn_data)

## -----------------------------------------------------------------------------
run(lbt07, syn_data)

## -----------------------------------------------------------------------------
run(lbt14, syn_data, direction = "high")

## -----------------------------------------------------------------------------
run(lbt14, syn_data)

## -----------------------------------------------------------------------------
run(lbt14, syn_data, direction = "high", gr_missing = "excl")

## -----------------------------------------------------------------------------
run(lbt14, syn_data, gr_missing = "gr_0")

## -----------------------------------------------------------------------------
run(lbt14, syn_data, direction = "high", prune_0 = FALSE)

## -----------------------------------------------------------------------------
run(mht01, syn_data)

## -----------------------------------------------------------------------------
run(mht01, syn_data, lbl_overall = "All Patients")

## -----------------------------------------------------------------------------
proc_data <- syn_data
proc_data$addv <- proc_data$addv %>%
  filter(DVCAT == "MAJOR")

run(pdt01, proc_data)

## -----------------------------------------------------------------------------
run(pdt02, syn_data)

## -----------------------------------------------------------------------------
run(rmpt01, syn_data)

## -----------------------------------------------------------------------------
proc_data <- syn_data
proc_data <- propagate(proc_data, "adsl", "AGEGR1", "USUBJID")
run(rmpt03, proc_data)

## -----------------------------------------------------------------------------
proc_data <- syn_data
proc_data$adsl <- proc_data$adsl %>%
  mutate(
    AGEGR2 = with_label(
      factor(case_when(
        AAGE < 18 ~ "<18",
        AAGE >= 18 & AAGE <= 65 ~ "18 - 65",
        AAGE > 65 ~ ">65",
      ), levels = c("<18", "18 - 65", ">65")),
      "Age Group 2"
    )
  )
proc_data <- propagate(proc_data, "adsl", "AGEGR2", "USUBJID")
run(rmpt03, proc_data, summaryvars = "AGEGR2")

## -----------------------------------------------------------------------------
run(rmpt04, syn_data)

## -----------------------------------------------------------------------------
run(rmpt05, syn_data)

## -----------------------------------------------------------------------------
proc_data <- log_filter(syn_data, PARAMCD == "BESRSPI", "adrs")

run(rspt01, proc_data, ref_group = NULL, perform_analysis = "unstrat", strata = NULL)

## -----------------------------------------------------------------------------
proc_data <- log_filter(syn_data, PARAMCD == "BESRSPI", "adrs")

run(rspt01, proc_data, odds_ratio = FALSE, perform_analysis = NULL)

## -----------------------------------------------------------------------------
proc_data <- log_filter(syn_data, PARAMCD == "BESRSPI", "adrs")

run(rspt01, proc_data, perform_analysis = "strat", strata = c("STRATA1", "STRATA2"))

## -----------------------------------------------------------------------------
proc_data <- log_filter(syn_data, PARAMCD == "BESRSPI", "adrs")

run(rspt01, proc_data,
  conf_level = 0.90,
  methods = list(
    prop_conf_method = "wald",
    diff_conf_method = "wald",
    diff_pval_method = "fisher"
  )
)

## -----------------------------------------------------------------------------
proc_data <- log_filter(syn_data, PARAMCD == "BESRSPI", "adrs")

preprocess(rspt01) <- function(adam_db, ...) {
  adam_db$adrs <- adam_db$adrs %>%
    mutate(RSP_LAB = tern::d_onco_rsp_label(.data$AVALC)) %>%
    mutate(IS_RSP = .data$AVALC %in% c("CR"))
  adam_db
}

run(rspt01, proc_data)

## -----------------------------------------------------------------------------
proc_data <- log_filter(syn_data, PARAMCD == "PFS", "adtte")

run(ttet01, proc_data)

## -----------------------------------------------------------------------------
proc_data <- log_filter(syn_data, PARAMCD == "PFS", "adtte")

run(ttet01, proc_data, summarize_event = FALSE)

## -----------------------------------------------------------------------------
proc_data <- log_filter(syn_data, PARAMCD == "PFS", "adtte")

run(ttet01, proc_data, method = "surv")

## -----------------------------------------------------------------------------
proc_data <- log_filter(syn_data, PARAMCD == "PFS", "adtte")

run(ttet01, proc_data, conf_level = 0.90, conf_type = "log-log", ties = "efron")

## -----------------------------------------------------------------------------
proc_data <- log_filter(syn_data, PARAMCD == "PFS", "adtte")

run(ttet01, proc_data, perform_analysis = "strat", strata = "STRATA1")

## -----------------------------------------------------------------------------
proc_data <- log_filter(syn_data, PARAMCD == "PFS", "adtte")

run(ttet01, proc_data, perform_analysis = "unstrat", time_point = c(3, 6))

## -----------------------------------------------------------------------------
proc_data <- log_filter(syn_data, PARAMCD == "PFS", "adtte")

preprocess(ttet01) <- function(adam_db, dataset = "adtte",
                               ...) {
  adam_db[[dataset]] <- adam_db[[dataset]] %>%
    mutate(
      AVALU = "DAYS",
      IS_EVENT = .data$CNSR == 0,
      IS_NOT_EVENT = .data$CNSR == 1,
      EVNT1 = factor(
        case_when(
          IS_EVENT == TRUE ~ render_safe("{Patient_label} with event (%)"),
          IS_EVENT == FALSE ~ render_safe("{Patient_label} without event (%)")
        ),
        levels = render_safe(c("{Patient_label} with event (%)", "{Patient_label} without event (%)"))
      ),
      EVNTDESC = factor(.data$EVNTDESC)
    )

  adam_db
}

run(ttet01, proc_data, perform_analysis = "unstrat", time_point = c(91, 183))

## -----------------------------------------------------------------------------
proc_data <- log_filter(syn_data, PARAMCD == "PFS", "adtte")

run(ttet01, proc_data, pval_method = "wald")

## -----------------------------------------------------------------------------
t_vs_chg <- run(vst01, syn_data)
head(t_vs_chg, 20)

## -----------------------------------------------------------------------------
run(vst02_1, syn_data)

## -----------------------------------------------------------------------------
run(vst02_2, syn_data)

## -----------------------------------------------------------------------------
l_ae_nollt <- run(ael01_nollt, syn_data)
head(l_ae_nollt, 10)

## ----fig.width = 10, fig.height = 6-------------------------------------------
proc_data <- log_filter(
  syn_data,
  PARAMCD == "BESRSPI" & ARM %in% c("A: Drug X", "B: Placebo"), "adrs"
)
run(fstg01, proc_data)

## ----fig.width = 10, fig.height = 6-------------------------------------------
run(fstg01, proc_data, conf_level = 0.90)

## ----fig.width = 10, fig.height = 6-------------------------------------------
run(fstg01, proc_data, method = "fisher", stat_var = c("n_tot", "n", "ci", "or", "pval"))

## ----fig.width = 10, fig.height = 6-------------------------------------------
run(fstg01, proc_data, subgroups = NULL)

## ----fig.width = 10, fig.height = 6-------------------------------------------
run(fstg01, proc_data, strata_var = "STRATA1")

## ----fig.width = 10, fig.height = 6-------------------------------------------
run(fstg01, proc_data, col_symbol_size = NULL)

## ----fig.width = 10, fig.height = 6-------------------------------------------
proc_data <- log_filter(
  syn_data,
  PARAMCD == "OS" & ARM %in% c("A: Drug X", "B: Placebo"), "adtte"
)
run(fstg02, proc_data)

## ----fig.width = 10, fig.height = 6-------------------------------------------
run(
  fstg02,
  proc_data,
  stat_var = c("n_tot", "n", "ci", "hr", "pval"),
  control = list(conf_level = 0.9, pval_method = "likelihood")
)

## ----fig.width = 10, fig.height = 6-------------------------------------------
run(fstg02, proc_data, subgroups = NULL)

## ----fig.width = 10, fig.height = 6-------------------------------------------
run(fstg02, proc_data, strata_var = "STRATA1")

## ----fig.width = 10, fig.height = 6-------------------------------------------
run(fstg02, proc_data, col_symbol_size = NULL)

## ----fig.width = 10, fig.height = 6-------------------------------------------
proc_data <- log_filter(syn_data, PARAMCD == "OS", "adtte")
run(kmg01, proc_data, dataset = "adtte")

## ----fig.width = 10, fig.height = 6-------------------------------------------
proc_data <- log_filter(syn_data, PARAMCD == "OS", "adtte")
run(
  kmg01,
  proc_data,
  dataset = "adtte",
  annot_coxph = TRUE,
  control_annot_coxph = tern::control_coxph_annot(x = 0.33, y = 0.42)
)

## ----fig.width = 10, fig.height = 6-------------------------------------------
proc_data <- log_filter(syn_data, PARAMCD == "OS", "adtte")
run(kmg01, proc_data, dataset = "adtte", censor_show = FALSE)

## ----fig.width = 10, fig.height = 6-------------------------------------------
proc_data <- log_filter(syn_data, PARAMCD == "OS", "adtte")
run(kmg01, proc_data, dataset = "adtte", annot_surv_med = FALSE)

## ----fig.width = 10, fig.height = 6-------------------------------------------
proc_data <- log_filter(syn_data, PARAMCD == "OS", "adtte")
run(kmg01, proc_data, dataset = "adtte", annot_stats = "median")
run(kmg01, proc_data, dataset = "adtte", annot_stats = c("min", "median"))

## ----fig.width = 10, fig.height = 6-------------------------------------------
proc_data <- log_filter(syn_data, PARAMCD == "OS", "adtte")
run(kmg01, proc_data, dataset = "adtte", annot_at_risk = FALSE)

## ----fig.height = 8-----------------------------------------------------------
proc_data <- log_filter(syn_data, PARAMCD == "DIABP", "advs")
run(mng01, proc_data, dataset = "advs", x_var = c("AVISIT", "AVISITN"))

## ----fig.height = 8-----------------------------------------------------------
proc_data <- log_filter(syn_data, PARAMCD == "DIABP", "advs")
run(mng01, proc_data, dataset = "advs", x_var = c("AVISIT", "AVISITN"), y_var = "CHG")

## ----fig.height = 8-----------------------------------------------------------
proc_data <- log_filter(syn_data, PARAMCD == "DIABP", "advs")
run(mng01, proc_data, dataset = "advs", x_var = c("AVISIT", "AVISITN"), interval_fun = "mean_sdi")

## ----fig.height = 8-----------------------------------------------------------
proc_data <- log_filter(syn_data, PARAMCD == "DIABP", "advs")
run(
  mng01, proc_data,
  dataset = "advs", x_var = c("AVISIT", "AVISITN"),
  interval_fun = "mean_ci", control = tern::control_analyze_vars(conf_level = 0.80)
)

## ----fig.height = 8-----------------------------------------------------------
proc_data <- log_filter(syn_data, PARAMCD == "DIABP", "advs")
run(mng01, proc_data, dataset = "advs", x_var = c("AVISIT", "AVISITN"), table = "n")

## ----fig.height = 8-----------------------------------------------------------
proc_data <- log_filter(syn_data, PARAMCD == "DIABP", "advs")
run(mng01, proc_data, dataset = "advs", x_var = c("AVISIT", "AVISITN"), table = NULL)

## ----fig.height = 8-----------------------------------------------------------
ggtheme <- ggplot2::theme(
  panel.grid = ggplot2::element_line(colour = "black", linetype = 3),
  panel.background = ggplot2::element_rect(fill = "white"),
  legend.position = "top",
  axis.text.x = ggplot2::element_text(angle = 22, hjust = 1, vjust = 1)
)
run(mng01, syn_data, dataset = "adlb", ggtheme = ggtheme)

