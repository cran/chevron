#' General Argument Name Convention
#'
#' @param adam_db (`list` of `data.frames`) object containing the `ADaM` datasets
#' @param main (`function`) returning a `tlg`, with `adam_db` as first argument. Typically one of the `_main` function
#'   of `chevron`.
#' @param preprocess (`function`) returning a pre-processed `list` of `data.frames`, with `adam_db` as first argument.
#'   Typically one of the `_pre` function of `chevron`.
#' @param postprocess (`function`) returning a post-processed `tlg`, with `tlg` as first argument.
#' @param dataset (`string`) the name of a table in the `adam_db` object.
#' @param type (`string`) indicating the subclass.
#' @param arm_var (`string`) variable used for column splitting
#' @param deco (`character`) decoration with `title`, `subtitles` and `main_footer` content
#' @param lbl_overall (`string`) label used for overall column, if set to `NULL` the overall column is omitted
#' @param prune_0 (`flag`) remove 0 count rows
#' @param req_tables (`character`) names of the required tables.
#' @param tlg (`TableTree`, `Listing` or `ggplot`) object typically produced by a `main` function.
#' @param group (`list of lists`) for group-dependent data binning
#' @param visitvar (`string`) typically `"AVISIT"` or user-defined visit incorporating `"ATPT"`.
#' @param paramcd_value Value of `PARAMCD` variable.
#' @param visit_value Value of visit variable.
#' @param key_cols (`character`) names of columns that should be treated as key columns when rendering the listing.
#'   Key columns allow you to group repeat occurrences.
#' @param disp_cols (`character`) names of non-key columns which should be displayed when the listing is rendered.
#' @param row_split_var (`character`) additional row split variables.
#' @param split_into_pages_by_var (`character` or `NULL`) the name of the variable to split the listing by.
#' @param page_var (`string`) variable name prior to which the row split is by page.
#' @param unique_rows (`flag`) whether to keep only unique rows in listing.
#' @param ... not used.
#' @returns invisible `NULL`. This function is for documentation purpose only.
#'
#' @details the following arguments are better provided through the study object: `lbl_overall`, `arm_var`.
#'
gen_args <- function(adam_db, main, preprocess, postprocess, dataset,
                     type, arm_var, lbl_overall, prune_0, req_tables, deco, group,
                     tlg, visitvar, visit_value, paramcd_value,
                     key_cols, disp_cols, row_split_var, split_into_pages_by_var, page_var, unique_rows, ...) {
}
