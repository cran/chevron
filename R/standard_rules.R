#' No Coding Available rule
#' @export
nocoding <- rule("No Coding Available" = c("", NA))

#' Missing rule
#' @export
missing_rule <- rule("<Missing>" = c("", NA), .drop = TRUE)

#' Empty rule
#' @export
empty_rule <- rule(.to_NA = "")

#' Yes/No rule in title case
#' @export
yes_no_rule <- rule("Yes" = c("Y", "YES", "y", "yes"), "No" = c("N", "NO", "n", "no")) # nolint

#' Outcome Rule
#' @export
outcome_rule <- rule(
  "1" = "FATAL",
  "2" = "NOT RECOVERED/NOT RESOLVED",
  "3" = "RECOVERED/RESOLVED",
  "4" = "RECOVERED/RESOLVED WITH SEQUELAE",
  "5" = "RECOVERING/RESOLVING",
  "6" = "UNKNOWN"
)

#' Dose Change Rule
#' @export
dose_change_rule <- rule(
  "1" = "DOSE INCREASED",
  "2" =  "DOSE NOT CHANGED",
  "3" = c("DOSE REDUCED", "DOSE RATE REDUCED"),
  "4" =  "DRUG INTERRUPTED",
  "5" = "DRUG WITHDRAWN",
  "6" = c("NOT APPLICABLE", "NOT EVALUABLE"),
  "7" = "UNKNOWN"
)

#' Get grade rule
#' @param direction (`string`) of abnormality direction.
#' @param missing (`string`) method to deal with missing
#' @returns a `rule` object.
#' @export
get_grade_rule <- function(direction = "high", missing = "incl") {
  assert_choice(direction, c("high", "low"))
  assert_choice(missing, c("incl", "gr_0", "excl"))
  rule_arg <- list()
  if (direction == "high") {
    rule_arg[["Not High"]] <- c("0", "-1", "-2", "-3", "-4")
    rule_arg[as.character(1:4)] <- as.character(1:4)
  } else {
    rule_arg[["Not Low"]] <- c("0", "1", "2", "3", "4")
    rule_arg[as.character(1:4)] <- as.character(-1:-4)
  }
  if (missing == "incl") {
    rule_arg$Missing <- c(NA, "", "<Missing>")
  } else if (missing == "gr_0") {
    rule_arg[[1]] <- c(rule_arg[[1]], NA, "")
  }
  rule(.lst = rule_arg)
}
