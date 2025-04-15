## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

library(chevron)
library(dplyr)

## -----------------------------------------------------------------------------
res <- script_funs(aet01, adam_db = "syn_data", args = "args_list")
writeLines(res)

## -----------------------------------------------------------------------------
aet01_custom <- aet01
preprocess(aet01_custom) <- function(adam_db, new_format, ...) {
  reformat(adam_db, new_format)
}

res_funs <- script_funs(aet01_custom, adam_db = "syn_data", args = "args_list")

## -----------------------------------------------------------------------------
writeLines(res_funs)

