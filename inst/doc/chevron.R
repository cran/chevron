## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(chevron)
data(syn_data, package = "chevron")

## -----------------------------------------------------------------------------
run(aet02, syn_data)

## -----------------------------------------------------------------------------
main(aet02)

## -----------------------------------------------------------------------------
res <- preprocess(aet02)(syn_data)

# or
foo <- aet02@preprocess
res <- foo(syn_data)

str(res, max.level = 0)

## ----eval = FALSE-------------------------------------------------------------
#  library(rtables)
#  library(tern)
#  my_template <- chevron_t(
#    main = "<your main function to build the table>",
#    preprocess = "<your pre function to process the data>",
#    postprocess = "<your post function to add custom sorting>"
#  )
#  
#  run(my_template, syn_data)

