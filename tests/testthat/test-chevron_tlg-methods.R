# run ----

syn_adv <- syn_data[c("adsl", "adae")]
syn_adv$adae <- syn_adv$adae[syn_adv$adae$AEBODSYS %in% c("cl A.1", "cl B.1", "cl B.2"), ]

test_that("run works as expected for chevron_t object", {
  expect_silent(res <- run(aet04, syn_adv, prune_0 = TRUE))
  expect_snapshot(cat(export_as_txt(res, lpp = 100)))
})

test_that("run works as expected for chevron_t object when auto_pre = FALSE", {
  skip_on_os("windows")
  proc_data <- syn_adv
  proc_data$adsl <- proc_data$adsl %>%
    mutate(DOMAIN = "ADSL")
  res <- run(dmt01, proc_data, auto_pre = FALSE)
  expect_snapshot(cat(export_as_txt(res, lpp = 100)))
})

test_that("run works as expected with argument printed", {
  skip_on_os("windows")
  res <- capture_output(tbl <- run(aet02, syn_adv, prune_0 = TRUE, verbose = TRUE, unwrap = FALSE))

  expect_snapshot(cat(res))
  expect_snapshot(cat(export_as_txt(tbl, lpp = 100)))
})

test_that("run works as expected with argument printed if the user argument is complicated", {
  skip_on_os("windows")
  user_args <- list(prune_0 = TRUE, not_used = iris, lbl_overall = "All Patients", row_split_var = "AEHLT")
  res <- capture_output(tbl <- run(aet02, syn_adv, user_args = user_args, verbose = TRUE, unwrap = FALSE))
  expect_snapshot(cat(res))
  expect_snapshot(cat(export_as_txt(tbl, lpp = 100)))
})

test_that("run uses the argument passed through the ellipsis in priority", {
  skip_on_os("windows")
  user_args <- list(prune_0 = TRUE, not_used = iris, lbl_overall = "All Patients", row_split_var = "AEHLT")
  res <- capture_output(
    tbl <- run(
      aet02,
      syn_adv,
      prune_0 = FALSE,
      another_not_used = iris,
      arm_var = "ARM",
      user_args = user_args,
      verbose = TRUE,
      unwrap = FALSE
    )
  )
  expect_snapshot(cat(res))
  expect_snapshot(cat(export_as_txt(tbl, lpp = 100)))
})

test_that("run works as expected with partial match argument", {
  skip_on_os("windows")
  res <- capture_output(tbl <- run(aet02, syn_adv, prune_0 = TRUE, verbose = TRUE, unwrap = FALSE, arm_var = "ARM"))
  expect_snapshot(cat(res))
  expect_snapshot(cat(export_as_txt(tbl, lpp = 100)))
})

test_that("run displays the symbols when available", {
  skip_on_os("windows")
  user_args <- list(prune_0 = TRUE, not_used = iris, lbl_overall = "All Patients", row_split_var = "AEHLT")
  arm_param <- "ARM"
  res <- capture_output(
    tbl <- run(
      aet02,
      syn_adv,
      prune_0 = FALSE,
      not_used = iris,
      another_not_used = "X",
      arm_var = arm_param,
      user_args = user_args,
      verbose = TRUE,
      unwrap = FALSE
    )
  )
  expect_snapshot(cat(res))
  expect_snapshot(cat(export_as_txt(tbl, lpp = 100)))
})

test_that("run print internal functions when unwrap is TRUE", {
  res <- capture_output(tbl <- run(aet02, syn_adv, prune_0 = TRUE, verbose = TRUE, unwrap = TRUE))

  out <- paste(res, collapse = "\n")
  expect_match(out, "Using template:  aet02")
  expect_match(out, "Main function:")
  expect_match(out, "Layout function:")

  skip_on_covr()
  expect_snapshot(cat(res))
})

test_that("run print internal functions when unwrap is TRUE and standard chevron_tlg has no layout", {
  withr::with_options(opts_partial_match_old, {
    res <- capture_output(tbl <- run(mng01, syn_data, dataset = "adlb", verbose = TRUE, unwrap = TRUE))
  })
  expect_snapshot(cat(res))
})

test_that("run print internal functions when unwrap is TRUE and the chevron_tlg object is customized", {
  custom_chevron <- chevron_t(
    main = function(adam_db, ...) {
      ggplot2::ggplot(adam_db$iris, ggplot2::aes(x = Sepal.Length, y = Sepal.Width)) +
        ggplot2::geom_point()
    }
  )

  res <- capture_output(tbl <- run(custom_chevron, list(iris = iris), verbose = TRUE, unwrap = TRUE))
  out <- paste(res, collapse = "\n")
  expect_match(out, "Using template:  custom_chevron")
  expect_match(out, "Main function:")
  expect_no_match(out, "Layout function:") # no layout

  skip_on_covr()
  expect_snapshot(cat(res))
})

test_that("run print main and postprocessing functions when unwrap is TRUE and auto_pre is FALSE", {
  custom_chevron <- chevron_t(
    main = function(adam_db, ...) {
      ggplot2::ggplot(adam_db$iris, ggplot2::aes(x = Sepal.Length, y = Sepal.Width)) +
        ggplot2::geom_point()
    }
  )
  iris_ls <- list(iris = iris)
  res <- capture_output(tbl <- run(custom_chevron, iris_ls, verbose = FALSE, unwrap = TRUE, auto_pre = FALSE))

  out <- paste(res, collapse = "\n")
  expect_no_match(out, "Using template:  custom_chevron")
  expect_no_match(out, "Preprocessing function:")
  expect_match(out, "Main function:")
  expect_no_match(out, "Layout function:") # no layout

  skip_on_covr()
  expect_snapshot(cat(res))
})

# args_ls ----

test_that("args_ls works as expected", {
  skip_on_os("windows")
  res <- expect_silent(args_ls(aet04))
  expect_list(res, len = 3, names = "named")
  expect_names(names(res), identical.to = c("main", "preprocess", "postprocess"))
})

test_that("args_ls works as expected when simplify is TRUE", {
  skip_on_os("windows")
  res <- expect_silent(args_ls(aet04, simplify = TRUE))
  expect_list(res, len = 7, names = "named")
  expect_names(
    names(res),
    identical.to = c(
      "adam_db", "arm_var", "lbl_overall", "grade_groups", "...", "tlg", "prune_0"
    )
  )
})

test_that("args_ls works as expected with custom chevron_tlg object", {
  skip_on_os("windows")
  obj <- aet04
  preprocess(obj) <- function(adam_db, arm_var = "overwritten", new_arg = "NEW", ...) {
    adam_db
  }

  res <- expect_silent(args_ls(obj, simplify = TRUE))
  expect_list(res, len = 8, names = "named")
  expect_names(
    names(res),
    identical.to = c(
      "adam_db", "arm_var", "lbl_overall", "grade_groups", "...", "new_arg", "tlg", "prune_0"
    )
  )
  expect_identical(res$arm_var, "ACTARM")
})

# main ----

test_that("main works as expected", {
  skip_on_os("windows")
  skip_on_covr()
  res <- main(aet04)
  expect_identical(res, aet04_main)
})

test_that("main setter works as expected", {
  skip_on_os("windows")
  func <- function(adam_db, ...) {
    build_table(basic_table(), adam_db)
  }
  obj <- aet04
  main(obj) <- func
  expect_identical(obj@main, func)
})

test_that("main setter throw an error as expected", {
  skip_on_os("windows")
  func <- function(adam_db) {
    build_table(basic_table(), adam_db)
  }
  obj <- aet04
  expect_error(main(obj) <- func, "Variable 'object@main': Must have formal arguments: ....",
    fixed = TRUE
  )
})

# preprocess ----

test_that("preprocess works as expected", {
  skip_on_os("windows")
  skip_on_covr()
  res <- preprocess(aet04)
  expect_identical(res, aet04_pre)
})

test_that("preprocess setter works as expected", {
  skip_on_os("windows")
  func <- function(adam_db, ...) adam_db
  obj <- aet04
  preprocess(obj) <- func
  expect_identical(obj@preprocess, func)
})

test_that("preprocess sends an error as expected", {
  skip_on_os("windows")
  func <- function(adam_db) adam_db
  obj <- aet04
  expect_error(preprocess(obj) <- func, "Variable 'object@preprocess': Must have formal arguments: ....",
    fixed = TRUE
  )
})

# postprocess ----

test_that("postprocess works as expected", {
  skip_on_os("windows")
  res <- postprocess(aet04)
  expect_identical(res, aet04@postprocess)
})

test_that("postprocess setter works as expected", {
  skip_on_os("windows")
  func <- function(tlg, ...) tlg
  obj <- aet04
  postprocess(obj) <- func
  expect_identical(obj@postprocess, func)
})

test_that("postprocess sends an error as expected", {
  skip_on_os("windows")
  func <- function(tlg) tlg
  obj <- aet04
  expect_error(postprocess(obj) <- func, "Variable 'object@postprocess': Must have formal arguments: ....",
    fixed = TRUE
  )
})

# dataset ----

test_that("dataset works as expected", {
  skip_on_os("windows")
  res <- dataset(aet01)
  expect_identical(res, aet01@dataset)
})

test_that("dataset setter works as expected", {
  skip_on_os("windows")
  obj <- aet01
  dataset(obj) <- "new_dataset"
  expect_identical(obj@dataset, "new_dataset")
})

test_that("dataset sends an error as expected", {
  skip_on_os("windows")
  obj <- aet01
  expect_error(dataset(obj) <- 1, "Must be of type 'character' (or 'NULL')",
    fixed = TRUE
  )
})

# script_funs ----

test_that("script_funs works as expected in interactive mode", {
  skip_on_os("windows")
  skip_if(!interactive())
  res <- expect_silent(script_funs(aet04, adam_db = "data", args = "args_ls"))
  expect_snapshot(res)
})

test_that("script_funs works as expected", {
  skip_on_os("windows")
  res <- expect_silent(script_funs(aet04, adam_db = "data", args = "args_ls"))
  expect_character(res)
})


test_that("script_funs generates a valid script", {
  skip_on_os("windows")
  withr::with_tempfile("tmp", fileext = ".R", {
    args_list <- list(
      arm_var = "ARM"
    )

    res_fun <- script_funs(aet04, adam_db = "syn_adv", args = "args_list")
    writeLines(res_fun, tmp)
    # Creating the object tlg_output in the script.
    res <- capture_output(source(tmp, local = TRUE))
    expected <- run(aet04, syn_adv, arm_var = "ARM")
    expect_identical(tlg_output, expected)

    out <- paste(res, collapse = "\n")
    expect_match(out, "Using template:  aet04")
    expect_match(out, "Main function:")
    expect_match(out, "Layout function:")
    expect_match(out, "aet04_lyt:")

    skip_on_covr()
    expect_snapshot(cat(paste(res, collapse = "\n")))
  })
})

test_that("script_funs works for simple template", {
  skip_on_os("windows")
  res <- expect_silent(script_funs(chevron_simple(), adam_db = "syn_adv"))
  expect_character(res)
})


# print_list ----

test_that("print_list works", {
  expect_snapshot(print_list(alist(a = 1, b = b, c = xx)))
})

test_that("print_list works for empty list", {
  expect_snapshot(print_list(alist()))
})
