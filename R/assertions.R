# assert_single_value ----

#' Check variable only has one unique value.
#' @param x value vector.
#' @param label (`string`) label of input.
#' @returns invisible `NULL` or an error message if the criteria are not fulfilled.
#' @export
assert_single_value <- function(x, label = deparse(substitute(x))) {
  unique_param_val <- unique(x)
  if (length(unique_param_val) > 1) {
    stop(
      quote_str(label),
      " has more than one values ",
      toString(unique_param_val),
      ", only one value is allowed."
    )
  }
}

# assert_valid_var ----

#' @title Check whether var is valid
#' @details
#' This function checks the variable values are valid or not.
#' @param x value of col_split variable
#' @param label (`string`) hints.
#' @param na_ok (`flag`) whether NA value is allowed
#' @param empty_ok (`flag`) whether length 0 value is allowed.
#' @param ... Further arguments to methods.
#' @returns invisible `NULL` or an error message if the criteria are not fulfilled.
#' @export
assert_valid_var <- function(x, label, na_ok, empty_ok, ...) {
  UseMethod("assert_valid_var")
}
#' @rdname assert_valid_var
#' @export
#' @param min_chars (`integer`) the minimum length of the characters.
assert_valid_var.character <- function(
    x, label = deparse(substitute(x)),
    na_ok = FALSE, empty_ok = FALSE,
    min_chars = 1L, ...) {
  assert_character(
    x,
    min.chars = min_chars,
    min.len = as.integer(!empty_ok),
    any.missing = na_ok,
    .var.name = label,
    ...
  )
}

#' @rdname assert_valid_var
#' @export
assert_valid_var.factor <- function(
    x, label = deparse(substitute(x)),
    na_ok = FALSE, empty_ok = FALSE,
    min_chars = 1L, ...) {
  assert_character(
    levels(x),
    min.chars = min_chars,
    .var.name = paste("level of", label)
  )
  assert_factor(
    x,
    min.levels = as.integer(!empty_ok),
    any.missing = na_ok,
    .var.name = label,
    ...
  )
}

#' @rdname assert_valid_var
#' @export
assert_valid_var.logical <- function(x, label = deparse(substitute(x)), na_ok = TRUE, empty_ok = FALSE, ...) {
  assert_logical(
    x,
    min.len = as.integer(!empty_ok),
    any.missing = na_ok,
    .var.name = label,
    ...
  )
}

#' @rdname assert_valid_var
#' @param integerish (`flag`) whether the number should be treated as `integerish`.
#' @export
assert_valid_var.numeric <- function(
    x, label = deparse(substitute(x)),
    na_ok = TRUE, empty_ok = FALSE, integerish = FALSE, ...) {
  check_fun <- if (integerish) assert_integerish else assert_numeric
  check_fun(
    x,
    min.len = as.integer(!empty_ok),
    any.missing = na_ok,
    .var.name = label,
    ...
  )
}

#' @rdname assert_valid_var
#' @param tzs (`character`) time zones.
#' @export
assert_valid_var.POSIXct <- function(x,
                                     label = deparse(substitute(x)),
                                     na_ok = TRUE,
                                     empty_ok = FALSE,
                                     tzs = OlsonNames(),
                                     ...) {
  assert_posixct(
    x,
    min.len = as.integer(!empty_ok),
    any.missing = na_ok,
    .var.name = label,
    ...
  )

  extra_args <- list(...)

  # Test if time zone of x is in OlsonNames
  if (lubridate::tz(x) %in% tzs) {
    return(invisible(NULL))
  } else if (is(extra_args$add, "AssertCollection")) {
    extra_args$add$push(paste("Non standard timezone detected for", label, "!"))
  } else {
    abort(paste("Non standard timezone detected for", label, "!"))
  }
}

#' @rdname assert_valid_var
#' @export
assert_valid_var.default <- function(x, label = deparse(substitute(x)), na_ok = FALSE, empty_ok = FALSE, ...) {
}

# assert_valid_variable ----

#' Check variables in a data frame are valid character or factor.
#' @param df (`data.frame`) input dataset.
#' @param vars (`character`) variables to check.
#' @param label (`string`) labels of the data frame.
#' @param types Named (`list`) of type of the input.
#' @param ... further arguments for `assert_valid_var`. Please note that different methods have different arguments
#' so if provided make sure the variables to check is of the same class.
#' @returns invisible `TRUE` or an error message if the criteria are not fulfilled.
#' @export
assert_valid_variable <- function(df, vars, label = deparse(substitute(df)), types = NULL, ...) {
  assert_names(colnames(df), must.include = vars, what = "colnames")

  labels <- sprintf("%s$%s", label, vars)
  if (length(types) == 1 && is.null(names(types))) {
    types <- setNames(rep(types, length(vars)), vars)
  }
  if (!is.null(types)) {
    vars_to_check <- which(vars %in% names(types))
    mapply(
      assert_valid_type,
      df[vars[vars_to_check]],
      types = types[vars_to_check],
      label = labels[vars_to_check]
    )
  }
  collection <- makeAssertCollection()
  mapply(assert_valid_var, df[vars], labels, MoreArgs = list(..., add = collection), SIMPLIFY = FALSE)
  reportAssertions(collection)
}

# assert_valid_type ----

#' Check variable is of correct type
#' @param x Object to check the type.
#' @param types (`character`) possible types to check.
#' @param label (`string`) label.
#' @returns invisible `NULL` or an error message if the criteria are not fulfilled.
assert_valid_type <- function(x, types, label = deparse(substitute(x))) {
  if (!any(vapply(types, is, object = x, FUN.VALUE = TRUE))) {
    abort(
      paste0(
        quote_str(label),
        " is not of type ",
        toString(types)
      )
    )
  }
}

# assert_valid_var_pair ----

#' Check variables are of same levels
#' @param df1 (`data.frame`) input.
#' @param df2 (`data.frame`) input.
#' @param var (`string`) variable to check.
#' @param lab1 (`string`) label hint for `df1`.
#' @param lab2 (`string`) label hint for `df2`.
#' @returns invisible `NULL` or an error message if the criteria are not fulfilled.
assert_valid_var_pair <- function(df1, df2, var, lab1 = deparse(substitute(df1)), lab2 = deparse(substitute(df2))) {
  assert_data_frame(df1)
  assert_data_frame(df2)
  assert_string(var)
  lvl_x <- lvls(df1[[var]])
  lvl_y <- lvls(df2[[var]])
  if (!identical(lvl_x, lvl_y)) {
    abort(
      paste0(
        quote_str(lab1), " and ",
        quote_str(lab2), " should contain the same levels in variable ",
        quote_str(var), "!"
      )
    )
  }
}
