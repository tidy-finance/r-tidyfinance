#' Validate a Single Column-Name Argument
#'
#' Aborts when `value` is not a length-one character vector. Used by
#' [data_options()] to validate the column-name mappings.
#'
#' @param value The argument value to validate.
#' @param arg A single string naming the argument, used in the error message.
#' @param description A short noun phrase describing the variable, inserted into
#'   the error message (e.g., `"entity"`, `"market capitalization lag"`).
#'
#' @returns Invisibly returns `NULL`; called for its side effect of aborting on
#'   invalid input.
#' @noRd
validate_column_name <- function(value, arg, description) {
  if (!is.character(value) || length(value) != 1) {
    cli::cli_abort(
      paste0(
        "{.arg {arg}} must be a string indicating the column name ",
        "for the {description} variable."
      ),
      call = rlang::caller_env()
    )
  }
  invisible(NULL)
}

#' Validate a Single Logical (Flag) Argument
#'
#' Aborts when `value` is not a single, non-`NA` logical. Used by the
#' `*_options()` constructors for their `TRUE`/`FALSE` switches.
#'
#' @param value The argument value to validate.
#' @param arg A single string naming the argument, used in the default message.
#' @param message Optional replacement for the default error message.
#'
#' @returns Invisibly returns `NULL`; called for its side effect of aborting on
#'   invalid input.
#' @noRd
validate_flag <- function(value, arg, message = NULL) {
  if (!is.logical(value) || length(value) != 1 || is.na(value)) {
    if (is.null(message)) {
      message <- "{.arg {arg}} must be a single logical."
    }
    cli::cli_abort(message, call = rlang::caller_env())
  }
  invisible(NULL)
}

#' Validate an Optional Single Numeric Argument Within Bounds
#'
#' When `value` is `NULL` the check passes. Otherwise aborts unless `value` is a
#' single, non-`NA` numeric satisfying the (optionally strict) lower and upper
#' bounds.
#'
#' @param value The argument value to validate (or `NULL`).
#' @param message The error message to emit on invalid input.
#' @param min,max Numeric lower and upper bounds. Default to `-Inf`/`Inf`.
#' @param min_strict,max_strict Logical; whether the corresponding bound is
#'   exclusive (`>`/`<`) rather than inclusive (`>=`/`<=`).
#'
#' @returns Invisibly returns `NULL`; called for its side effect of aborting on
#'   invalid input.
#' @noRd
validate_optional_number <- function(
  value,
  message,
  min = -Inf,
  max = Inf,
  min_strict = FALSE,
  max_strict = FALSE
) {
  if (is.null(value)) {
    return(invisible(NULL))
  }
  ok <- is.numeric(value) &&
    length(value) == 1 &&
    !is.na(value) &&
    (if (min_strict) value > min else value >= min) &&
    (if (max_strict) value < max else value <= max)
  if (!ok) {
    cli::cli_abort(message, call = rlang::caller_env())
  }
  invisible(NULL)
}
