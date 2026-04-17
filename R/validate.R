#' Validate and Coerce Date Range Arguments
#'
#' Checks that `start_date` and `end_date` are a valid pair, coerces them to
#' `Date`, and handles the case where both are `NULL`. When both are `NULL` and
#' `use_default_range = TRUE`, a two-year default window ending one year ago is
#' applied and reported to the user.
#'
#' @param start_date A scalar coercible to `Date` via `as.Date()`, or `NULL`.
#' @param end_date A scalar coercible to `Date` via `as.Date()`, or `NULL`.
#' @param use_default_range A logical scalar. If `TRUE` and both date arguments
#'   are `NULL`, a default one-year range is used instead of returning
#'   `NULL`. Defaults to `FALSE`.
#'
#' @returns A named list with elements `start_date` and `end_date`, both of
#'   class `Date` (or `NULL` when no dates are provided and
#'   `use_default_range = FALSE`).
#'
#' @keywords internal
#' @family utility functions
#'
validate_dates <- function(start_date, end_date, use_default_range = FALSE) {
  if (is.null(start_date) || is.null(end_date)) {
    if (use_default_range) {
      start_date <- Sys.Date() %m-% years(2)
      end_date <- Sys.Date() %m-% years(1)
      cli::cli_inform(c(
        paste(
          "No {.arg start_date} or {.arg end_date} provided.",
          "Using the range {start_date} to {end_date} to avoid",
          "downloading large amounts of data."
        )
      ))
      list(start_date = start_date, end_date = end_date)
    } else {
      cli::cli_inform(
        paste(
          "No {.arg start_date} or {.arg end_date} provided.",
          "Returning the full data set."
        )
      )
      list(start_date = NULL, end_date = NULL)
    }
  } else {
    start_date <- as.Date(start_date)
    end_date <- as.Date(end_date)
    if (start_date > end_date) {
      cli::cli_abort("{.arg start_date} cannot be after {.arg end_date}.")
    }
    list(start_date = start_date, end_date = end_date)
  }
}
