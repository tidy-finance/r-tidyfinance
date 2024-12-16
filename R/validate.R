#' @keywords internal
#'
validate_dates <- function(start_date, end_date, use_default_range = FALSE) {
  if (is.null(start_date) || is.null(end_date)) {
    if (use_default_range) {
      start_date <- Sys.Date() %m-% years(2)
      end_date <- Sys.Date() %m-% years(1)
      cli::cli_inform(c(
        "No {.arg start_date} or {.arg end_date} provided.",
        "Using the range {start_date} to {end_date} to avoid downloading large amounts of data."
      ))
      return(list(start_date = start_date, end_date = end_date))
    } else {
      cli::cli_inform(
        "No {.arg start_date} or {.arg end_date} provided. Returning the full data set."
      )
      return(list(start_date = NULL, end_date = NULL))
    }
  } else {
    start_date <- as.Date(start_date)
    end_date <- as.Date(end_date)
    if (start_date > end_date) {
      cli::cli_abort("{.arg start_date} cannot be after {.arg end_date}.")
    }
    return(list(start_date = start_date, end_date = end_date))
  }
}
