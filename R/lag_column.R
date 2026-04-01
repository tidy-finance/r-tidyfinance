#' Lag a Column Based on Date and Time Range
#'
#' @description `r lifecycle::badge('experimental')`
#'
#' Generates a lagged version of a given column based on a date variable, with the
#' ability to specify a range of lags. It also allows for the optional removal of `NA` values.
#'
#' @param column A numeric vector or column to be lagged.
#' @param date A vector representing dates corresponding to the `column`. The column should be in a
#'  date or datetime format.
#' @param lag An integer or an `lubridate::periods()` object, e.g., `month(1)`, specifying the minimum lag (inclusive) to apply to `column`.
#' @param max_lag An integer or an `lubridate::periods()` object specifying the maximum lag (inclusive) to apply to `column`.
#'  Defaults to `lag`.
#' @param drop_na A logical value indicating whether to drop `NA` values from the resulting lagged
#'  column. Defaults to `FALSE`.
#' @param ff_adjustment A logical value indicating whether to lag monthly data based on Fama-French conventions. Here, the values are lagged based on the last observation within the year is taken. Defaults to `FALSE`.
#' @returns A vector of the same length as `column`, containing the lagged values.
#'  If no matching dates are found within the lag window, `NA` is returned for that position.
#'
#' @family rolling and lagging functions
#' @export
#'
#' @examples
#' set.seed(42)
#' # Basic example with a vector
#' dates <- as.Date("2023-01-01") + 0:9
#' values <- rnorm(10)
#' lagged_values <- lag_column(values, dates, lag = 1, max_lag = 3)
#'
#' # Example using a tibble and dplyr::group_by
#' data <- tibble::tibble(
#'   permno = rep(1:2, each = 10),
#'   date = rep(seq.Date(as.Date('2023-01-01'), by = "month", length.out = 10), 2),
#'   size = runif(20, 100, 200),
#'   bm = runif(20, 0.5, 1.5)
#' )
#'
#' data |>
#'  dplyr::group_by(permno) |>
#'  dplyr::mutate(
#'    across(c(size, bm), \(x) {
#'      lag_column(x, date, months(3), months(6), drop_na = TRUE)
#'    })
#'  ) |>
#'  dplyr::ungroup()
#'
lag_column <- function(
  column,
  date,
  lag,
  max_lag = lag,
  drop_na = FALSE,
  ff_adjustment = FALSE
) {
  if (lag < 0 || max_lag < lag) {
    cli::cli_abort(
      "{.arg lag} and {.arg max_lag} must be non-negative and {.arg max_lag} must be >= {.arg lag}"
    )
  }
  if (anyDuplicated(date)) {
    cli::cli_abort(
      "{.arg date} must contain unique values. Did you forget to group by an identifier variable?"
    )
  }

  src_date <- date
  src_value <- column

  if (drop_na) {
    keep <- !is.na(src_value)
    src_date <- src_date[keep]
    src_value <- src_value[keep]
  }

  if (ff_adjustment) {
    yr <- lubridate::year(src_date)
    keep <- ave(as.numeric(src_date), yr, FUN = max) == as.numeric(src_date)
    src_date <- src_date[keep]
    src_value <- src_value[keep]
  }

  # Fast path: exact date match via hash lookup (O(n))
  if (lag == max_lag) {
    target_date <- date - lag
    idx <- match(target_date, src_date)
    result <- rep(NA_real_, length(date))
    valid <- !is.na(idx)
    result[valid] <- src_value[idx[valid]]
    return(result)
  }

  # Sort source dates for findInterval
  ord <- order(src_date)
  src_date_sorted <- src_date[ord]
  src_value_sorted <- src_value[ord]

  lower <- date - max_lag
  upper <- date - lag

  # Find the last source date <= upper
  idx <- findInterval(upper, src_date_sorted)

  result <- rep(NA_real_, length(date))
  valid <- idx > 0L
  valid[valid] <- src_date_sorted[idx[valid]] >= lower[valid]
  result[valid] <- src_value_sorted[idx[valid]]

  result
}


#' Add Lagged Columns Based on Date and Time Range
#'
#' @description
#' Takes a data.frame and appends a lagged version of given columns based on a date variable, with the
#' ability to specify a range of lags. It also allows for the optional removal of `NA` values.
#'
#' @param df A data frame containing the variables to lag.
#' @param cols A character vector specifying the names of the columns to be lagged.
#' @param by An optional character vector specifying the names of the columns to group by before applying the lag. Defaults to `NULL`.
#' @param lag An integer or an `lubridate::periods()` object, e.g., `month(1)`, specifying the minimum lag (inclusive) to apply to `cols`.
#' @param max_lag An integer or an `lubridate::periods()` object specifying the maximum lag (inclusive) to apply to `cols`.
#'  Defaults to `lag`.
#' @param drop_na A logical value indicating whether to drop `NA` values from the resulting lagged
#'  columns. Defaults to `FALSE`.
#' @param ff_adjustment A logical value indicating whether to lag monthly data based on Fama-French conventions. Here, the values are lagged based on the last observation within the year is taken. Defaults to `FALSE`.
#' @param data_options A named list of \link{data_options} with characters, indicating
#'   the column names required to run this function. The required column names identify
#'   dates. Defaults to `date = date`.
#' @returns A data frame with new, lagged columns added.
#' @family rolling and lagging functions
#' @examples
#' set.seed(42)
#' # Example using a tibble and dplyr::group_by
#' data <- tibble::tibble(
#'   permno = rep(1:2, each = 10),
#'   date = rep(seq.Date(as.Date("2023-01-01"), by = "month", length.out = 10), 2),
#'   size = runif(20, 100, 200),
#'   bm = runif(20, 0.5, 1.5)
#' )
#' add_lagged_columns(data, cols = "size", lag = months(2), by = "permno")
#' @export
add_lagged_columns <- function(
  df,
  cols,
  lag,
  max_lag = lag,
  by = NULL,
  drop_na = FALSE,
  ff_adjustment = FALSE,
  data_options = NULL
) {
  if (is.null(data_options)) {
    data_options <- data_options()
  }

  if (!data_options$date %in% names(df)) {
    cli::cli_abort(
      "{.arg df} must contain the date column {.field {data_options$date}}."
    )
  }

  missing_cols <- setdiff(cols, names(df))
  if (length(missing_cols) > 0) {
    cli::cli_abort(
      "{.arg df} is missing column{?s}: {.field {missing_cols}}."
    )
  }

  if (!is.null(by)) {
    missing_by <- setdiff(by, names(df))
    if (length(missing_by) > 0) {
      cli::cli_abort(
        "{.arg df} is missing grouping column{?s}: {.field {missing_by}}."
      )
    }
  }

  df |>
    dplyr::group_by(dplyr::across(dplyr::all_of(by))) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(cols),
        \(x) {
          lag_column(
            x,
            .data[[data_options$date]],
            lag,
            max_lag,
            drop_na,
            ff_adjustment
          )
        },
        .names = "{.col}_lag"
      )
    ) |>
    dplyr::ungroup()
}
