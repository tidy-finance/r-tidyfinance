#' Lag a Column Based on Date and Time Range
#'
#' @description `r lifecycle::badge('experimental')`
#'
#' This function generates a lagged version of a given column based on a date variable, with the
#' ability to specify a range of lags. It also allows for the optional removal of `NA` values.
#'
#' @param column A numeric vector or column to be lagged.
#' @param date A vector representing dates corresponding to the `column`. The column should be in a
#'  date or datetime format.
#' @param lag An integer or an `lubridate::periods()` object, e.g., `month(1)`, specifying the minimum lag (in days, hours, etc.) to apply to `column`.
#' @param max_lag An integer or an `lubridate::periods()` object specifying the maximum lag (in days, hours, etc.) to apply to `column`.
#'  Defaults to `lag`.
#' @param drop_na A logical value indicating whether to drop `NA` values from the resulting lagged
#'  column. Defaults to `TRUE`.
#' @param ff_adjustment A logical value indicating whether to lag monthly data based on Fama-French conventions. Here, the values are lagged based on the last observation within the year is taken. Defaults to `FALSE`.
#' @return A vector of the same length as `column`, containing the lagged values.
#'  If no matching dates are found within the lag window, `NA` is returned for that position.
#'
#' @export
#'
#' @examples
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
#'data |>
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
      "{.arg lag} and {.arg max_lag} must be non-negative and {.arg max_lag} must be greater than or equal to {.arg lag}"
    )
  }

  if (length(unique(date)) != length(date)) {
    cli::cli_abort(
      "{.arg date} must contain unique values. Did you forget to group by an identifier variable?"
    )
  }
  tmp_data <- dplyr::tibble(date = date, value = column) |>
    dplyr::mutate(lower_bound = date + lag, upper_bound = date + max_lag)

  if (drop_na) {
    tmp_data <- tmp_data[!is.na(tmp_data$value), ]
  }

  if (ff_adjustment) {
    tmp_data <- tmp_data |>
      dplyr::mutate(year = lubridate::year(date)) |>
      dplyr::slice_max(order_by = date, n = 1, .by = year) |>
      dplyr::select(-year)
  }

  dplyr::tibble(date = date) |>
    dplyr::left_join(
      tmp_data |> select(-date),
      dplyr::join_by(closest(date >= lower_bound), date <= upper_bound)
    ) |>
    pull(value)
}


#' Add a lagged a Column Based on Date and Time Range
#'
#' @description
#' This function takes a data.frame and appends a lagged version of given columns based on a date variable, with the
#' ability to specify a range of lags. It also allows for the optional removal of `NA` values.
#'
#' @param df A data frame containing the variables to lag.
#' @param cols A character vector specifying the names of the columns to be lagged.
#' @param by An optional character vector specifying the names of the columns to group by before applying the lag. Defaults to `NULL`.
#' @param lag An integer or an `lubridate::periods()` object, e.g., `month(1)`, specifying the minimum lag (in days, hours, etc.) to apply to `column`.
#' @param max_lag An integer or an `lubridate::periods()` object specifying the maximum lag (in days, hours, etc.) to apply to `column`.
#'  Defaults to `lag`.
#' @param drop_na A logical value indicating whether to drop `NA` values from the resulting lagged
#'  column. Defaults to `TRUE`.
#' @param ff_adjustment A logical value indicating whether to lag monthly data based on Fama-French conventions. Here, the values are lagged based on the last observation within the year is taken. Defaults to `FALSE`.
#' @param data_options A list of additional options for data processing, such as the `date` column. If `NULL`, defaults are used.
#' @return A data frame with new, lagged columns added.
#' @examples
#' # Example using a tibble and dplyr::group_by
#'  data <- tibble::tibble(
#'    permno = rep(1:2, each = 10),
#'    date = rep(seq.Date(as.Date('2023-01-01'), by = "month", length.out = 10), 2),
#'    size = runif(20, 100, 200),
#'    bm = runif(20, 0.5, 1.5)
#'  )
#' data |> add_lagged_columns(cols = "size", lag= months(2), by = "permno")
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
  date <- df[[data_options$date]]

  tmp_df <- df |>
    group_by(across(all_of(by))) |>
    mutate(across(
      all_of(cols),
      \(x) {
        lag_column(x, date, lag, max_lag, drop_na, ff_adjustment)
      },
      .names = "{.col}_lag"
    ))
  return(tmp_df)
}
