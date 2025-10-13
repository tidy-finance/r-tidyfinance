#' Add Lagged Versions of Columns to a Data Frame
#'
#' @description `r lifecycle::badge('experimental')`
#'
#' This function adds lagged versions of specified columns to a data frame. Optionally,
#' the operation can be grouped by another column and allows for flexible handling
#' of missing values. The lag is applied based on the `date` column in the data frame.
#'
#' @param data A data frame containing the columns to be lagged.
#' @param cols A character vector specifying the names of the columns to lag.
#' @param by An optional column by which to group the data when applying the lag.
#' Default is `NULL`, meaning no grouping.
#' @param lag The number of periods to lag the columns by. Must be non-negative.
#' @param max_lag An optional maximum lag period. The default is equal to `lag`.
#' @param drop_na A logical value indicating whether to drop rows with missing
#' values in the lagged columns. Default is `TRUE`.
#' @param data_options A list of additional options for data processing, such as
#' the `date` column. If `NULL`, defaults are used.
#'
#' @return A data frame with lagged versions of the specified columns appended,
#' optionally grouped by another column.
#'
#' @export
#'
#' @examples
#' # Create a sample data frame
#' data <- tibble::tibble(
#'   permno = rep(1:2, each = 10),
#'   date = rep(seq.Date(as.Date('2023-01-01'), by = "month", length.out = 10), 2),
#'   bm = runif(20, 0.5, 1.5),
#'   size = runif(20, 100, 200)
#' )
#'
#' # Add lagged columns for 'bm' and 'size' with a 3-month lag, grouped by 'permno'
#' data |>
#'   add_lag_columns(c("bm", "size"), lag = months(3), by = "permno")
#'
#' # Introduce missing values in the data
#' data$bm[c(3, 5, 7, 15, 18)] <- NA
#' data$size[c(2, 4, 8, 13)] <- NA
#'
#' # Add lagged columns with NA values removed
#' data |>
#'   add_lag_columns(c("bm", "size"), lag = months(3), by = permno)
#'
add_lag_columns <- function(
  data,
  cols,
  by = NULL,
  lag,
  max_lag = lag,
  drop_na = TRUE,
  data_options = NULL
) {
  if (lag < 0 || max_lag < lag) {
    cli::cli_abort(
      "{.arg lag} and {.arg max_lag} must be non-negative and {.arg max_lag} must be greater than or equal to {.arg lag}"
    )
  }

  if (is.null(data_options)) {
    data_options <- data_options()
  }

  date_col <- data_options$date

  by <- rlang::enquo(by)
  cols <- rlang::syms(cols)

  main_data <- data |>
    rename(date = all_of(date_col)) |>
    mutate(..lower_bound = date + lag, ..upper_bound = date + max_lag)

  result <- data
  for (col in cols) {
    if (!rlang::quo_is_null(by)) {
      tmp_data <- main_data |>
        select(!!by, ..lower_bound, ..upper_bound, all_of(col))
    } else {
      tmp_data <- main_data |>
        select(..lower_bound, ..upper_bound, all_of(col))
    }

    if (drop_na) {
      tmp_data <- tmp_data |>
        tidyr::drop_na(!!col)
    }

    if (!rlang::quo_is_null(by)) {
      result <- result |>
        left_join(
          tmp_data,
          join_by(!!by, closest(date >= ..lower_bound), date <= ..upper_bound),
          suffix = c("", "_lag")
        ) |>
        select(-..lower_bound, -..upper_bound)
    } else {
      result <- result |>
        left_join(
          tmp_data,
          join_by(closest(date >= ..lower_bound), date <= ..upper_bound),
          suffix = c("", "_lag")
        ) |>
        select(-..lower_bound, -..upper_bound)
    }
  }

  result |>
    rename(date_col := date)
}

#' Add lagged variable values over a date range
#'
#' @description
#' Merges lagged values of selected variables from one dataset (`new_data`)
#' into another (`original_data`), based on date ranges defined by `min_lag`
#' and `max_lag`. Optionally applies a Fama–French style year-based adjustment.
#'
#' @param original_data A data frame containing the original panel data.
#' @param new_data A data frame containing the variables to lag and merge.
#' @param id_keys A character vector specifying the identifier column(s).
#' @param id_date A string giving the date column name (default: `"date"`).
#' @param min_lag A `lubridate::Period` specifying the lower lag bound.
#' @param max_lag A `lubridate::Period` specifying the upper lag bound.
#' @param ff_adjustment Logical; if `TRUE`, applies a Fama–French year adjustment.
#'
#' @return A data frame with lagged columns from `new_data` merged into `original_data`.
#' @export
#'
#' @examples
#' library(dplyr)
#' library(lubridate)
#'
#' df1 <- tibble(
#'   id = rep(1:2, each = 6),
#'   date = rep(seq(as.Date("2020-01-01"), by = "month", length.out = 6), 2)
#' )
#'
#' df2 <- df1 |>
#'   mutate(x = rnorm(n()))
#'
#' add_lagged_values(
#'   original_data = df1,
#'   new_data = df2,
#'   id_keys = "id",
#'   min_lag = months(1),
#'   max_lag = months(3)
#' )
add_lagged_values <- function(
  original_data,
  new_data,
  id_keys,
  id_date = "date",
  min_lag,
  max_lag,
  ff_adjustment = FALSE
) {
  stopifnot(is.character(id_keys))
  stopifnot(id_date %in% names(original_data))
  stopifnot(id_date %in% names(new_data))

  new_column_names <- new_data |>
    dplyr::select(-dplyr::all_of(c(id_keys, id_date))) |>
    names()

  new_data <- new_data |>
    dplyr::mutate(
      .lower = .data[[id_date]] %m+% min_lag,
      .upper = .lower %m+% max_lag
    )

  if (ff_adjustment) {
    new_data <- new_data |>
      dplyr::mutate(.year = lubridate::year(.data[[id_date]]))
  }

  for (col_name in new_column_names) {
    tmp_data <- new_data |>
      dplyr::select(
        dplyr::all_of(c(id_keys, id_date, col_name, ".lower", ".upper")),
        dplyr::any_of(".year")
      )

    if (ff_adjustment) {
      tmp_data <- tmp_data |>
        dplyr::group_by(dplyr::across(all_of(c(id_keys, ".year")))) |>
        dplyr::slice_max(order_by = .data[[id_date]], n = 1) |>
        dplyr::ungroup() |>
        dplyr::select(-dplyr::all_of(id_date))
    }

    col_values <- original_data |>
      dplyr::rename(.date = dplyr::all_of(id_date)) |>
      dplyr::left_join(
        tmp_data,
        dplyr::join_by(
          !!!rlang::syms(id_keys),
          closest(.date >= .lower),
          .date < .upper
        )
      ) |>
      dplyr::pull(dplyr::all_of(col_name))

    original_data <- original_data |>
      dplyr::mutate("{col_name}" := col_values)
  }

  original_data
}
