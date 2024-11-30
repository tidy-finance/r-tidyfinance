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
  data, cols, by = NULL, lag, max_lag = lag, drop_na = TRUE, data_options = NULL
) {

  if (lag < 0 || max_lag < lag) {
    cli::cli_abort("{.arg lag} and {.arg max_lag} must be non-negative and {.arg max_lag} must be greater than or equal to {.arg lag}")
  }

  if (is.null(data_options)) {
    data_options <- data_options()
  }

  date_col <- data_options$date

  by <- rlang::enquo(by)
  cols <- rlang::syms(cols)

  main_data <- data |>
    rename(date = all_of(date_col)) |>
    mutate(..lower_bound = date + lag,
           ..upper_bound = date + max_lag)

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
        left_join(tmp_data,
                  join_by(!!by, closest(date >= ..lower_bound), date <= ..upper_bound),
                  suffix = c("", "_lag")) |>
        select(-..lower_bound, -..upper_bound)
    } else {
      result <- result |>
        left_join(tmp_data,
                  join_by(closest(date >= ..lower_bound), date <= ..upper_bound),
                  suffix = c("", "_lag")) |>
        select(-..lower_bound, -..upper_bound)
    }
  }

  result |>
    rename(date_col := date)
}
