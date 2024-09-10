library(tidyfinance)
library(dplyr)

add_lag_column <- function(data, cols, by = NULL, lag, max_lag = lag, drop_na = TRUE, data_options = NULL) {

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
    rename(date = date_col) |>
    mutate(..lower_bound = date + lag,
           ..upper_bound = date + max_lag)

  result <- data
  for (col in cols) {

    if (!rlang::quo_is_null(by)) {
      tmp_data <- main_data |>
        select(!!by, ..lower_bound, ..upper_bound, !!col)
    } else {
      tmp_data <- main_data |>
        select(..lower_bound, ..upper_bound, !!col)
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

# Example usage
data <- tibble::tibble(
  permno = rep(1:2, each = 10),
  date = rep(seq.Date(as.Date('2023-01-01'), by = "month", length.out = 10), 2),
  bm = runif(20, 0.5, 1.5),
  size = runif(20, 100, 200)
)

data |>
  add_lag_column(c("bm", "size"), lag = months(3), by = "permno")

# Introduce some NAs into the data
data$bm[c(3, 5, 7, 15, 18)] <- NA
data$size[c(2, 4, 8, 13)] <- NA

# Apply the lag function
data |>
  add_lag_column(c("bm", "size"), lag = months(3), by = permno)

