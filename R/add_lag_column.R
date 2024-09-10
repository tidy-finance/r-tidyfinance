library(dplyr)
library(rlang)
library(tidyr)

# TODO: add date column as parameter - or data_options?

add_lag_column <- function(data, cols, by = NULL, lag, max_lag = lag, drop_na = TRUE) {

  if (lag < 0 || max_lag < lag) {
    cli::cli_abort("{.arg lag} and {.arg max_lag} must be non-negative and {.arg max_lag} must be greater than or equal to {.arg lag}")
  }

  by <- enquo(by)
  cols <- enquos(cols)

  if (!quo_is_null(by)) {
    main_data <- data |>
      mutate(..lower_bound = date + lag,
             ..upper_bound = date + max_lag) |>
      select(!!by, ..lower_bound, ..upper_bound, !!!cols)
  } else {
    main_data <- data |>
      mutate(..lower_bound = date + lag,
             ..upper_bound = date + max_lag) |>
      select(..lower_bound, ..upper_bound, !!!cols)
  }

  results <- data
  for (col in cols) {

    if (!quo_is_null(by)) {
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

    if (!quo_is_null(by)) {
      result <- data |>
        left_join(tmp_data,
                  join_by(!!by, closest(date >= ..lower_bound), date <= ..upper_bound),
                  suffix = c("", "_lag"))
    } else {
      result <- data |>
        left_join(tmp_data,
                  join_by(closest(date >= ..lower_bound), date <= ..upper_bound),
                  suffix = c("", "_lag"))
    }
  }

  result |>
    select(-..lower_bound, -..upper_bound)
}

# Example usage
data <- tibble::tibble(
  permno = rep(1:2, each = 10),
  date = rep(seq.Date(as.Date('2023-01-01'), by = "month", length.out = 10), 2),
  bm = runif(20, 0.5, 1.5),
  size = runif(20, 100, 200)
)

data |>
  add_lag_column(c(bm, size), lag = months(3), by = permno)

data |>
  add_lag_column(c("bm", "size"), lag = months(3))



