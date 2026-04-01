#' Add Lagged Columns via Join
#'
#' @description
#' Appends lagged versions of specified columns to a data frame using a
#' join-based approach.
#'
#' When `lag == max_lag` (the default), an equi-join is used: source dates are
#' shifted forward by `lag` and matched exactly. When `lag < max_lag`, an
#' inequality join is used: for each row, the most recent source value within the
#' window `[date - max_lag, date - lag]` is selected.
#'
#' The combination of `by` and date columns must be unique in `df`. If `by` is
#' `NULL`, dates alone must be unique.
#'
#' @param df A data frame containing the variables to lag.
#' @param cols A character vector specifying the names of the columns to be lagged.
#'  Each column produces a new column suffixed with `_lag`.
#' @param lag An integer or a `lubridate::periods()` object, e.g., `months(1)`,
#'  specifying the minimum lag (inclusive) to apply.
#' @param max_lag An integer or a `lubridate::periods()` object specifying the
#'  maximum lag (inclusive) to apply. Defaults to `lag` (exact lag).
#' @param by An optional character vector specifying grouping columns (e.g., a
#'  stock identifier). Lagged values are matched within groups. Defaults to `NULL`.
#' @param drop_na A logical value. If `TRUE`, `NA` values in the source columns
#'  are excluded before matching, so the lookup skips over missing observations.
#'  Applied independently per column. Defaults to `FALSE`.
#' @param ff_adjustment A logical value. If `TRUE`, only the last observation per
#'  year (within each group defined by `by`) is retained as a source for lagged
#'  values, following Fama-French conventions for annual accounting data.
#'  Defaults to `FALSE`.
#' @param data_options A named list of \link{data_options} with characters, indicating
#'   the column names required to run this function. The required column names identify
#'   dates. Defaults to `date = date`.
#'
#' @returns A data frame with the same rows as `df` and new columns appended,
#'  each suffixed with `_lag`. Unmatched rows receive `NA` in the lagged columns.
#'
#' @family rolling and lagging functions
#' @export
#'
#' @examples
#' set.seed(42)
#' data <- tibble::tibble(
#'   permno = rep(1:2, each = 10),
#'   date = rep(seq.Date(as.Date("2023-01-01"), by = "month", length.out = 10), 2),
#'   size = runif(20, 100, 200),
#'   bm = runif(20, 0.5, 1.5)
#' )
#'
#' # Exact lag: each row gets the value from exactly 2 months earlier
#' add_lagged_columns(data, cols = c("size", "bm"), lag = months(2), by = "permno")
#'
#' # Window lag: each row gets the most recent value from 2 to 4 months earlier
#' add_lagged_columns(data, cols = "size", lag = months(2), max_lag = months(4), by = "permno")
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

  date_col <- data_options$date

  if (!date_col %in% names(df)) {
    cli::cli_abort(
      "{.arg df} must contain the date column {.field {date_col}}."
    )
  }

  if (lag < 0 || max_lag < lag) {
    cli::cli_abort(
      "{.arg lag} and {.arg max_lag} must be non-negative and {.arg max_lag} must be >= {.arg lag}."
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

  join_cols <- c(by, date_col)

  if (anyDuplicated(df[join_cols])) {
    cli::cli_abort(
      "The combination of {.arg by} and date columns must be unique in {.arg df}."
    )
  }

  exact_lag <- (lag == max_lag)

  if (!exact_lag) {
    df[[".upper"]] <- df[[date_col]] - lag
    df[[".lower"]] <- df[[date_col]] - max_lag
  }

  for (col in cols) {
    lagged <- df[c(join_cols, col)]

    if (drop_na) {
      lagged <- lagged[!is.na(lagged[[col]]), ]
    }

    if (ff_adjustment) {
      yr <- lubridate::year(lagged[[date_col]])
      grp <- if (!is.null(by)) interaction(lagged[by], yr) else yr
      max_dates <- ave(as.numeric(lagged[[date_col]]), grp, FUN = max)
      lagged <- lagged[as.numeric(lagged[[date_col]]) == max_dates, ]
    }

    lag_col_name <- paste0(col, "_lag")

    if (exact_lag) {
      lagged[[date_col]] <- lagged[[date_col]] + lag
      names(lagged)[names(lagged) == col] <- lag_col_name

      df <- dplyr::left_join(df, lagged, by = join_cols)
    } else {
      names(lagged)[names(lagged) == date_col] <- ".src_date"
      names(lagged)[names(lagged) == col] <- lag_col_name

      df <- df |>
        dplyr::left_join(
          lagged,
          by = dplyr::join_by(!!!by, closest(.upper >= .src_date))
        ) |>
        dplyr::mutate(
          !!lag_col_name := dplyr::if_else(
            !is.na(.data[[".src_date"]]) & .data[[".src_date"]] >= .data[[".lower"]],
            .data[[lag_col_name]],
            NA_real_
          )
        ) |>
        dplyr::select(-".src_date")
    }
  }

  if (!exact_lag) {
    df[[".upper"]] <- NULL
    df[[".lower"]] <- NULL
  }

  df
}
