#' Join Lagged Variable Values over a Date Range
#'
#' Joins lagged values of selected variables from one dataset (`new_data`)
#' into another (`original_data`), based on date ranges defined by `min_lag`
#' and `max_lag`. Unlike [add_lagged_columns()], this function supports
#' joining across data frames with different date grids (e.g., monthly source
#' data into quarterly target data).
#'
#' For `min_lag`/`max_lag` expressed in whole months or years, the dates in
#' both data frames should be the first of the month, as returned by the
#' `download_data()` functions. On other days of the month the monthly offset
#' can miss the intended match (for example, a source dated 30 April never
#' matches a target dated 31 May); floor the dates with
#' `lubridate::floor_date(date, "month")` beforehand. This does not apply to
#' `ff_adjustment = TRUE`, which works on calendar years.
#'
#' @param original_data A data frame containing the target panel data.
#' @param new_data A data frame containing the source variables to lag and
#'   merge. All columns besides `id_keys` and the date column will be lagged
#'   and joined.
#' @param id_keys A character vector specifying the identifier column(s).
#' @param min_lag A `lubridate::Period` specifying the lower lag bound
#'   (inclusive). Required unless `ff_adjustment = TRUE`.
#' @param max_lag A `lubridate::Period` specifying the upper lag bound
#'   (inclusive). Required unless `ff_adjustment = TRUE`.
#' @param ff_adjustment Logical; if `TRUE`, applies the Fama and French
#'   (1993) timing convention: the last observation per identifier and
#'   calendar year (the fiscal year-end) is used from July of the following
#'   year through June of the year after that, whatever the month of the
#'   fiscal year-end. A value for a fiscal year ending in December 2020 or in
#'   June 2020 is thus used from July 2021 to June 2022. `min_lag` and
#'   `max_lag` are ignored. Defaults to `FALSE`.
#' @param data_options A list of class `tidyfinance_data_options` (created via
#'   [data_options()]) specifying column name mappings. The `date` element is
#'   used to identify the date column. Uses [data_options()] default if
#'   `NULL`: `"date" = "date"`.
#'
#' @returns A data frame with all columns from `original_data` plus the
#'   lagged columns from `new_data` (keeping their original names).
#'
#' @family rolling and lagging functions
#' @export
#'
#' @examples
#' set.seed(42)
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
#' join_lagged_values(
#'   original_data = df1,
#'   new_data = df2,
#'   id_keys = "id",
#'   min_lag = months(1),
#'   max_lag = months(3)
#' )
#'
#' # Fama-French timing: annual values are used from July to June
#' annual <- tibble(
#'   id = 1:2,
#'   date = as.Date(c("2019-12-01", "2019-06-01")),
#'   x = c(10, 20)
#' )
#' monthly <- tibble(
#'   id = rep(1:2, each = 3),
#'   date = rep(as.Date(c("2020-06-01", "2020-07-01", "2021-06-01")), 2)
#' )
#' join_lagged_values(monthly, annual, id_keys = "id", ff_adjustment = TRUE)
join_lagged_values <- function(
  original_data,
  new_data,
  id_keys,
  min_lag = NULL,
  max_lag = NULL,
  ff_adjustment = FALSE,
  data_options = NULL
) {
  # To avoid undefined global variable notes in checks, the columns and the
  # closest() helper used in the join_by() below are defined explicitly here.
  .date <- .lower <- .upper <- closest <- NULL

  if (is.null(data_options)) {
    data_options <- data_options()
  }
  id_date <- data_options$date

  if (ff_adjustment) {
    inform_ff_lags_ignored(c(
      if (!is.null(min_lag)) "min_lag",
      if (!is.null(max_lag)) "max_lag"
    ))
  } else if (is.null(min_lag) || is.null(max_lag)) {
    cli::cli_abort(
      paste(
        "{.arg min_lag} and {.arg max_lag} are required unless",
        "{.code ff_adjustment = TRUE}."
      )
    )
  }

  if (!is.character(id_keys)) {
    cli::cli_abort(
      paste(
        "{.arg id_keys} must be a character vector,",
        "not {.obj_type_friendly {id_keys}}."
      )
    )
  }
  if (!id_date %in% names(original_data)) {
    cli::cli_abort(
      "{.arg original_data} must contain the column {.field {id_date}}."
    )
  }
  if (!id_date %in% names(new_data)) {
    cli::cli_abort(
      "{.arg new_data} must contain the column {.field {id_date}}."
    )
  }

  missing_keys_original <- setdiff(id_keys, names(original_data))
  if (length(missing_keys_original) > 0) {
    cli::cli_abort(
      paste(
        "{.arg original_data} is missing id column{?s}:",
        "{.field {missing_keys_original}}."
      )
    )
  }

  missing_keys_new <- setdiff(id_keys, names(new_data))
  if (length(missing_keys_new) > 0) {
    cli::cli_abort(
      "{.arg new_data} is missing id column{?s}: {.field {missing_keys_new}}."
    )
  }

  new_column_names <- new_data |>
    dplyr::select(-dplyr::all_of(c(id_keys, id_date))) |>
    names()

  if (length(new_column_names) == 0) {
    cli::cli_abort(
      paste(
        "{.arg new_data} must contain columns besides {.field {id_keys}} and",
        "{.field {id_date}}."
      )
    )
  }

  original_non_key_cols <- setdiff(names(original_data), c(id_keys, id_date))
  duplicate_cols <- intersect(new_column_names, original_non_key_cols)
  if (length(duplicate_cols) > 0) {
    cli::cli_abort(
      c(
        paste0(
          "{cli::qty(length(duplicate_cols))}Column{?s} in {.arg new_data} ",
          "already exist in {.arg original_data}."
        ),
        "x" = paste0(
          "{cli::qty(length(duplicate_cols))}",
          "Duplicate column{?s}: {.field {duplicate_cols}}."
        ),
        "i" = paste0(
          "{cli::qty(length(duplicate_cols))}",
          "Remove or rename {?this column/these columns} ",
          "from {.arg new_data} before joining."
        )
      )
    )
  }

  if (ff_adjustment) {
    new_data <- check_new_col(new_data, c(".lower", ".upper", ".year"))
    new_column_names <- setdiff(new_column_names, ".year")
  } else {
    new_data <- new_data |>
      check_new_col(c(".lower", ".upper")) |>
      dplyr::mutate(
        .lower = .data[[id_date]] %m+% min_lag,
        .upper = .data[[id_date]] %m+% max_lag
      )
  }

  for (col_name in new_column_names) {
    if (ff_adjustment) {
      tmp_data <- ff_windows(new_data, id_keys, id_date, col_name)
    } else {
      tmp_data <- new_data |>
        dplyr::select(
          dplyr::all_of(c(id_keys, col_name, ".lower", ".upper"))
        )
    }

    col_values <- original_data |>
      dplyr::rename(.date = dplyr::all_of(id_date)) |>
      dplyr::left_join(
        tmp_data,
        dplyr::join_by(
          !!!rlang::syms(id_keys),
          closest(.date >= .lower),
          .date <= .upper
        )
      ) |>
      dplyr::pull(dplyr::all_of(col_name))

    original_data <- original_data |>
      check_new_col(col_name) |>
      dplyr::mutate("{col_name}" := col_values)
  }

  original_data
}

# Fama-French timing: keep the last observation per id and calendar year of
# the fiscal year-end, and use it from July of the following year through June
# of the year after that.
ff_windows <- function(data, id_keys, date_col, cols) {
  .year <- NULL

  data |>
    dplyr::select(dplyr::all_of(c(id_keys, date_col, cols))) |>
    dplyr::mutate(.year = lubridate::year(.data[[date_col]])) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(id_keys, ".year")))) |>
    dplyr::slice_max(order_by = .data[[date_col]], n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      .lower = lubridate::make_date(.year + 1L, 7L, 1L),
      .upper = lubridate::make_date(.year + 2L, 6L, 30L)
    ) |>
    dplyr::select(-dplyr::all_of(c(date_col, ".year")))
}

inform_ff_lags_ignored <- function(args) {
  if (length(args) == 0) {
    return(invisible())
  }
  cli::cli_inform(
    c(
      "i" = paste(
        "{.arg {args}} {?is/are} ignored with {.code ff_adjustment = TRUE}.",
        "A value for a fiscal year ending in calendar year t is used from",
        "July of t + 1 through June of t + 2."
      )
    ),
    .frequency = "regularly",
    .frequency_id = "tidyfinance_ff_adjustment_lags"
  )
}
