#' Join lagged variable values over a date range
#'
#' @description `r lifecycle::badge('experimental')`
#'
#' Joins lagged values of selected variables from one dataset (`new_data`)
#' into another (`original_data`), based on date ranges defined by `min_lag`
#' and `max_lag`. Unlike [add_lagged_columns()], this function supports joining
#' across data frames with different date grids (e.g., monthly source data
#' into quarterly target data).
#'
#' @param original_data A data frame containing the target panel data.
#' @param new_data A data frame containing the source variables to lag and merge.
#'  All columns besides `id_keys` and `id_date` will be lagged and joined.
#' @param id_keys A character vector specifying the identifier column(s).
#' @param id_date A string giving the date column name (default: `"date"`).
#' @param min_lag A `lubridate::Period` specifying the lower lag bound (inclusive).
#' @param max_lag A `lubridate::Period` specifying the upper lag bound (inclusive).
#' @param ff_adjustment Logical; if `TRUE`, keeps only the last observation per
#'  identifier and year before lagging (Fama–French convention). Defaults to `FALSE`.
#'
#' @returns A data frame with all columns from `original_data` plus the lagged
#'  columns from `new_data` (keeping their original names).
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
join_lagged_values <- function(
  original_data,
  new_data,
  id_keys,
  id_date = "date",
  min_lag,
  max_lag,
  ff_adjustment = FALSE
) {
  if (!is.character(id_keys)) {
    cli::cli_abort(
      "{.arg id_keys} must be a character vector, not {.obj_type_friendly {id_keys}}."
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
      "{.arg original_data} is missing id column{?s}: {.field {missing_keys_original}}."
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
      "{.arg new_data} must contain columns besides {.field {id_keys}} and {.field {id_date}}."
    )
  }

  new_data <- new_data |>
    dplyr::mutate(
      .lower = .data[[id_date]] %m+% min_lag,
      .upper = .data[[id_date]] %m+% max_lag
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
        dplyr::group_by(dplyr::across(dplyr::all_of(c(id_keys, ".year")))) |>
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
          .date <= .upper
        )
      ) |>
      dplyr::pull(dplyr::all_of(col_name))

    original_data <- original_data |>
      dplyr::mutate("{col_name}" := col_values)
  }

  original_data
}
