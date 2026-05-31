#' Filter Sorting Data
#'
#' Applies sample construction filters to a data frame before portfolio
#' sorting. Filters are applied in a fixed order: financials exclusion,
#' utilities exclusion, minimum stock price, minimum size quantile, minimum
#' listing age, positive book equity, and positive earnings. An informational
#' message is emitted for each filter that actually removes at least one
#' observation.
#'
#' @param data A data frame containing the stock-level panel data to be
#'   filtered.
#' @param filter_options A list of class `tidyfinance_filter_options` created
#'   by [filter_options()]. If `NULL` (the default), the defaults from
#'   [filter_options()] are used (i.e., no filters are applied). The arguments
#'   accepted by [filter_options()] include
#'   \itemize{
#'     \item `exclude_financials` A logical indicating whether to exclude
#'       financial firms (SIC codes 6000–6799). Defaults to `FALSE`.
#'     \item `exclude_utilities` A logical indicating whether to exclude
#'       utility firms (SIC codes 4900–4999). Defaults to `FALSE`.
#'     \item `min_stock_price` A single positive numeric specifying the
#'       minimum stock price required to include an observation. `NULL` (the
#'       default) applies no price filter.
#'     \item `min_size_quantile` A single numeric strictly between 0 and 1
#'       specifying the minimum cross-sectional size quantile (based on lagged
#'       market cap) required to include an observation. `NULL` (the default)
#'       applies no size quantile filter. The cutoff is computed from NYSE
#'       stocks only; the `exchange` column (mapped via [data_options()])
#'       must be present in the data or an error is raised.
#'     \item `min_listing_age` A single non-negative integer or numeric
#'       specifying the minimum number of months a stock must have been listed
#'       in CRSP. `NULL` (the default) applies no listing age filter.
#'     \item `exclude_negative_book_equity` A logical indicating whether to
#'       exclude observations with non-positive book equity. Defaults to
#'       `FALSE`.
#'     \item `exclude_negative_earnings` A logical indicating whether to
#'       exclude observations with non-positive earnings. Defaults to `FALSE`.
#'   }
#' @param data_options A list of class `tidyfinance_data_options` (created via
#'   [data_options()]) specifying column name mappings. The `siccd` element is
#'   used to specify the SIC code column, `price` is used to specify the
#'   (adjusted) price column, `mktcap_lag` is used to specify the market
#'   capitalization column, `date` is used to specify the date column,
#'   `listing_age` is used to specify the listing age column, `be` is used to
#'   specify the book equity column, and `earnings` is used to specify the
#'   earnings column. Uses [data_options()] default if `NULL`:
#'   `"siccd" = "siccd"`, `"price" = "prc_adj"`, `"exchange" = "exchange"`
#'   `"mktcap_lag" = "mktcap_lag"`, `"date" = "date"`,
#'   `"listing_age" = "listing_age"`, `"be" = "be"`, and `"earnings" = "ib"`.
#' @param quiet A logical indicating whether informational messages should be
#'   suppressed. Defaults to `FALSE`.
#'
#' @returns The filtered data frame, preserving the class and structure of the
#'   input.
#'
#' @family portfolio functions
#' @export
#'
#' @examples
#' data <- data.frame(
#'   permno = 1:5,
#'   date = as.Date("2020-01-01"),
#'   siccd = c(6100, 2000, 4950, 3000, 6500),
#'   prc_adj = c(5, 0.5, 15, 20, 10)
#' )
#'
#' data |>
#'   filter_sorting_data(
#'     filter_options = filter_options(
#'       exclude_financials = TRUE,
#'       min_stock_price = 1
#'     )
#'   )
#'
filter_sorting_data <- function(
  data,
  filter_options = NULL,
  data_options = NULL,
  quiet = FALSE
) {
  if (!is.logical(quiet) || length(quiet) != 1 || is.na(quiet)) {
    cli::cli_abort("{.arg quiet} must be a single logical.")
  }

  if (is.null(filter_options)) {
    filter_options <- filter_options()
  }

  if (is.null(data_options)) {
    data_options <- data_options()
  }

  # exclude_financials / exclude_utilities (share the same SIC column)
  if (
    isTRUE(filter_options$exclude_financials) ||
      isTRUE(filter_options$exclude_utilities)
  ) {
    col_siccd <- data_options$siccd
    require_column(data, col_siccd, "data_options$siccd")

    if (isTRUE(filter_options$exclude_financials)) {
      data <- filter_with_log(
        data,
        is.na(.data[[col_siccd]]) |
          !(.data[[col_siccd]] >= 6000 & .data[[col_siccd]] <= 6799),
        "exclude_financials",
        quiet
      )
    }

    if (isTRUE(filter_options$exclude_utilities)) {
      data <- filter_with_log(
        data,
        is.na(.data[[col_siccd]]) |
          !(.data[[col_siccd]] >= 4900 & .data[[col_siccd]] <= 4999),
        "exclude_utilities",
        quiet
      )
    }
  }

  # min_stock_price
  if (!is.null(filter_options$min_stock_price)) {
    col_price <- data_options$price
    require_column(data, col_price, "data_options$price")
    data <- filter_with_log(
      data,
      !is.na(.data[[col_price]]) &
        .data[[col_price]] >= filter_options$min_stock_price,
      "min_stock_price",
      quiet
    )
  }

  # min_size_quantile
  if (!is.null(filter_options$min_size_quantile)) {
    col_mktcap_lag <- data_options$mktcap_lag
    col_date <- data_options$date
    col_exchange <- data_options$exchange
    require_column(data, col_mktcap_lag, "data_options$mktcap_lag")
    require_column(data, col_date, "data_options$date")
    require_column(
      data,
      col_exchange,
      "data_options$exchange",
      info = paste0(
        "The size quantile cutoff is computed from NYSE stocks. ",
        "Set {.arg data_options$exchange} to the correct column name."
      )
    )
    n_before <- nrow(data)
    size_threshold <- filter_options$min_size_quantile
    size_cutoffs <- data |>
      dplyr::filter(.data[[col_exchange]] == "NYSE") |>
      dplyr::group_by(.data[[col_date]]) |>
      dplyr::summarise(
        .size_cutoff = quantile(
          .data[[col_mktcap_lag]],
          probs = size_threshold,
          na.rm = TRUE
        ),
        .groups = "drop"
      )

    dates_in_data <- data |>
      dplyr::distinct(.data[[col_date]]) |>
      dplyr::pull(col_date)

    dates_missing_cutoff <- setdiff(dates_in_data, size_cutoffs[[col_date]])
    if (length(dates_missing_cutoff) > 0) {
      cli::cli_warn(c(
        paste0(
          "Filter 'min_size_quantile': {length(dates_missing_cutoff)} ",
          "date{?s} dropped because no NYSE stocks are available to compute ",
          "the size quantile cutoff."
        ),
        "i" = "Affected date{?s}: {.val {dates_missing_cutoff}}."
      ))
    }

    data <- data |>
      check_new_col(".size_cutoff") |>
      dplyr::select(-dplyr::any_of(".size_cutoff")) |>
      dplyr::inner_join(size_cutoffs, by = col_date) |>
      dplyr::filter(
        !is.na(.data[[col_mktcap_lag]]) &
          .data[[col_mktcap_lag]] >= .data[[".size_cutoff"]]
      ) |>
      dplyr::select(-".size_cutoff")
    n_dropped <- n_before - nrow(data)
    if (!quiet && n_dropped > 0) {
      cli::cli_inform(
        "Filter 'min_size_quantile': removed {n_dropped} observation{?s}."
      )
    }
  }

  # min_listing_age
  if (!is.null(filter_options$min_listing_age)) {
    col_listing_age <- data_options$listing_age
    require_column(data, col_listing_age, "data_options$listing_age")
    data <- filter_with_log(
      data,
      !is.na(.data[[col_listing_age]]) &
        .data[[col_listing_age]] >= filter_options$min_listing_age,
      "min_listing_age",
      quiet
    )
  }

  # exclude_negative_book_equity
  if (isTRUE(filter_options$exclude_negative_book_equity)) {
    col_be <- data_options$be
    require_column(data, col_be, "data_options$be")
    data <- filter_with_log(
      data,
      !is.na(.data[[col_be]]) & .data[[col_be]] > 0,
      "exclude_negative_book_equity",
      quiet
    )
  }

  # exclude_negative_earnings
  if (isTRUE(filter_options$exclude_negative_earnings)) {
    col_earn <- data_options$earnings
    require_column(data, col_earn, "data_options$earnings")
    data <- filter_with_log(
      data,
      !is.na(.data[[col_earn]]) & .data[[col_earn]] > 0,
      "exclude_negative_earnings",
      quiet
    )
  }

  data
}

#' Abort if a mapped column is missing from the data
#'
#' Shared guard used by [filter_sorting_data()] to verify that a column mapped
#' through [data_options()] is present before it is used in a filter.
#'
#' @param data A data frame to check.
#' @param col A single string giving the column name that must be present.
#' @param arg A single string naming the `data_options` element to reference in
#'   the default hint (e.g., `"data_options$siccd"`).
#' @param info Optional replacement for the default hint shown under the error.
#'
#' @returns Invisibly returns `data`; called for its side effect of aborting
#'   when `col` is missing.
#' @noRd
require_column <- function(data, col, arg, info = NULL) {
  if (!col %in% colnames(data)) {
    if (is.null(info)) {
      info <- "Set {.arg {arg}} to the correct column name."
    }
    cli::cli_abort(
      c(
        "Column {.val {col}} not found in {.arg data}.",
        "i" = info
      ),
      call = rlang::caller_env()
    )
  }
  invisible(data)
}

#' Apply a row filter and report how many observations were removed
#'
#' Shared helper used by [filter_sorting_data()]. Applies `condition` via
#' [dplyr::filter()], then emits an informational message reporting the number
#' of dropped observations unless `quiet` is `TRUE` or nothing was removed.
#'
#' @param data A data frame to filter.
#' @param condition An unquoted filter expression, evaluated in the context of
#'   `data` (it may reference columns via the `.data` pronoun).
#' @param label A single string naming the filter, used in the message.
#' @param quiet A logical; if `TRUE`, no message is emitted.
#'
#' @returns The filtered data frame.
#' @noRd
filter_with_log <- function(data, condition, label, quiet) {
  n_before <- nrow(data)
  data <- dplyr::filter(data, {{ condition }})
  n_dropped <- n_before - nrow(data)
  if (!quiet && n_dropped > 0) {
    cli::cli_inform("Filter '{label}': removed {n_dropped} observation{?s}.")
  }
  data
}
