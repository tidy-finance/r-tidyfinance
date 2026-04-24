#' Filter Sorting Data
#'
#' Applies sample construction filters to a data frame before portfolio
#' sorting. Filters are applied in a fixed order: financials exclusion,
#' utilities exclusion, minimum stock price, minimum size quantile, minimum
#' listing age, and positive book equity. An informational message is emitted
#' for each filter that actually removes at least one observation.
#'
#' @param data A data frame containing the stock-level panel data to be
#'   filtered.
#' @param filter_options A list of class `tidyfinance_filter_options` created
#'   by [filter_options()]. If `NULL` (the default), the defaults from
#'   [filter_options()] are used (i.e., no filters are applied).
#' @param data_options A list of class `tidyfinance_data_options` created by
#'   [data_options()]. If `NULL` (the default), the defaults from
#'   [data_options()] are used.
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
#' \dontrun{
#' data |>
#'   filter_sorting_data(
#'     filter_options = filter_options(
#'       exclude_financials = TRUE,
#'       min_stock_price = 1
#'     )
#'   )
#' }
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
    if (!col_siccd %in% colnames(data)) {
      cli::cli_abort(c(
        "Column {.val {col_siccd}} not found in {.arg data}.",
        "i" = "Set {.arg data_options$siccd} to the correct column name."
      ))
    }

    if (isTRUE(filter_options$exclude_financials)) {
      n_before <- nrow(data)
      data <- data |>
        dplyr::filter(
          is.na(.data[[col_siccd]]) |
            !(.data[[col_siccd]] >= 6000 & .data[[col_siccd]] <= 6799)
        )
      n_dropped <- n_before - nrow(data)
      if (!quiet && n_dropped > 0) {
        cli::cli_inform(
          "Filter 'exclude_financials': removed {n_dropped} observation{?s}."
        )
      }
    }

    if (isTRUE(filter_options$exclude_utilities)) {
      n_before <- nrow(data)
      data <- data |>
        dplyr::filter(
          is.na(.data[[col_siccd]]) |
            !(.data[[col_siccd]] >= 4900 & .data[[col_siccd]] <= 4999)
        )
      n_dropped <- n_before - nrow(data)
      if (!quiet && n_dropped > 0) {
        cli::cli_inform(
          "Filter 'exclude_utilities': removed {n_dropped} observation{?s}."
        )
      }
    }
  }

  # min_stock_price
  if (!is.null(filter_options$min_stock_price)) {
    col_price <- data_options$price
    if (!col_price %in% colnames(data)) {
      cli::cli_abort(c(
        "Column {.val {col_price}} not found in {.arg data}.",
        "i" = "Set {.arg data_options$price} to the correct column name."
      ))
    }
    n_before <- nrow(data)
    data <- data |>
      dplyr::filter(
        !is.na(.data[[col_price]]) &
          .data[[col_price]] >= filter_options$min_stock_price
      )
    n_dropped <- n_before - nrow(data)
    if (!quiet && n_dropped > 0) {
      cli::cli_inform(
        "Filter 'min_stock_price': removed {n_dropped} observation{?s}."
      )
    }
  }

  # min_size_quantile
  if (!is.null(filter_options$min_size_quantile)) {
    col_mktcap_lag <- data_options$mktcap_lag
    col_date <- data_options$date
    if (!col_mktcap_lag %in% colnames(data)) {
      cli::cli_abort(c(
        "Column {.val {col_mktcap_lag}} not found in {.arg data}.",
        "i" = "Set {.arg data_options$mktcap_lag} to the correct column name."
      ))
    }
    if (!col_date %in% colnames(data)) {
      cli::cli_abort(c(
        "Column {.val {col_date}} not found in {.arg data}.",
        "i" = "Set {.arg data_options$date} to the correct column name."
      ))
    }
    n_before <- nrow(data)
    size_threshold <- filter_options$min_size_quantile
    data <- data |>
      dplyr::group_by(.data[[col_date]]) |>
      dplyr::mutate(
        .size_cutoff = quantile(
          .data[[col_mktcap_lag]],
          probs = size_threshold,
          na.rm = TRUE
        )
      ) |>
      dplyr::ungroup() |>
      dplyr::filter(
        !is.na(.data[[col_mktcap_lag]]) &
          .data[[col_mktcap_lag]] >= .size_cutoff
      ) |>
      dplyr::select(-.size_cutoff)
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
    if (!col_listing_age %in% colnames(data)) {
      cli::cli_abort(c(
        "Column {.val {col_listing_age}} not found in {.arg data}.",
        "i" = "Set {.arg data_options$listing_age} to the correct column name."
      ))
    }
    n_before <- nrow(data)
    data <- data |>
      dplyr::filter(
        !is.na(.data[[col_listing_age]]) &
          .data[[col_listing_age]] >= filter_options$min_listing_age
      )
    n_dropped <- n_before - nrow(data)
    if (!quiet && n_dropped > 0) {
      cli::cli_inform(
        "Filter 'min_listing_age': removed {n_dropped} observation{?s}."
      )
    }
  }

  # positive_book_equity
  if (isTRUE(filter_options$positive_book_equity)) {
    col_be <- data_options$be
    if (!col_be %in% colnames(data)) {
      cli::cli_abort(c(
        "Column {.val {col_be}} not found in {.arg data}.",
        "i" = "Set {.arg data_options$be} to the correct column name."
      ))
    }
    n_before <- nrow(data)
    data <- data |>
      dplyr::filter(
        !is.na(.data[[col_be]]) & .data[[col_be]] > 0
      )
    n_dropped <- n_before - nrow(data)
    if (!quiet && n_dropped > 0) {
      cli::cli_inform(
        "Filter 'positive_book_equity': removed {n_dropped} observation{?s}."
      )
    }
  }

  data
}
