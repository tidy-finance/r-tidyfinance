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
    col_exchange <- data_options$exchange
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
    if (!col_exchange %in% colnames(data)) {
      cli::cli_abort(c(
        "Column {.val {col_exchange}} not found in {.arg data}.",
        "i" = paste0(
          "The size quantile cutoff is computed from NYSE stocks. ",
          "Set {.arg data_options$exchange} to the correct column name."
        )
      ))
    }
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
    data <- data |>
      check_new_col(".size_cutoff") |>
      dplyr::inner_join(size_cutoffs, by = col_date) |>
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

  # exclude_negative_book_equity
  if (isTRUE(filter_options$exclude_negative_book_equity)) {
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
        paste0(
          "Filter 'exclude_negative_book_equity': ",
          "removed {n_dropped} observation{?s}."
        )
      )
    }
  }

  # exclude_negative_earnings
  if (isTRUE(filter_options$exclude_negative_earnings)) {
    col_earn <- data_options$earnings
    if (!col_earn %in% colnames(data)) {
      cli::cli_abort(c(
        "Column {.val {col_earn}} not found in {.arg data}.",
        "i" = "Set {.arg data_options$earnings} to the correct column name."
      ))
    }
    n_before <- nrow(data)
    data <- data |>
      dplyr::filter(
        !is.na(.data[[col_earn]]) & .data[[col_earn]] > 0
      )
    n_dropped <- n_before - nrow(data)
    if (!quiet && n_dropped > 0) {
      cli::cli_inform(
        paste0(
          "Filter 'exclude_negative_earnings': ",
          "removed {n_dropped} observation{?s}."
        )
      )
    }
  }

  data
}
