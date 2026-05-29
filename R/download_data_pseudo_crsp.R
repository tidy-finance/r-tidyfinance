#' Generate Pseudo CRSP Data
#'
#' Returns pseudo CRSP data with the same column layout as
#' [download_data_wrds_crsp()]. Useful for testing and for reproducing the
#' workflow of analyses that rely on CRSP without a WRDS subscription. The
#' returned values are simulated and not suitable for inference.
#'
#' Both `"crsp_monthly"` and `"crsp_daily"` are supported. The daily panel
#' uses weekdays (Monday-Friday) only; weekend dates are excluded so the
#' pseudo calendar approximates a trading-day grid.
#'
#' @param dataset A string specifying the dataset to simulate. Supported:
#'   `"crsp_monthly"` and `"crsp_daily"`.
#' @param start_date Optional. A character string or Date object in
#'   "YYYY-MM-DD" format specifying the start date for the pseudo panel.
#' @param end_date Optional. A character string or Date object in "YYYY-MM-DD"
#'   format specifying the end date for the pseudo panel.
#' @param version Accepted for API compatibility with
#'   [download_data_wrds_crsp()]; the pseudo schema follows the v2 output.
#' @param additional_columns Additional CRSP columns to include. Filled with
#'   plausible random draws so call sites that pass `additional_columns`
#'   continue to work; the values themselves are not economically meaningful.
#' @param add_ccm_links A logical indicating whether CRSP-Compustat links
#'   should be appended. When `TRUE`, the output gains a `gvkey` column whose
#'   values are derived from the same pseudo identifier universe used by
#'   [download_data_pseudo_ccm_links()].
#' @param adjust_volume Accepted for API compatibility with
#'   [download_data_wrds_crsp()]; ignored for pseudo data.
#' @param batch_size Accepted for API compatibility with
#'   [download_data_wrds_crsp()]; ignored for pseudo data.
#' @param n_assets Integer. Number of pseudo firms in the universe.
#'   Defaults to `1000`.
#' @param seed Integer. Random seed; defaults to `1234`. Identical
#'   `(seed, n_assets)` produces identical output across calls and matches the
#'   identifier universe used by [download_data_pseudo_compustat()] and
#'   [download_data_pseudo_ccm_links()].
#'
#' @returns For `"crsp_monthly"`, a tibble with columns `permno`, `date`,
#'   `calculation_date`, `ret`, `shrout`, `prc`, `primaryexch`, `siccd`,
#'   `listing_age`, `mktcap`, `mktcap_lag`, `exchange`, `industry`, and
#'   `ret_excess`. For `"crsp_daily"`, a tibble with columns `permno`, `date`,
#'   `ret`, and `ret_excess`. If `add_ccm_links = TRUE`, a `gvkey` column is
#'   appended.
#'
#' @family pseudo functions
#' @export
#'
#' @examples
#' download_data_pseudo_crsp(
#'   "crsp_monthly",
#'   start_date = "2020-01-01",
#'   end_date = "2024-12-31",
#'   n_assets = 20
#' )
#' download_data_pseudo_crsp(
#'   "crsp_daily",
#'   start_date = "2020-01-01",
#'   end_date = "2020-03-31",
#'   n_assets = 20
#' )
download_data_pseudo_crsp <- function(
  dataset = NULL,
  start_date = NULL,
  end_date = NULL,
  version = "v2",
  additional_columns = NULL,
  add_ccm_links = FALSE,
  adjust_volume = FALSE,
  batch_size = 500,
  n_assets = 1000L,
  seed = 1234L
) {
  if (is.null(dataset)) {
    cli::cli_abort("Argument {.arg dataset} is required.")
  }
  if (!dataset %in% c("crsp_monthly", "crsp_daily")) {
    cli::cli_abort(c(
      "Unsupported CRSP dataset: {.val {dataset}}",
      i = paste(
        "Supported pseudo datasets:",
        "{.val crsp_monthly}, {.val crsp_daily}."
      )
    ))
  }

  dates <- validate_dates(start_date, end_date, use_default_range = TRUE)
  start_date <- dates$start_date
  end_date <- dates$end_date

  identifiers <- simulate_pseudo_identifiers(n_assets = n_assets, seed = seed)

  if (dataset == "crsp_monthly") {
    panel <- simulate_pseudo_crsp_monthly(
      identifiers = identifiers,
      start_date = start_date,
      end_date = end_date,
      additional_columns = additional_columns,
      seed = seed
    )
  } else {
    panel <- simulate_pseudo_crsp_daily(
      identifiers = identifiers,
      start_date = start_date,
      end_date = end_date,
      additional_columns = additional_columns,
      seed = seed
    )
  }

  if (isTRUE(add_ccm_links)) {
    panel <- panel |>
      left_join(
        identifiers |> select("permno", "gvkey"),
        join_by(permno)
      )
  }

  panel
}

#' Monthly CRSP pseudo panel
#' @noRd
simulate_pseudo_crsp_monthly <- function(
  identifiers,
  start_date,
  end_date,
  additional_columns,
  seed
) {
  months <- seq(
    lubridate::floor_date(start_date, "month"),
    lubridate::floor_date(end_date, "month"),
    by = "1 month"
  )

  primaryexch_lookup <- c(NYSE = "N", AMEX = "A", NASDAQ = "Q")

  set.seed(seed + 1L)
  panel <- tidyr::expand_grid(
    identifiers,
    tibble(date = months)
  ) |>
    arrange(.data[["permno"]], .data[["date"]]) |>
    mutate(
      calculation_date = lubridate::ceiling_date(.data[["date"]], "month") - 1L,
      shrout = stats::runif(dplyr::n(), 1, 50) * 1000,
      prc = stats::runif(dplyr::n(), 1, 1000),
      ret = stats::rnorm(dplyr::n(), mean = 0.008, sd = 0.10),
      mktcap = .data[["shrout"]] * .data[["prc"]] / 1000,
      primaryexch = unname(primaryexch_lookup[.data[["exchange"]]])
    ) |>
    group_by(.data[["permno"]]) |>
    mutate(
      listing_age = seq_len(dplyr::n()) - 1L,
      mktcap_lag = dplyr::lag(.data[["mktcap"]])
    ) |>
    ungroup() |>
    mutate(
      ret_excess = pmax(
        .data[["ret"]] - stats::runif(dplyr::n(), 0, 0.004), -1
      )
    )

  if (length(additional_columns) > 0L) {
    for (col in additional_columns) {
      if (!col %in% names(panel)) {
        panel[[col]] <- stats::rnorm(nrow(panel))
      }
    }
  }

  panel |>
    select(
      "permno",
      "date",
      "calculation_date",
      "ret",
      "shrout",
      "prc",
      "primaryexch",
      "siccd",
      "listing_age",
      "mktcap",
      "mktcap_lag",
      "exchange",
      "industry",
      "ret_excess",
      dplyr::any_of(additional_columns)
    )
}

#' Daily CRSP pseudo panel (weekdays only)
#' @noRd
simulate_pseudo_crsp_daily <- function(
  identifiers,
  start_date,
  end_date,
  additional_columns,
  seed
) {
  days <- seq(start_date, end_date, by = "1 day")
  days <- days[lubridate::wday(days, week_start = 1) <= 5L]

  set.seed(seed + 2L)
  panel <- tidyr::expand_grid(
    identifiers |> select("permno"),
    tibble(date = days)
  ) |>
    arrange(.data[["permno"]], .data[["date"]]) |>
    mutate(
      ret = stats::rnorm(dplyr::n(), mean = 0.0004, sd = 0.02),
      ret_excess = pmax(
        .data[["ret"]] - stats::runif(dplyr::n(), 0, 0.0002), -1
      )
    )

  if (length(additional_columns) > 0L) {
    for (col in additional_columns) {
      if (!col %in% names(panel)) {
        panel[[col]] <- stats::rnorm(nrow(panel))
      }
    }
  }

  panel |>
    select(
      "permno",
      "date",
      "ret",
      "ret_excess",
      dplyr::any_of(additional_columns)
    )
}
