#' Generate Pseudo Compustat Data
#'
#' Returns pseudo Compustat data with the same column layout as
#' [download_data_wrds_compustat()]. Useful for testing and for reproducing
#' the workflow of analyses that rely on Compustat without a WRDS
#' subscription. The returned values are simulated and not suitable for
#' inference.
#'
#' Both `"compustat_annual"` and `"compustat_quarterly"` are supported.
#'
#' @param dataset A string specifying the dataset to simulate. Supported:
#'   `"compustat_annual"` and `"compustat_quarterly"`.
#' @param start_date Optional. A character string or Date object in
#'   "YYYY-MM-DD" format specifying the start date for the pseudo panel.
#' @param end_date Optional. A character string or Date object in "YYYY-MM-DD"
#'   format specifying the end date for the pseudo panel.
#' @param additional_columns Additional Compustat columns to include. Filled
#'   with plausible random draws so call sites that pass `additional_columns`
#'   continue to work; the values themselves are not economically meaningful.
#' @param only_usd Accepted for API compatibility with
#'   [download_data_wrds_compustat()]; the pseudo universe is treated as
#'   USD-denominated, so this argument has no effect.
#' @param n_assets Integer. Number of pseudo firms in the universe.
#'   Defaults to `1000`.
#' @param seed Integer. Random seed; defaults to `1234`. Identical
#'   `(seed, n_assets)` produces identical output across calls and matches the
#'   identifier universe used by [download_data_pseudo_crsp()] and
#'   [download_data_pseudo_ccm_links()].
#'
#' @returns For `"compustat_annual"`, a tibble with columns `gvkey`, `date`,
#'   `datadate`, the financial-statement variables `seq`, `ceq`, `at`, `lt`,
#'   `txditc`, `txdb`, `itcb`, `pstkrv`, `pstkl`, `pstk`, `capx`, `oancf`,
#'   `sale`, `cogs`, `xint`, `xsga`, `ib`, `curcd`, plus the derived `be`,
#'   `op`, `at_lag`, `inv`, and any requested `additional_columns`. For
#'   `"compustat_quarterly"`, a tibble with columns `gvkey`, `date`,
#'   `datadate`, `atq`, `ceqq`, and any requested `additional_columns`.
#'
#' @family pseudo functions
#' @export
#'
#' @examples
#' download_data_pseudo_compustat(
#'   "compustat_annual",
#'   start_date = "2020-01-01",
#'   end_date = "2024-12-31",
#'   n_assets = 20
#' )
#' download_data_pseudo_compustat(
#'   "compustat_quarterly",
#'   start_date = "2020-01-01",
#'   end_date = "2024-12-31",
#'   n_assets = 20
#' )
download_data_pseudo_compustat <- function(
  dataset = NULL,
  start_date = NULL,
  end_date = NULL,
  additional_columns = NULL,
  only_usd = FALSE,
  n_assets = 1000L,
  seed = 1234L
) {
  if (is.null(dataset)) {
    cli::cli_abort("Argument {.arg dataset} is required.")
  }
  if (!dataset %in% c("compustat_annual", "compustat_quarterly")) {
    cli::cli_abort(c(
      "Unsupported Compustat dataset: {.val {dataset}}",
      i = paste(
        "Supported pseudo datasets:",
        "{.val compustat_annual}, {.val compustat_quarterly}."
      )
    ))
  }

  dates <- validate_dates(start_date, end_date, use_default_range = TRUE)
  start_date <- dates$start_date
  end_date <- dates$end_date

  identifiers <- simulate_pseudo_identifiers(n_assets = n_assets, seed = seed)

  if (dataset == "compustat_annual") {
    simulate_pseudo_compustat_annual(
      identifiers = identifiers,
      start_date = start_date,
      end_date = end_date,
      additional_columns = additional_columns,
      seed = seed
    )
  } else {
    simulate_pseudo_compustat_quarterly(
      identifiers = identifiers,
      start_date = start_date,
      end_date = end_date,
      additional_columns = additional_columns,
      seed = seed
    )
  }
}

#' Annual Compustat pseudo panel
#' @noRd
simulate_pseudo_compustat_annual <- function(
  identifiers,
  start_date,
  end_date,
  additional_columns,
  seed
) {
  years <- seq(year(start_date), year(end_date), by = 1L)

  set.seed(seed + 4L)
  panel <- tidyr::expand_grid(
    identifiers |> select("gvkey"),
    tibble(year = years)
  ) |>
    arrange(.data$gvkey, .data$year) |>
    group_by(.data$gvkey) |>
    mutate(
      at = 100 * exp(cumsum(stats::rnorm(dplyr::n(), mean = 0.05, sd = 0.30)))
    ) |>
    ungroup() |>
    mutate(
      datadate = ymd(paste0(.data$year, "-12-31")),
      date = lubridate::floor_date(.data$datadate, "month"),
      seq = .data$at * stats::runif(dplyr::n(), 0.3, 0.7),
      ceq = .data$seq * stats::runif(dplyr::n(), 0.8, 1.0),
      lt = .data$at - .data$seq,
      txditc = .data$at * stats::runif(dplyr::n(), 0.00, 0.05),
      txdb = .data$txditc * stats::runif(dplyr::n(), 0.0, 1.0),
      itcb = .data$txditc - .data$txdb,
      pstkrv = .data$at * stats::runif(dplyr::n(), 0.0, 0.02),
      pstkl = .data$pstkrv,
      pstk = .data$pstkrv,
      capx = .data$at * stats::runif(dplyr::n(), 0.02, 0.10),
      oancf = .data$at * stats::rnorm(dplyr::n(), mean = 0.07, sd = 0.05),
      sale = .data$at * stats::runif(dplyr::n(), 0.5, 1.5),
      cogs = .data$sale * stats::runif(dplyr::n(), 0.5, 0.8),
      xsga = .data$sale * stats::runif(dplyr::n(), 0.05, 0.20),
      xint = .data$at * stats::runif(dplyr::n(), 0.005, 0.03),
      ib = .data$at * stats::rnorm(dplyr::n(), mean = 0.05, sd = 0.10),
      curcd = "USD"
    )

  if (length(additional_columns) > 0L) {
    for (col in additional_columns) {
      if (!col %in% names(panel)) {
        panel[[col]] <- stats::rnorm(nrow(panel))
      }
    }
  }

  panel |>
    mutate(
      be = coalesce(
        .data$seq,
        .data$ceq + .data$pstk,
        .data$at - .data$lt
      ) +
        coalesce(.data$txditc, .data$txdb + .data$itcb, 0) -
        coalesce(.data$pstkrv, .data$pstkl, .data$pstk, 0),
      op = (.data$sale -
        coalesce(.data$cogs, 0) -
        coalesce(.data$xsga, 0) -
        coalesce(.data$xint, 0)) /
        .data$be
    ) |>
    left_join(
      panel |>
        select("gvkey", "year", at_lag = "at") |>
        mutate(year = .data$year + 1L),
      by = c("gvkey", "year")
    ) |>
    mutate(
      inv = .data$at / .data$at_lag - 1,
      inv = if_else(.data$at_lag <= 0, NA_real_, .data$inv)
    ) |>
    select("gvkey", "date", "datadate", dplyr::everything(), -"year")
}

#' Quarterly Compustat pseudo panel
#' @noRd
simulate_pseudo_compustat_quarterly <- function(
  identifiers,
  start_date,
  end_date,
  additional_columns,
  seed
) {
  quarter_starts <- seq(
    lubridate::floor_date(start_date, "quarter"),
    lubridate::floor_date(end_date, "quarter"),
    by = "3 months"
  )
  quarter_ends <- lubridate::ceiling_date(
    quarter_starts, "quarter", change_on_boundary = TRUE
  ) - 1L

  set.seed(seed + 3L)
  panel <- tidyr::expand_grid(
    identifiers |> select("gvkey"),
    tibble(datadate = quarter_ends)
  ) |>
    arrange(.data$gvkey, .data$datadate) |>
    group_by(.data$gvkey) |>
    mutate(
      atq = 100 * exp(cumsum(stats::rnorm(dplyr::n(), mean = 0.012, sd = 0.15)))
    ) |>
    ungroup() |>
    mutate(
      date = lubridate::floor_date(.data$datadate, "month"),
      ceqq = .data$atq * stats::runif(dplyr::n(), 0.2, 0.6)
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
      "gvkey",
      "date",
      "datadate",
      "atq",
      "ceqq",
      dplyr::any_of(additional_columns)
    )
}
