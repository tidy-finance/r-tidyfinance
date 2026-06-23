#' Internal Router for `domain = "Pseudo Data"`
#'
#' Single entry point invoked by [download_data()] when
#' `domain = "Pseudo Data"`. Emits the pseudo-data notice, validates the
#' requested `dataset`, and dispatches to the corresponding per-dataset
#' generator. Not exported; users access pseudo data via
#' `download_data(domain = "Pseudo Data", ...)` or the per-dataset
#' `download_data_pseudo_*()` functions.
#'
#' @param dataset A string identifying the pseudo dataset to return.
#'   Supported: `"crsp_monthly"`, `"crsp_daily"`, `"compustat_annual"`,
#'   `"compustat_quarterly"`, and `"ccm_links"`.
#' @param start_date,end_date Optional date bounds passed through to the
#'   per-dataset generator (see [download_data_pseudo_crsp()] etc.).
#' @param ... Additional arguments forwarded to the per-dataset generator
#'   (e.g. `n_assets`, `seed`, `additional_columns`, `add_ccm_links`).
#'
#' @returns A tibble whose structure matches the corresponding
#'   `download_data_wrds_*()` output.
#'
#' @keywords internal
#' @noRd
simulate_pseudo_data <- function(
  dataset = NULL,
  start_date = NULL,
  end_date = NULL,
  ...
) {
  if (is.null(dataset)) {
    cli::cli_abort("Argument {.arg dataset} is required.")
  }

  check_supported_dataset_pseudo(dataset)

  cli::cli_inform(c(
    i = paste(
      "Returning pseudo data from {.code domain = \"Pseudo Data\"}.",
      "Schema matches {.code domain = \"WRDS\"}, but values are simulated",
      "and not suitable for inference."
    )
  ))

  if (grepl("^crsp", dataset)) {
    download_data_pseudo_crsp(
      dataset = dataset,
      start_date = start_date,
      end_date = end_date,
      ...
    )
  } else if (grepl("^compustat", dataset)) {
    download_data_pseudo_compustat(
      dataset = dataset,
      start_date = start_date,
      end_date = end_date,
      ...
    )
  } else if (dataset == "ccm_links") {
    download_data_pseudo_ccm_links(...)
  }
}

#' Check if a pseudo dataset is supported
#' @noRd
check_supported_dataset_pseudo <- function(dataset) {
  supported_datasets <- c(
    "crsp_monthly",
    "crsp_daily",
    "compustat_annual",
    "compustat_quarterly",
    "ccm_links"
  )

  if (!dataset %in% supported_datasets) {
    cli::cli_abort(c(
      "Unsupported pseudo dataset: {.val {dataset}}",
      i = "Supported datasets: {.val {supported_datasets}}"
    ))
  }
}

#' Simulate an Identifier Universe for the Pseudo Panels
#'
#' Internal helper that draws a pseudo universe of stock identifiers used by
#' the `download_data_pseudo_*()` family. The universe is fully determined
#' by `seed` and `n_assets` so that calls to different pseudo datasets (CRSP,
#' Compustat, CCM links) share the same identifier mapping and join cleanly.
#'
#' Industry and exchange mixes are calibrated to the empirical frequencies of
#' the real CRSP universe; SIC codes are drawn from the conventional range for
#' the assigned industry so that downstream industry filters (e.g. excluding
#' financials at 6000-6799 or utilities at 4900-4999) drop the intended firms.
#'
#' @param n_assets Integer. Number of pseudo firms in the universe.
#' @param seed Integer. Random seed; the same `(seed, n_assets)` always yields
#'   the same universe.
#'
#' @returns A tibble with one row per pseudo firm and columns `permno`,
#'   `permco`, `gvkey`, `exchange`, `industry`, and `siccd`.
#'
#' @keywords internal
#' @noRd
simulate_pseudo_identifiers <- function(n_assets = 1000L, seed = 1234L) {
  n_assets <- as.integer(n_assets)
  if (length(n_assets) != 1L || is.na(n_assets) || n_assets <= 0L) {
    cli::cli_abort("{.arg n_assets} must be a single positive integer.")
  }

  industries <- tibble(
    industry = c(
      "Agriculture",
      "Construction",
      "Finance",
      "Manufacturing",
      "Mining",
      "Public",
      "Retail",
      "Services",
      "Transportation",
      "Utilities",
      "Wholesale"
    ),
    prob = c(
      0.00319,
      0.0113,
      0.185,
      0.339,
      0.0508,
      0.0779,
      0.0620,
      0.169,
      0.0493,
      0.0180,
      0.0357
    )
  )

  exchanges <- tibble(
    exchange = c("AMEX", "NASDAQ", "NYSE"),
    prob = c(0.113, 0.671, 0.216)
  )

  set.seed(seed)
  identifiers <- tibble(
    permno = seq_len(n_assets),
    permco = seq_len(n_assets),
    gvkey = sprintf("%06d", seq_len(n_assets) + 10000L),
    exchange = sample(
      exchanges$exchange,
      n_assets,
      replace = TRUE,
      prob = exchanges$prob
    ),
    industry = sample(
      industries$industry,
      n_assets,
      replace = TRUE,
      prob = industries$prob
    )
  ) |>
    mutate(
      siccd = dplyr::case_when(
        .data$industry == "Agriculture" ~
          sample(100:999, dplyr::n(), replace = TRUE),
        .data$industry == "Mining" ~
          sample(1000:1499, dplyr::n(), replace = TRUE),
        .data$industry == "Construction" ~
          sample(1500:1799, dplyr::n(), replace = TRUE),
        .data$industry == "Manufacturing" ~
          sample(1800:3999, dplyr::n(), replace = TRUE),
        .data$industry == "Transportation" ~
          sample(4000:4899, dplyr::n(), replace = TRUE),
        .data$industry == "Utilities" ~
          sample(4900:4999, dplyr::n(), replace = TRUE),
        .data$industry == "Wholesale" ~
          sample(5000:5199, dplyr::n(), replace = TRUE),
        .data$industry == "Retail" ~
          sample(5200:5999, dplyr::n(), replace = TRUE),
        .data$industry == "Finance" ~
          sample(6000:6799, dplyr::n(), replace = TRUE),
        .data$industry == "Services" ~
          sample(7000:8999, dplyr::n(), replace = TRUE),
        .data$industry == "Public" ~
          sample(9000:9999, dplyr::n(), replace = TRUE)
      )
    )

  identifiers
}
