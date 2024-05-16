# Define types ------------------------------------------------------------
#' List Supported Fama-French Dataset Types
#'
#' This function returns a tibble with the supported Fama-French dataset types,
#' including their names and frequencies (daily, weekly, monthly). Each dataset
#' type is associated with a specific Fama-French model (e.g., 3 factors, 5
#' factors). Additionally, it annotates each dataset with the domain
#' "Fama-French".
#'
#' @return A tibble with columns: `type` (the type of dataset), `dataset_name`
#'   (a descriptive name of the dataset), and `domain` (the domain to which the
#'   dataset belongs, always "Fama-French").
#'
#' @importFrom tibble tribble
#'
list_supported_types_ff <- function() {
  tribble(
    ~type, ~dataset_name,
    "factors_ff3_daily", "Fama/French 3 Factors [Daily]",
    "factors_ff3_weekly", "Fama/French 3 Factors [Weekly]",
    "factors_ff3_monthly", "Fama/French 3 Factors",
    "factors_ff5_daily", "Fama/French 5 Factors (2x3) [Daily]",
    "factors_ff5_monthly", "Fama/French 5 Factors (2x3)",
    "factors_ff_industry_5_monthly", "5 Industry Portfolios",
    "factors_ff_industry_5_daily", "5 Industry Portfolios [Daily]",
    "factors_ff_industry_10_monthly", "10 Industry Portfolios",
    "factors_ff_industry_10_daily", "10 Industry Portfolios [Daily]",
    "factors_ff_industry_30_monthly", "30 Industry Portfolios",
    "factors_ff_industry_30_daily", "30 Industry Portfolios [Daily]",
    "factors_ff_industry_38_monthly", "38 Industry Portfolios",
    "factors_ff_industry_38_daily", "38 Industry Portfolios [Daily]",
    "factors_ff_industry_48_monthly", "48 Industry Portfolios",
    "factors_ff_industry_48_daily", "48 Industry Portfolios [Daily]",
    "factors_ff_industry_49_monthly", "49 Industry Portfolios",
    "factors_ff_industry_49_daily", "49 Industry Portfolios [Daily]"
  ) |>
    mutate(domain = "Fama-French")
}

#' List Supported Global Q Dataset Types
#'
#' This function returns a tibble with the supported Global Q dataset types,
#' including their names and frequencies (daily, weekly, weekly week-to-week,
#' monthly, quarterly, annual). Each dataset type is associated with the Global
#' Q model, specifically the q5 factors model for the year 2022. Additionally,
#' it annotates each dataset with the domain "Global Q".
#'
#' @return A tibble with columns: `type` (the type of dataset), `dataset_name`
#'   (the file name of the dataset), and `domain` (the domain to which the
#'   dataset belongs, always "Global Q").
#'
#' @importFrom tibble tribble
#'
list_supported_types_q <- function() {
  tribble(
    ~type, ~dataset_name,
    "factors_q5_daily", "q5_factors_daily_2022.csv",
    "factors_q5_weekly", "q5_factors_weekly_2022.csv",
    "factors_q5_weekly_w2w", "q5_factors_weekly_w2w_2022.csv",
    "factors_q5_monthly", "q5_factors_monthly_2022.csv",
    "factors_q5_quarterly", "q5_factors_quarterly_2022.csv",
    "factors_q5_annual", "q5_factors_annual_2022.csv"
  ) |>
    mutate(domain = "Global Q")
}

#' List Supported Macro Predictor Dataset Types
#'
#' This function returns a tibble with the supported macro predictor dataset
#' types provided by Goyal-Welch, including their frequencies (monthly,
#' quarterly, annual). All dataset types reference the same source file
#' "PredictorData2022.xlsx" for the year 2022. Additionally, it annotates each
#' dataset with the domain "Goyal-Welch".
#'
#' @return A tibble with columns: `type` (the type of dataset), `dataset_name`
#'   (the file name of the dataset, which is the same for all types), and
#'   `domain` (the domain to which the dataset belongs, always "Goyal-Welch").
#'
#' @importFrom tibble tribble
#'
list_supported_types_macro_predictors <- function() {
  tribble(
    ~type, ~dataset_name,
    "macro_predictors_monthly", "PredictorData2022.xlsx",
    "macro_predictors_quarterly", "PredictorData2022.xlsx",
    "macro_predictors_annual", "PredictorData2022.xlsx",
  ) |>
    mutate(domain = "Goyal-Welch")
}

#' List Supported WRDS Dataset Types
#'
#' This function returns a tibble with the supported dataset types provided via
#' WRDS. Additionally, it annotates each dataset with the domain "WRDS".
#'
#' @return A tibble with columns: `type` (the type of dataset), `dataset_name`
#'   (the file name of the dataset), and `domain` (the domain to which the
#'   dataset belongs, always "WRDS").
#'
#' @importFrom tibble tribble
#'
list_supported_types_wrds <- function() {
  tribble(
    ~type, ~dataset_name,
    "wrds_crsp_monthly", "crsp.msf, crsp.msenames, crsp.msedelist",
    "wrds_crsp_daily", "crsp.dsf, crsp.msenames, crsp.msedelist",
    "wrds_compustat_annual", "comp.funda",
    "wrds_ccm_links", "crsp.ccmxpf_linktable",
    "wrds_fisd", "fisd.fisd_mergedissue, fisd.fisd_mergedissuer",
    "wrds_trace", "trace.trace_enhanced"
  ) |>
    mutate(domain = "WRDS")
}

#' List All Supported Dataset Types
#'
#' This function aggregates and returns a comprehensive tibble of all supported
#' dataset types from different domains. It includes various datasets across
#' different frequencies (daily, weekly, monthly, quarterly, annual) and models
#' (e.g., q5 factors, Fama-French 3 and 5 factors, macro predictors).
#'
#' @return A tibble aggregating all supported dataset types with columns: `type`
#'   (the type of dataset), `dataset_name` (a descriptive name or file name of
#'   the dataset), and `domain` (the domain to which the dataset belongs, e.g.,
#'   "Global Q", "Fama-French", "Goyal-Welch").
#'
#' @examples
#' list_supported_types()
#'
#' @importFrom dplyr bind_rows
#'
#' @export
list_supported_types <- function() {
  dplyr::bind_rows(
    list_supported_types_q(),
    list_supported_types_ff(),
    list_supported_types_macro_predictors(),
    list_supported_types_wrds()
  )
}
