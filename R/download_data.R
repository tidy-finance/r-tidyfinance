# Define types ------------------------------------------------------------
#' List Supported Fama-French Dataset Types
#'
#' This function returns a tibble with the supported Fama-French dataset types,
#' including their names and frequencies (daily, weekly, monthly). Each dataset type
#' is associated with a specific Fama-French model (e.g., 3 factors, 5 factors).
#' Additionally, it annotates each dataset with the domain "Fama-French".
#'
#' @return A tibble with columns: `type` (the type of dataset), `dataset_name`
#' (a descriptive name of the dataset), and `domain` (the domain to which the dataset belongs, always "Fama-French").
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
    "factors_ff5_monthly", "Fama/French 5 Factors (2x3)"
  ) |>
    mutate(domain = "Fama-French")
}

#' List Supported Global Q Dataset Types
#'
#' This function returns a tibble with the supported Global Q dataset types,
#' including their names and frequencies (daily, weekly, weekly week-to-week, monthly, quarterly, annual).
#' Each dataset type is associated with the Global Q model, specifically the q5 factors model for the year 2022.
#' Additionally, it annotates each dataset with the domain "Global Q".
#'
#' @return A tibble with columns: `type` (the type of dataset), `dataset_name`
#' (the file name of the dataset), and `domain` (the domain to which the dataset belongs, always "Global Q").
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
#' This function returns a tibble with the supported macro predictor dataset types
#' provided by Welch-Goyal, including their frequencies (monthly, quarterly, annual).
#' All dataset types reference the same source file "PredictorData2022.xlsx" for the year 2022.
#' Additionally, it annotates each dataset with the domain "Welch-Goyal".
#'
#' @return A tibble with columns: `type` (the type of dataset), `dataset_name`
#' (the file name of the dataset, which is the same for all types), and `domain`
#' (the domain to which the dataset belongs, always "Welch-Goyal").
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
    mutate(domain = "Welch-Goyal")
}

#' List All Supported Dataset Types
#'
#' This function aggregates and returns a comprehensive tibble of all supported dataset types
#' from different domains. It includes various datasets
#' across different frequencies (daily, weekly, monthly, quarterly, annual) and models
#' (e.g., q5 factors, Fama/French 3 and 5 factors, macro predictors).
#'
#' @return A tibble aggregating all supported dataset types with columns: `type` (the type of dataset),
#' `dataset_name` (a descriptive name or file name of the dataset), and `domain` (the domain to which the
#' dataset belongs, e.g., "Global Q", "Fama-French", "Welch-Goyal").
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
    list_supported_types_macro_predictors()
  )
}

#' Check if a Dataset Type is Supported
#'
#' This function checks if a given dataset type is supported by verifying against
#' a list of all supported dataset types from different domains.
#' If the specified type is not supported, it stops execution and returns an error message
#' listing all supported types.
#'
#' @param type The dataset type to check for support.
#' @return Does not return a value; instead, it either passes silently if the type is supported
#' or stops execution with an error message if the type is unsupported.
#'
check_supported_type <- function(type) {
  supported_types <- list_supported_types()
  if (!any(type %in% supported_types$type)) {
    stop("Unsupported type specified. Choose one of the following: ",
         paste(supported_types$type, collapse = ", "))
  }
}

# Main function -----------------------------------------------------------
#' Download and Process Data Based on Type
#'
#' Downloads and processes data based on the specified type (e.g., Fama-French factors, Global Q factors,
#' or macro predictors), and date range. This function checks if the specified type is supported and
#' then delegates to the appropriate function for downloading and processing the data.
#'
#' @param type The type of dataset to download, indicating either factor data or macroeconomic predictors.
#' @param start_date The start date for filtering the data, in "YYYY-MM-DD" format.
#' @param end_date The end date for filtering the data, in "YYYY-MM-DD" format.
#' @return A tibble with processed data, including dates and the relevant financial metrics,
#' filtered by the specified date range.
#'
#' @examples
#' download_data("factors_ff3_monthly", "2000-01-01", "2020-12-31")
#' download_data("macro_predictors_monthly", "2000-01-01", "2020-12-31")
#'
#' @export
download_data <- function(type, start_date, end_date) {

  check_supported_type(type)

  if (grepl("factors", type)) {
    processed_data <- download_data_factors(type, start_date, end_date)
  }

  if (grepl("macro_predictors", type)) {
    processed_data <- download_data_macro_predictors(type, start_date, end_date)
  }

  processed_data
}

# Factors -----------------------------------------------------------------
#' Download and Process Factor Data
#'
#' Downloads and processes factor data based on the specified type (Fama-French or Global Q),
#' and date range. This function delegates to specific functions based on the type of factors requested:
#' Fama-French or Global Q. It checks if the specified type is supported before proceeding with
#' the download and processing.
#'
#' @param type The type of dataset to download, indicating the factor model and frequency.
#' @param start_date The start date for filtering the data, in "YYYY-MM-DD" format.
#' @param end_date The end date for filtering the data, in "YYYY-MM-DD" format.
#' @return A tibble with processed factor data, including dates, risk-free rates, market excess returns,
#' and other factors, filtered by the specified date range.
#'
#' @examples
#' download_data_factors("factors_ff3_monthly", "2000-01-01", "2020-12-31")
#' download_data_factors("factors_q5_daily", "2020-01-01", "2020-12-31")
#'
#' @export
download_data_factors <- function(type, start_date, end_date) {

  check_supported_type(type)

  if (grepl("factors_ff", type)) {
    processed_data <- download_data_factors_ff(type, start_date, end_date)
  }
  if (grepl("factors_q", type)) {
    processed_data <- download_data_factors_q(type, start_date, end_date)
  }

  processed_data
}

#' Download and Process Fama-French Factor Data
#'
#' Downloads and processes Fama-French factor data based on the specified type (e.g., "factors_ff3_monthly"),
#' and date range. The function first checks if the specified type is supported and requires the 'frenchdata'
#' package to download the data. It processes the raw data into a structured format, including date conversion,
#' scaling factor values, and filtering by the specified date range.
#'
#' @param type The type of dataset to download, corresponding to the specific Fama-French model and frequency.
#' @param start_date The start date for filtering the data, in "YYYY-MM-DD" format.
#' @param end_date The end date for filtering the data, in "YYYY-MM-DD" format.
#' @return A tibble with processed factor data, including the date, risk-free rate, market excess return, and other factors, filtered by the specified date range.
#'
#' @examples
#' download_data_factors_ff("factors_ff3_monthly", "2000-01-01", "2020-12-31")
#'
#' @import dplyr
#' @importFrom lubridate ymd floor_date
#'
#' @export
download_data_factors_ff <- function(type, start_date, end_date) {

  check_supported_type(type)

  check_if_package_installed("frenchdata", type)

  factors_ff_types <- list_supported_types_ff()
  dataset <- factors_ff_types$dataset_name[factors_ff_types$type == type]

  raw_data <- suppressMessages(frenchdata::download_french_data(dataset))
  raw_data <- raw_data$subsets$data[[1]]

  if (grepl("monthly", type)) {
    processed_data <- raw_data |>
      mutate(date = lubridate::floor_date(lubridate::ymd(paste0(date, "01")), "month"))
  } else {
    processed_data <- raw_data |>
      mutate(date = lubridate::ymd(date))
  }

  processed_data <-  processed_data |>
    mutate(
      across(-c(date), ~as.numeric(.) / 100)
    ) |>
    rename_with(tolower) |>
    rename(mkt_excess = `mkt-rf`) |>
    filter(date >= start_date & date <= end_date)  |>
    select(date, risk_free = rf, mkt_excess, everything())

  processed_data
}

#' Download and Process Global Q Factor Data
#'
#' Downloads and processes Global Q factor data based on the specified type (daily, monthly, etc.),
#' date range, and source URL. The function first checks if the specified type is supported,
#' identifies the dataset name from the supported types, then downloads and processes the data from
#' the provided URL. The processing includes date conversion, renaming variables to a standardized
#' format, scaling factor values, and filtering by the specified date range.
#'
#' @param type The type of dataset to download (e.g., "factors_q5_daily", "factors_q5_monthly").
#' @param start_date The start date for filtering the data, in "YYYY-MM-DD" format.
#' @param end_date The end date for filtering the data, in "YYYY-MM-DD" format.
#' @param url The base URL from which to download the dataset files, with a specific path for Global Q datasets.
#' @return A tibble with processed factor data, including the date, risk-free rate, market excess return, and other factors, filtered by the specified date range.
#'
#' @examples
#' download_data_factors_q("factors_q5_daily", "2020-01-01", "2020-12-31")
#'
#' @import dplyr
#' @importFrom lubridate ymd
#' @importFrom utils read.csv
#'
#' @export
download_data_factors_q <- function(type, start_date, end_date, url = "http://global-q.org/uploads/1/2/2/6/122679606/") {

  check_supported_type(type)

  factors_q_types <- list_supported_types_q()
  dataset <- factors_q_types$dataset_name[factors_q_types$type == type]
  raw_data <- suppressMessages(utils::read.csv(paste0(url, dataset)) |> as_tibble())

  if (grepl("monthly", type)) {
    processed_data <- raw_data |>
      mutate(date = lubridate::ymd(paste(year, month, "01", sep = "-"))) |>
      select(-c(year, month))
  }
  if (grepl("daily", type)) {
    processed_data <- raw_data |>
      mutate(DATE = lubridate::ymd(DATE))
  }

  processed_data <- processed_data |>
    rename_with(~sub("R_", "", .)) |>
    rename_with(~tolower(.)) |>
    mutate(across(-date, ~. / 100)) |>
    filter(date >= start_date & date <= end_date) |>
    select(date, risk_free = f, mkt_excess = mkt, everything())

  processed_data
}

# Macro predictors --------------------------------------------------------
#' Download and Process Macro Predictor Data
#'
#' Downloads and processes macroeconomic predictor data based on the specified type (monthly, quarterly,
#' or annual), date range, and source URL. The function first checks if the specified type is supported,
#' then downloads the data from the provided URL (defaulting to a Google Sheets export link). It processes
#' the raw data into a structured format, calculating additional financial metrics and filtering by the
#' specified date range.
#'
#' @param type The type of dataset to download ("macro_predictors_monthly", "macro_predictors_quarterly",
#' "macro_predictors_annual").
#' @param start_date The start date for filtering the data, in "YYYY-MM-DD" format.
#' @param end_date The end date for filtering the data, in "YYYY-MM-DD" format.
#' @param url The URL from which to download the dataset, with a default Google Sheets export link.
#' @return A tibble with processed data, filtered by the specified date range and including financial metrics.
#'
#' @examples
#' download_data_macro_predictors("macro_predictors_monthly", "2000-01-01", "2020-12-31")
#'
#' @import dplyr
#' @importFrom tidyr drop_na
#' @importFrom lubridate ym
#' @importFrom utils download.file
#'
#' @export
download_data_macro_predictors <- function(type, start_date, end_date, url = "https://docs.google.com/spreadsheets/d/1g4LOaRj4TvwJr9RIaA_nwrXXWTOy46bP") {

  check_supported_type(type)

  check_if_package_installed("readxl", type)

  temporary_file <- tempfile()

  utils::download.file(
    url = paste0(url, "/export?format=xlsx"),
    destfile = temporary_file,
    mode = "wb",
    quiet = TRUE
  )

  if (grepl("monthly", type)) {
    raw_data <- suppressMessages(readxl::read_xlsx(temporary_file, sheet = "Monthly"))
    processed_data <- raw_data |>
      mutate(date = lubridate::ym(yyyymm))
  }
  if (grepl("quarterly", type)) {
    raw_data <- suppressMessages(readxl::read_xlsx(temporary_file, sheet = "Quarterly"))
    processed_data <- raw_data |>
      mutate(
        year = substr(yyyyq, 1, 4),
        quarter = substr(yyyyq, 5, 5),
        month = as.integer(quarter)*3-2,
        date = as.Date(paste0(year, "-", month, "-01"))
      )
  }
  if (grepl("annual", type)) {
    raw_data <- suppressMessages(readxl::read_xlsx(temporary_file, sheet = "Annual"))
    processed_data <- raw_data |>
      mutate(date = as.Date(paste0(yyyy, "-01-01")))
  }

  processed_data <- processed_data |>
    mutate(across(where(is.character), as.numeric)) |>
    mutate(
      IndexDiv = Index + D12,
      logret = log(IndexDiv) - log(lag(IndexDiv)),
      rp_div = lead(logret - Rfree, 1),
      dp = log(D12) - log(Index),
      dy = log(D12) - log(lag(Index)),
      ep = log(E12) - log(Index),
      de = log(D12) - log(E12),
      tms = lty - tbl,
      dfy = BAA - AAA
    ) |>
    select(date, rp_div, dp, dy, ep, de, svar, bm = `b/m`, ntis, tbl, lty, ltr,
           tms, dfy, infl
    ) |>
    filter(date >= start_date & date <= end_date) |>
    tidyr::drop_na()

  file.remove(temporary_file)

  processed_data
}

# Set global variables ------------------------------------------------
utils::globalVariables(
  c(
    "date", "rp_div", "dp", "dy", "ep", "de", "svar", "bm", "b/m", "ntis", "tbl", "lty", "ltr",
    "tms", "dfy", "infl", "AAA", "BAA", "D12", "DATE", "E12", "Index", "IndexDiv", "Rfree",
    "mkt-rf", "mkt_excess", "rf", "year", "month", "f", "mkt", "yyyymm", "yyyyq", "quarter",
    "yyyy", "logret"
  )
)
