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
    "factors_ff5_monthly", "Fama/French 5 Factors (2x3)"
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

#' Check if a Dataset Type is Supported
#'
#' This function checks if a given dataset type is supported by verifying
#' against a list of all supported dataset types from different domains. If the
#' specified type is not supported, it stops execution and returns an error
#' message listing all supported types.
#'
#' @param type The dataset type to check for support.
#'
#' @return Does not return a value; instead, it either passes silently if the
#'   type is supported or stops execution with an error message if the type is
#'   unsupported.
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
#' Downloads and processes data based on the specified type (e.g., Fama-French
#' factors, Global Q factors, or macro predictors), and date range. This
#' function checks if the specified type is supported and then delegates to the
#' appropriate function for downloading and processing the data.
#'
#' @param type The type of dataset to download, indicating either factor data or
#'   macroeconomic predictors.
#' @param start_date The start date for filtering the data, in "YYYY-MM-DD"
#'   format.
#' @param end_date The end date for filtering the data, in "YYYY-MM-DD" format.
#'
#' @return A tibble with processed data, including dates and the relevant
#'   financial metrics, filtered by the specified date range.
#'
#' @examples
#' \donttest{
#'   download_data("factors_ff3_monthly", "2000-01-01", "2020-12-31")
#'   download_data("macro_predictors_monthly", "2000-01-01", "2020-12-31")
#' }
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

  if (grepl("wrds", type)) {
    processed_data <- download_data_wrds(type, start_date, end_date)
  }

  processed_data
}

# Factors -----------------------------------------------------------------
#' Download and Process Factor Data
#'
#' Downloads and processes factor data based on the specified type (Fama-French
#' or Global Q), and date range. This function delegates to specific functions
#' based on the type of factors requested: Fama-French or Global Q. It checks if
#' the specified type is supported before proceeding with the download and
#' processing.
#'
#' @param type The type of dataset to download, indicating the factor model and
#'   frequency.
#' @param start_date The start date for filtering the data, in "YYYY-MM-DD"
#'   format.
#' @param end_date The end date for filtering the data, in "YYYY-MM-DD" format.
#'
#' @return A tibble with processed factor data, including dates, risk-free
#'   rates, market excess returns, and other factors, filtered by the specified
#'   date range.
#'
#' @examples
#' \donttest{
#'   download_data_factors("factors_ff3_monthly", "2000-01-01", "2020-12-31")
#'   download_data_factors("factors_q5_daily", "2020-01-01", "2020-12-31")
#' }
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
#' Downloads and processes Fama-French factor data based on the specified type
#' (e.g., "factors_ff3_monthly"), and date range. The function first checks if
#' the specified type is supported and requires the 'frenchdata' package to
#' download the data. It processes the raw data into a structured format,
#' including date conversion, scaling factor values, and filtering by the
#' specified date range.
#'
#' @param type The type of dataset to download, corresponding to the specific
#'   Fama-French model and frequency.
#' @param start_date The start date for filtering the data, in "YYYY-MM-DD"
#'   format.
#' @param end_date The end date for filtering the data, in "YYYY-MM-DD" format.
#'
#' @return A tibble with processed factor data, including the date, risk-free
#'   rate, market excess return, and other factors, filtered by the specified
#'   date range.
#'
#' @examples
#' \donttest{
#'   download_data_factors_ff("factors_ff3_monthly", "2000-01-01", "2020-12-31")
#' }
#'
#' @import dplyr
#' @importFrom lubridate ymd floor_date
#'
#' @export
download_data_factors_ff <- function(type, start_date, end_date) {

  check_supported_type(type)

  check_if_package_installed("frenchdata", type)

  download_french_data <- getNamespace("frenchdata")$download_french_data

  factors_ff_types <- list_supported_types_ff()
  dataset <- factors_ff_types$dataset_name[factors_ff_types$type == type]

  raw_data <- suppressMessages(download_french_data(dataset))
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
#' Downloads and processes Global Q factor data based on the specified type
#' (daily, monthly, etc.), date range, and source URL. The function first checks
#' if the specified type is supported, identifies the dataset name from the
#' supported types, then downloads and processes the data from the provided URL.
#' The processing includes date conversion, renaming variables to a standardized
#' format, scaling factor values, and filtering by the specified date range.
#'
#' @param type The type of dataset to download (e.g., "factors_q5_daily",
#'   "factors_q5_monthly").
#' @param start_date The start date for filtering the data, in "YYYY-MM-DD"
#'   format.
#' @param end_date The end date for filtering the data, in "YYYY-MM-DD" format.
#' @param url The base URL from which to download the dataset files, with a
#'   specific path for Global Q datasets.
#'
#' @return A tibble with processed factor data, including the date, risk-free
#'   rate, market excess return, and other factors, filtered by the specified
#'   date range.
#'
#' @examples
#' \donttest{
#'   download_data_factors_q("factors_q5_daily", "2020-01-01", "2020-12-31")
#' }
#'
#' @import dplyr
#' @importFrom lubridate ymd
#' @importFrom utils read.csv
#'
#' @export
download_data_factors_q <- function(
    type, start_date, end_date, url = "http://global-q.org/uploads/1/2/2/6/122679606/"
  ) {

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
#' Downloads and processes macroeconomic predictor data based on the specified
#' type (monthly, quarterly, or annual), date range, and source URL. The
#' function first checks if the specified type is supported, then downloads the
#' data from the provided URL (defaulting to a Google Sheets export link). It
#' processes the raw data into a structured format, calculating additional
#' financial metrics and filtering by the specified date range.
#'
#' @param type The type of dataset to download ("macro_predictors_monthly",
#'   "macro_predictors_quarterly", "macro_predictors_annual").
#' @param start_date The start date for filtering the data, in "YYYY-MM-DD"
#'   format.
#' @param end_date The end date for filtering the data, in "YYYY-MM-DD" format.
#' @param url The URL from which to download the dataset, with a default Google
#'   Sheets export link.
#'
#' @return A tibble with processed data, filtered by the specified date range
#'   and including financial metrics.
#'
#' @examples
#' \donttest{
#'   download_data_macro_predictors("macro_predictors_monthly", "2000-01-01", "2020-12-31")
#' }
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

  read_xlsx <- getNamespace("readxl")$read_xlsx

  temporary_file <- tempfile()

  utils::download.file(
    url = paste0(url, "/export?format=xlsx"),
    destfile = temporary_file,
    mode = "wb",
    quiet = TRUE
  )

  if (grepl("monthly", type)) {
    raw_data <- suppressMessages(read_xlsx(temporary_file, sheet = "Monthly"))
    processed_data <- raw_data |>
      mutate(date = lubridate::ym(yyyymm))
  }
  if (grepl("quarterly", type)) {
    raw_data <- suppressMessages(read_xlsx(temporary_file, sheet = "Quarterly"))
    processed_data <- raw_data |>
      mutate(
        year = substr(yyyyq, 1, 4),
        quarter = substr(yyyyq, 5, 5),
        month = as.integer(quarter)*3-2,
        date = as.Date(paste0(year, "-", month, "-01"))
      )
  }
  if (grepl("annual", type)) {
    raw_data <- suppressMessages(read_xlsx(temporary_file, sheet = "Annual"))
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

# WRDS data -----------------------------------------------------------
#' Download Data from WRDS
#'
#' This function acts as a wrapper to download data from various WRDS datasets
#' including CRSP, Compustat, and CCM links based on the specified type. It is
#' designed to handle different data types by redirecting to the appropriate
#' specific data download function.
#'
#' @param type A string specifying the type of data to download. It should match
#'   one of the predefined patterns to indicate the dataset: "wrds_crsp" for
#'   CRSP data, "wrds_compustat" for Compustat data, or "wrds_ccm_links" for CCM
#'   links data.
#' @param start_date A date in 'YYYY-MM-DD' format indicating the start of the
#'   period for which data is requested.
#' @param end_date A date in 'YYYY-MM-DD' format indicating the end of the
#'   period for which data is requested.
#'
#' @return A data frame containing the requested data, with the structure and
#'   contents depending on the specified `type`.
#'
#' @examples
#' \donttest{
#'   crsp_monthly <- download_data_wrds("wrds_crsp_monthly", "2000-01-01", "2020-12-31")
#'   compustat_annual <- download_data_wrds("wrds_compustat_annual", "2000-01-01", "2020-12-31")
#'   ccm_links <- download_data_wrds("wrds_ccm_links", "2000-01-01", "2020-12-31")
#' }
#'
#' @export
download_data_wrds <- function(type, start_date, end_date) {

  check_supported_type(type)

  if (grepl("wrds_crsp", type)) {
    processed_data <- download_data_wrds_crsp(type, start_date, end_date)
  }
  if (grepl("wrds_compustat", type)) {
    processed_data <- download_data_wrds_compustat(type, start_date, end_date)
  }
  if (grepl("wrds_ccm_links", type)) {
    processed_data <- download_data_wrds_ccm_links()
  }

  processed_data

}

#' Download Data from WRDS CRSP
#'
#' This function downloads and processes stock return data from the CRSP
#' database for a specified period. Users can choose between monthly and daily
#' data types. The function also adjusts returns for delisting and calculates
#' market capitalization and excess returns over the risk-free rate.
#'
#' @param type A string specifying the type of CRSP data to download:
#'   "crsp_monthly" or "crsp_daily".
#' @param start_date A Date object or string specifying the start date of the
#'   period for which data is requested.
#' @param end_date A Date object or string specifying the end date of the
#'   period.
#' @param batch_size An optional integer specifying the batch size for
#'   processing daily data, with a default of 500.
#' @param version An optional character specifying which CRSP version to use. "v2" (the default) uses the updated second version of CRSP, and "v1" downloads the legacy version of CRSP.
#' @param ... Additional arguments to be passed to underlying functions.
#'
#' @return A data frame containing CRSP stock returns, adjusted for delistings,
#'   along with calculated market capitalization and excess returns over the
#'   risk-free rate. The structure of the returned data frame depends on the
#'   selected data type.
#'
#' @examples
#' \donttest{
#'   crsp_monthly <- download_data_wrds_crsp("wrds_crsp_monthly", "2020-11-01", "2020-12-31")
#'   crsp_daily <- download_data_wrds_crsp("wrds_crsp_daily", "2020-12-01", "2020-12-31")
#' }
#'
#' @import dplyr
#' @import tidyr
#' @import lubridate
#'
#' @export
download_data_wrds_crsp <- function(type, start_date, end_date, batch_size = 500, version = "v2", ...) {

  if (!(version %in% c("v1", "v2"))) stop("Parameter version must be equal to v1 or v2.")

  check_if_package_installed("dbplyr", type)

  in_schema <- getNamespace("dbplyr")$in_schema

  con <- get_wrds_connection()

  if (grepl("crsp_monthly", type)) {

    if (version == "v1") {

      msf_db <- tbl(con, in_schema("crsp", "msf"))
      msenames_db <- tbl(con, in_schema("crsp", "msenames"))
      msedelist_db <- tbl(con, in_schema("crsp", "msedelist"))

      crsp_monthly <- msf_db |>
        filter(date >= start_date & date <= end_date) |>
        inner_join(
          msenames_db |>
            filter(shrcd %in% c(10, 11)) |>
            select(permno, exchcd, siccd, namedt, nameendt),
          join_by(permno)
        ) |>
        filter(date >= namedt & date <= nameendt) |>
        mutate(month = floor_date(date, "month")) |>
        left_join(
          msedelist_db |>
            select(permno, dlstdt, dlret, dlstcd) |>
            mutate(month = floor_date(dlstdt, "month")),
          join_by(permno, month)
        ) |>
        select(
          permno, date, month, ret, shrout, altprc,
          exchcd, siccd, dlret, dlstcd,
          ...
        ) |>
        collect() |>
        mutate(
          month = ymd(month),
          shrout = shrout * 1000
        )

      disconnection_connection(con)

      crsp_monthly <- crsp_monthly |>
        mutate(
          mktcap = abs(shrout * altprc) / 10^6,
          mktcap = na_if(mktcap, 0)
        )

      mktcap_lag <- crsp_monthly |>
        mutate(month = month %m+% months(1)) |>
        select(permno, month, mktcap_lag = mktcap)

      crsp_monthly <- crsp_monthly |>
        left_join(mktcap_lag, join_by(permno, month))

      crsp_monthly <- crsp_monthly |>
        mutate(exchange = case_when(
          exchcd %in% c(1, 31) ~ "NYSE",
          exchcd %in% c(2, 32) ~ "AMEX",
          exchcd %in% c(3, 33) ~ "NASDAQ",
          .default = "Other"
        ))

      crsp_monthly <- crsp_monthly |>
        mutate(industry = case_when(
          siccd >= 1 & siccd <= 999 ~ "Agriculture",
          siccd >= 1000 & siccd <= 1499 ~ "Mining",
          siccd >= 1500 & siccd <= 1799 ~ "Construction",
          siccd >= 2000 & siccd <= 3999 ~ "Manufacturing",
          siccd >= 4000 & siccd <= 4899 ~ "Transportation",
          siccd >= 4900 & siccd <= 4999 ~ "Utilities",
          siccd >= 5000 & siccd <= 5199 ~ "Wholesale",
          siccd >= 5200 & siccd <= 5999 ~ "Retail",
          siccd >= 6000 & siccd <= 6799 ~ "Finance",
          siccd >= 7000 & siccd <= 8999 ~ "Services",
          siccd >= 9000 & siccd <= 9999 ~ "Public",
          .default = "Missing"
        ))

      crsp_monthly <- crsp_monthly |>
        mutate(ret_adj = case_when(
          is.na(dlstcd) ~ ret,
          !is.na(dlstcd) & !is.na(dlret) ~ dlret,
          dlstcd %in% c(500, 520, 580, 584) |
            (dlstcd >= 551 & dlstcd <= 574) ~ -0.30,
          dlstcd == 100 ~ ret,
          .default = -1
        )) |>
        select(-c(dlret, dlstcd))

      factors_ff3_monthly <- download_data_factors_ff(
        "factors_ff3_monthly", start_date, end_date
      ) |>
        rename(month = date)

      crsp_monthly <- crsp_monthly |>
        left_join(factors_ff3_monthly,
                  join_by(month)
        ) |>
        mutate(
          ret_excess = ret_adj - risk_free,
          ret_excess = pmax(ret_excess, -1)
        ) |>
        select(-risk_free, -hml, -smb)

      processed_data <- crsp_monthly |>
        drop_na(ret_excess, mktcap, mktcap_lag)

    } else {

      msf_db <- tbl(con, in_schema("crsp", "msf_v2"))
      stksecurityinfohist_db <- tbl(con, in_schema("crsp", "stksecurityinfohist"))

      crsp_monthly <- msf_db |>
        filter(mthcaldt >= start_date & mthcaldt <= end_date) |>
        inner_join(
          stksecurityinfohist_db |>
            filter(sharetype == "NS" &
                     securitytype == "EQTY" &
                     securitysubtype == "COM" &
                     usincflg == "Y" &
                     issuertype %in% c("ACOR", "CORP")) |>
            select(permno, secinfostartdt, secinfoenddt),
          join_by(permno)
        ) |>
        filter(mthcaldt >= secinfostartdt & mthcaldt <= secinfoenddt) |>
        mutate(month = floor_date(mthcaldt, "month")) |>
        select(
          permno,
          date = mthcaldt,
          month,
          ret = mthret,
          shrout,
          prc = mthprc,
          primaryexch,
          siccd
        ) |>
        collect() |>
        mutate(
          month = ymd(month),
          shrout = shrout * 1000
        )

      disconnection_connection(con)

      crsp_monthly <- crsp_monthly |>
        mutate(
          mktcap = shrout * prc / 10^6,
          mktcap = na_if(mktcap, 0)
        )

      mktcap_lag <- crsp_monthly |>
        mutate(month = month %m+% months(1)) |>
        select(permno, month, mktcap_lag = mktcap)

      crsp_monthly <- crsp_monthly |>
        left_join(mktcap_lag, join_by(permno, month))

      crsp_monthly <- crsp_monthly |>
        mutate(exchange = case_when(
          primaryexch == "N" ~ "NYSE",
          primaryexch == "A" ~ "AMEX",
          primaryexch == "Q" ~ "NASDAQ",
          .default = "Other"
        ))

      crsp_monthly <- crsp_monthly |>
        mutate(industry = case_when(
          siccd >= 1 & siccd <= 999 ~ "Agriculture",
          siccd >= 1000 & siccd <= 1499 ~ "Mining",
          siccd >= 1500 & siccd <= 1799 ~ "Construction",
          siccd >= 2000 & siccd <= 3999 ~ "Manufacturing",
          siccd >= 4000 & siccd <= 4899 ~ "Transportation",
          siccd >= 4900 & siccd <= 4999 ~ "Utilities",
          siccd >= 5000 & siccd <= 5199 ~ "Wholesale",
          siccd >= 5200 & siccd <= 5999 ~ "Retail",
          siccd >= 6000 & siccd <= 6799 ~ "Finance",
          siccd >= 7000 & siccd <= 8999 ~ "Services",
          siccd >= 9000 & siccd <= 9999 ~ "Public",
          .default = "Missing"
        ))

      factors_ff3_monthly <- download_data_factors_ff(
        "factors_ff3_monthly", start_date, end_date
      ) |>
        rename(month = date)

      crsp_monthly <- crsp_monthly |>
        left_join(factors_ff3_monthly,
                  join_by(month)
        ) |>
        mutate(
          ret_excess = ret - risk_free,
          ret_excess = pmax(ret_excess, -1)
        ) |>
        select(-risk_free, -hml, -smb)

      processed_data <- crsp_monthly |>
        drop_na(ret_excess, mktcap, mktcap_lag)
    }
  }

  if (grepl("crsp_daily", type)) {

    if (version == "v1") {

      dsf_db <- tbl(con, in_schema("crsp", "dsf")) |>
        filter(date >= start_date & date <= end_date)
      msenames_db <- tbl(con, in_schema("crsp", "msenames"))
      msedelist_db <- tbl(con, in_schema("crsp", "msedelist"))

      permnos <- dsf_db |>
        distinct(permno) |>
        pull()

      factors_ff3_daily <- download_data_factors_ff(
        "factors_ff3_daily", start_date, end_date
      )

      batches <- ceiling(length(permnos) / batch_size)

      crsp_daily_list <- list()

      for (j in 1:batches) {

        permno_batch <- permnos[
          ((j - 1) * batch_size + 1):min(j * batch_size, length(permnos))
        ]

        crsp_daily_sub <- dsf_db |>
          filter(permno %in% permno_batch) |>
          select(permno, date, ret) |>
          collect() |>
          drop_na()

        if (nrow(crsp_daily_sub) > 0) {

          msedelist_sub <- msedelist_db |>
            filter(permno %in% permno_batch) |>
            select(permno, dlstdt, dlret) |>
            collect() |>
            drop_na()

          crsp_daily_sub <- crsp_daily_sub |>
            left_join(msedelist_sub, join_by(permno, date == dlstdt)) |>
            bind_rows(msedelist_sub |>
                        anti_join(crsp_daily_sub,
                                  join_by(permno, dlstdt == date))) |>
            mutate(ret = if_else(!is.na(dlret), dlret, ret),
                   date = if_else(!is.na(dlstdt), dlstdt, date)) |>
            select(-c(dlret, dlstdt)) |>
            left_join(msedelist_sub |>
                        select(permno, dlstdt), join_by(permno)) |>
            mutate(dlstdt = replace_na(dlstdt, as.Date(end_date))) |>
            filter(date <= dlstdt) |>
            select(-dlstdt)

          crsp_daily_list[[j]] <- crsp_daily_sub |>
            mutate(month = floor_date(date, "month")) |>
            left_join(factors_ff3_daily |>
                        select(date, risk_free), join_by(date)) |>
            mutate(
              ret_excess = ret - risk_free,
              ret_excess = pmax(ret_excess, -1)
            ) |>
            select(permno, date, month, ret, ret_excess)

        }
      }

      disconnection_connection(con)

      processed_data <- bind_rows(crsp_daily_list)

    } else {

      dsf_db <- tbl(con, in_schema("crsp", "dsf_v2"))

      permnos <- dsf_db |>
        distinct(permno) |>
        pull()

      factors_ff3_daily <- download_data_factors_ff(
        "factors_ff3_daily", start_date, end_date
      )

      batches <- ceiling(length(permnos) / batch_size)

      crsp_daily_list <- list()

      for (j in 1:batches) {

        permno_batch <- permnos[
          ((j - 1) * batch_size + 1):min(j * batch_size, length(permnos))
        ]

        crsp_daily_sub <- dsf_db |>
          filter(permno %in% permno_batch &
                   dlycaldt >= start_date & dlycaldt <= end_date) |>
          select(permno, date = dlycaldt, ret = dlyret) |>
          collect() |>
          drop_na()

        if (nrow(crsp_daily_sub) > 0) {

          crsp_daily_list[[j]] <- crsp_daily_sub |>
            mutate(month = floor_date(date, "month")) |>
            left_join(factors_ff3_daily |>
                        select(date, risk_free), join_by(date)) |>
            mutate(
              ret_excess = ret - risk_free,
              ret_excess = pmax(ret_excess, -1)
            ) |>
            select(permno, date, month, ret, ret_excess)

        }
      }

      disconnection_connection(con)

      processed_data <- bind_rows(crsp_daily_list)

    }

  }

  processed_data
}

#' Download Data from WRDS Compustat
#'
#' This function downloads financial data from the WRDS Compustat database for a
#' given type of financial data, start date, and end date. It filters the data
#' according to industry format, data format, and consolidation level, and
#' calculates book equity (be), operating profitability (op), and investment
#' (inv) for each company.
#'
#' @param type The type of financial data to download.
#' @param start_date The start date for the data retrieval in "YYYY-MM-DD"
#'   format.
#' @param end_date The end date for the data retrieval in "YYYY-MM-DD" format.
#' @param ... Additional Compustat variables that should be added from the raw
#'   data.
#'
#' @return A data frame with financial data for the specified period, including
#'   variables for book equity (be), operating profitability (op), investment
#'   (inv), and others.
#'
#' @examples
#' \donttest{
#'   compustat <- download_data_wrds_compustat("wrds_compustat_annual", "2020-01-01", "2020-12-31")
#' }
#'
#' @import dplyr
#' @importFrom lubridate year
#'
#' @export
download_data_wrds_compustat <- function(type, start_date, end_date, ...) {

  check_if_package_installed("dbplyr", type)

  in_schema <- getNamespace("dbplyr")$in_schema

  con <- get_wrds_connection()

  if (grepl("compustat_annual", type)) {
    funda_db <- tbl(con, in_schema("comp", "funda"))

    compustat <- funda_db |>
      filter(
        indfmt == "INDL" &
          datafmt == "STD" &
          consol == "C" &
          datadate >= start_date & datadate <= end_date
      ) |>
      select(
        gvkey, datadate, seq, ceq, at, lt, txditc,
        txdb, itcb, pstkrv, pstkl, pstk, capx, oancf,
        sale, cogs, xint, xsga,
        ...
      ) |>
      collect()

    compustat <- compustat |>
      mutate(
        be = coalesce(seq, ceq + pstk, at - lt) +
          coalesce(txditc, txdb + itcb, 0) -
          coalesce(pstkrv, pstkl, pstk, 0),
        be = if_else(be <= 0, NA, be),
        op = (sale - coalesce(cogs, 0) -
                coalesce(xsga, 0) - coalesce(xint, 0)) / be,
      )

    compustat <- compustat |>
      mutate(year = lubridate::year(datadate)) |>
      group_by(gvkey, year) |>
      filter(datadate == max(datadate)) |>
      ungroup()

    processed_data <- compustat |>
      left_join(
        compustat |>
          select(gvkey, year, at_lag = at) |>
          mutate(year = year + 1),
        join_by(gvkey, year)
      ) |>
      mutate(
        inv = at / at_lag - 1,
        inv = if_else(at_lag <= 0, NA, inv)
      )

    disconnection_connection(con)

    processed_data
  }
}

#' Download CCM Links from WRDS
#'
#' This function downloads data from the WRDS CRSP/Compustat Merged (CCM) links
#' database. It allows users to specify the type of links (`linktype`), the
#' primacy of the link (`linkprim`), and whether to use flagged links
#' (`usedflag`).
#'
#' @param linktype A character vector indicating the type of link to download.
#'   The default is `c("LU", "LC")`, where "LU" stands for "Link Up" and "LC"
#'   for "Link CRSP".
#' @param linkprim A character vector indicating the primacy of the link.
#'   Default is `c("P", "C")`, where "P" indicates primary and "C" indicates
#'   conditional links.
#' @param usedflag An integer indicating whether to use flagged links. The
#'   default is `1`, indicating that only flagged links should be used.
#'
#' @return A data frame with the columns `permno`, `gvkey`, `linkdt`, and
#'   `linkenddt`, where `linkenddt` is the end date of the link, and missing end
#'   dates are replaced with today's date.
#'
#' @examples
#' \donttest{
#'   ccm_links <- download_data_wrds_ccm_links(linktype = "LU", linkprim = "P", usedflag = 1)
#' }
#'
#' @import dplyr
#' @importFrom tidyr replace_na
#' @importFrom lubridate today
#'
#' @export
download_data_wrds_ccm_links <- function(
    linktype = c("LU", "LC"), linkprim = c("P", "C"), usedflag = 1
  ) {

  check_if_package_installed("dbplyr", "wrds_ccm_links")

  in_schema <- getNamespace("dbplyr")$in_schema

  con <- get_wrds_connection()

  ccmxpf_linktable_db <- tbl(con, in_schema("crsp", "ccmxpf_linktable"))

  ccm_links <- ccmxpf_linktable_db |>
    filter(linktype %in% local(linktype) &
             linkprim %in% local(linkprim) &
             usedflag == local(usedflag)) |>
    select(permno = lpermno, gvkey, linkdt, linkenddt) |>
    collect() |>
    mutate(linkenddt = tidyr::replace_na(linkenddt, lubridate::today()))

  disconnection_connection(con)

  ccm_links
}

#' Download Filtered FISD Data from WRDS
#'
#' Establishes a connection to the WRDS database to download a filtered subset
#' of the FISD (Fixed Income Securities Database). The function filters the
#' `fisd_mergedissue` and `fisd_mergedissuer` tables based on several criteria
#' related to the securities, such as security level, bond type, coupon type,
#' and others, focusing on specific attributes that denote the nature of the
#' securities. It finally returns a data frame with selected fields from the
#' `fisd_mergedissue` table after joining it with issuer information from the
#' `fisd_mergedissuer` table for issuers domiciled in the USA.
#'
#' @return A data frame containing a subset of FISD data with fields related to
#'   the bond's characteristics and issuer information. This includes complete
#'   CUSIP, maturity, offering amount, offering date, dated date, interest
#'   frequency, coupon, last interest date, issue ID, issuer ID, SIC code of the
#'   issuer.
#'
#' @examples
#' \donttest{
#'   fisd <- download_data_wrds_fisd()
#' }
#'
#' @import dplyr
#'
#' @export
download_data_wrds_fisd <- function() {

  check_if_package_installed("dbplyr", "fisd_mergedissue")

  in_schema <- getNamespace("dbplyr")$in_schema

  con <- get_wrds_connection()

  fisd_mergedissue_db <- tbl(con, in_schema("fisd", "fisd_mergedissue"))

  fisd <- fisd_mergedissue_db |>
    filter(
      security_level == "SEN",
      slob == "N" | is.na(slob),
      is.na(security_pledge),
      asset_backed == "N" | is.na(asset_backed),
      defeased == "N" | is.na(defeased),
      is.na(defeased_date),
      bond_type %in% c("CDEB", "CMTN", "CMTZ", "CZ", "USBN"),
      pay_in_kind != "Y" | is.na(pay_in_kind),
      is.na(pay_in_kind_exp_date),
      yankee == "N" | is.na(yankee),
      canadian == "N" | is.na(canadian),
      foreign_currency == "N",
      coupon_type %in% c("F", "Z"),
      is.na(fix_frequency),
      coupon_change_indicator == "N",
      interest_frequency %in% c("0", "1","2", "4", "12"),
      rule_144a == "N",
      private_placement == "N" | is.na(private_placement),
      defaulted == "N",
      is.na(filing_date),
      is.na(settlement),
      convertible == "N",
      is.na(exchange),
      putable == "N" | is.na(putable),
      unit_deal == "N" | is.na(unit_deal),
      exchangeable == "N" | is.na(exchangeable),
      perpetual == "N",
      preferred_security == "N" | is.na(preferred_security)
    ) |>
    select(
      complete_cusip, maturity,
      offering_amt, offering_date,
      dated_date,
      interest_frequency, coupon,
      last_interest_date,
      issue_id, issuer_id
    ) |>
    collect()

  fisd_mergedissuer_db <- tbl(con, in_schema("fisd", "fisd_mergedissuer"))

  fisd_issuer <- fisd_mergedissuer_db |>
    filter(country_domicile == "USA") |>
    select(issuer_id, sic_code) |>
    collect()

  disconnection_connection(con)

  fisd <- fisd |>
    inner_join(fisd_issuer, join_by(issuer_id))

  fisd
}

#' Download Cleaned TRACE Data from WRDS
#'
#' Establishes a connection to the WRDS database to download the specified
#' CUSIPs trade messages from the Trade Reporting and Compliance Engine (TRACE).
#' The trade data is cleaned as suggested by Dick-Nielsen (2009, 2014).
#'
#' @param cusips A character vector specifying the 9-digit CUSIPs to download.
#' @param start_date The start date for filtering the data, in "YYYY-MM-DD"
#'   format.
#' @param end_date The end date for filtering the data, in "YYYY-MM-DD" format.
#'
#' @return A data frame containing the cleaned trade messages from TRACE for the
#'   selected CUSIPs over the time window specified. Output variables include
#'   identifying information (i.e., CUSIP, trade date/time) and trade-specific
#'   information (i.e., price/yield, volume, counterparty, and reporting side).
#'
#' @examples
#' \donttest{
#'   one_bond <- download_data_wrds_clean_trace("00101JAH9", "2019-01-01", "2021-12-31")
#' }
#'
#' @import dplyr
#' @importFrom lubridate as_datetime
#'
#' @export
download_data_wrds_clean_trace <- function(cusips, start_date, end_date) {

  check_if_package_installed("dbplyr", "clean_trace")

  in_schema <- getNamespace("dbplyr")$in_schema

  con <- get_wrds_connection()

  trace_enhanced_db <- tbl(con, in_schema("trace", "trace_enhanced"))

  trace_all <- trace_enhanced_db |>
    filter(cusip_id %in% cusips) |>
    filter(trd_exctn_dt >= start_date & trd_exctn_dt <= end_date) |>
    select(cusip_id, msg_seq_nb, orig_msg_seq_nb,
           entrd_vol_qt, rptd_pr, yld_pt, rpt_side_cd, cntra_mp_id,
           trd_exctn_dt, trd_exctn_tm, trd_rpt_dt, trd_rpt_tm,
           pr_trd_dt, trc_st, asof_cd, wis_fl,
           days_to_sttl_ct, stlmnt_dt, spcl_trd_fl) |>
    collect()

  disconnection_connection(con)

  # Enhanced Trace: Post 06-02-2012 -----------------------------------------
  # Trades (trc_st = T) and correction (trc_st = R)
  trace_post_TR <- trace_all |>
    filter((trc_st == "T" | trc_st == "R"),
           trd_rpt_dt >= as.Date("2012-02-06"))

  # Cancelations (trc_st = X) and correction cancelations (trc_st = C)
  trace_post_XC <- trace_all |>
    filter((trc_st == "X" | trc_st == "C"),
           trd_rpt_dt >= as.Date("2012-02-06"))

  # Cleaning corrected and cancelled trades
  trace_post_TR <- trace_post_TR |>
    anti_join(trace_post_XC,
              by = c("cusip_id", "msg_seq_nb", "entrd_vol_qt",
                     "rptd_pr", "rpt_side_cd", "cntra_mp_id",
                     "trd_exctn_dt", "trd_exctn_tm"))

  # Reversals (trc_st = Y)
  trace_post_Y <- trace_all |>
    filter(trc_st == "Y",
           trd_rpt_dt >= as.Date("2012-02-06"))

  # Clean reversals
  ## match the orig_msg_seq_nb of the Y-message to
  ## the msg_seq_nb of the main message
  trace_post <- trace_post_TR |>
    anti_join(trace_post_Y,
              by = c("cusip_id", "msg_seq_nb" = "orig_msg_seq_nb",
                     "entrd_vol_qt", "rptd_pr", "rpt_side_cd",
                     "cntra_mp_id", "trd_exctn_dt", "trd_exctn_tm"))


  # Enhanced TRACE: Pre 06-02-2012 ------------------------------------------
  # Cancelations (trc_st = C)
  trace_pre_C <- trace_all |>
    filter(trc_st == "C",
           trd_rpt_dt < as.Date("2012-02-06"))

  # Trades w/o cancelations
  ## match the orig_msg_seq_nb of the C-message
  ## to the msg_seq_nb of the main message
  trace_pre_T <- trace_all |>
    filter(trc_st == "T",
           trd_rpt_dt < as.Date("2012-02-06")) |>
    anti_join(trace_pre_C,
              by = c("cusip_id", "msg_seq_nb" = "orig_msg_seq_nb",
                     "entrd_vol_qt", "rptd_pr", "rpt_side_cd",
                     "cntra_mp_id", "trd_exctn_dt", "trd_exctn_tm"))

  # Corrections (trc_st = W) - W can also correct a previous W
  trace_pre_W <- trace_all |>
    filter(trc_st == "W",
           trd_rpt_dt < as.Date("2012-02-06"))

  # Implement corrections in a loop
  ## Correction control
  correction_control <- nrow(trace_pre_W)
  correction_control_last <- nrow(trace_pre_W)

  ## Correction loop
  while (correction_control > 0) {
    # Corrections that correct some msg
    trace_pre_W_correcting <- trace_pre_W |>
      semi_join(trace_pre_T,
                by = c("cusip_id", "trd_exctn_dt",
                       "orig_msg_seq_nb" = "msg_seq_nb"))

    # Corrections that do not correct some msg
    trace_pre_W <- trace_pre_W |>
      anti_join(trace_pre_T,
                by = c("cusip_id", "trd_exctn_dt",
                       "orig_msg_seq_nb" = "msg_seq_nb"))

    # Delete msgs that are corrected and add correction msgs
    trace_pre_T <- trace_pre_T |>
      anti_join(trace_pre_W_correcting,
                by = c("cusip_id", "trd_exctn_dt",
                       "msg_seq_nb" = "orig_msg_seq_nb")) |>
      union_all(trace_pre_W_correcting)

    # Escape if no corrections remain or they cannot be matched
    correction_control <- nrow(trace_pre_W)

    if (correction_control == correction_control_last) {

      correction_control <- 0

    }

    correction_control_last <- nrow(trace_pre_W)

  }


  # Clean reversals
  ## Record reversals
  trace_pre_R <- trace_pre_T |>
    filter(asof_cd == 'R') |>
    group_by(cusip_id, trd_exctn_dt, entrd_vol_qt,
             rptd_pr, rpt_side_cd, cntra_mp_id) |>
    arrange(trd_exctn_tm, trd_rpt_dt, trd_rpt_tm) |>
    mutate(seq = row_number()) |>
    ungroup()

  ## Remove reversals and the reversed trade
  trace_pre <- trace_pre_T |>
    filter(is.na(asof_cd) | !(asof_cd %in% c('R', 'X', 'D'))) |>
    group_by(cusip_id, trd_exctn_dt, entrd_vol_qt,
             rptd_pr, rpt_side_cd, cntra_mp_id) |>
    arrange(trd_exctn_tm, trd_rpt_dt, trd_rpt_tm) |>
    mutate(seq = row_number()) |>
    ungroup() |>
    anti_join(trace_pre_R,
              by = c("cusip_id", "trd_exctn_dt", "entrd_vol_qt",
                     "rptd_pr", "rpt_side_cd", "cntra_mp_id", "seq")) |>
    select(-seq)


  # Agency trades -----------------------------------------------------------
  # Combine pre and post trades
  trace_clean <- trace_post |>
    union_all(trace_pre)

  # Keep angency sells and unmatched agency buys
  ## Agency sells
  trace_agency_sells <- trace_clean |>
    filter(cntra_mp_id == "D",
           rpt_side_cd == "S")

  # Agency buys that are unmatched
  trace_agency_buys_filtered <- trace_clean |>
    filter(cntra_mp_id == "D",
           rpt_side_cd == "B") |>
    anti_join(trace_agency_sells,
              by = c("cusip_id", "trd_exctn_dt",
                     "entrd_vol_qt", "rptd_pr"))

  # Agency clean
  trace_clean <- trace_clean |>
    filter(cntra_mp_id == "C")  |>
    union_all(trace_agency_sells) |>
    union_all(trace_agency_buys_filtered)


  # Additional Filters ------------------------------------------------------
  trace_add_filters <- trace_clean |>
    mutate(days_to_sttl_ct2 = stlmnt_dt - trd_exctn_dt) |>
    filter(is.na(days_to_sttl_ct) | as.numeric(days_to_sttl_ct) <= 7,
           is.na(days_to_sttl_ct2) | as.numeric(days_to_sttl_ct2) <= 7,
           wis_fl == "N",
           is.na(spcl_trd_fl) | spcl_trd_fl == "",
           is.na(asof_cd) | asof_cd == "")


  # Output ------------------------------------------------------------------
  # Only keep necessary columns
  trace_final <- trace_add_filters |>
    arrange(cusip_id, trd_exctn_dt, trd_exctn_tm) |>
    select(cusip_id, trd_exctn_dt, trd_exctn_tm,
           rptd_pr, entrd_vol_qt, yld_pt, rpt_side_cd, cntra_mp_id) |>
    mutate(trd_exctn_tm = format(lubridate::as_datetime(trd_exctn_tm, tz = "America/New_York"), "%H:%M:%S"))

  trace_final
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

utils::globalVariables(
  c(
    "altprc", "at", "at_lag", "be", "capx", "ceq", "cogs", "consol",
    "datadate", "datafmt", "dlret",
    "dlstcd", "dlstdt", "exchcd", "gvkey", "indfmt", "inv", "itcb", "linkdt", "linkenddt",
    "lpermno", "lt", "mktcap", "namedt", "nameendt", "oancf", "permno", "pstk", "pstkl", "pstkrv", "ret",
    "ret_adj", "ret_excess", "risk_free", "sale", "shrcd", "shrout", "siccd", "txdb", "txditc", "xint",
    "xsga","ret_excess",
    "dlycaldt", "dlyret", "issuertype", "mthcaldt", "mthprc", "mthret", "prc", "primaryexch",
    "secinfoenddt", "secinfostartdt", "securitysubtype", "securitytype", "sharetype",
    "usincflg", "hml", "smb"
  )
)

utils::globalVariables(
  c(
    "issuer_id", "sic_code", "country_domicile",
    "complete_cusip", "maturity", "offering_amt", "offering_date",
    "dated_date", "interest_frequency", "coupon", "last_interest_date",
    "issue_id", "security_level", "slob", "security_pledge", "asset_backed",
    "defeased", "defeased_date", "bond_type", "pay_in_kind","pay_in_kind_exp_date",
    "yankee", "canadian", "foreign_currency", "coupon_type", "fix_frequency",
    "coupon_change_indicator", "interest_frequency", "rule_144a",
    "private_placement", "defaulted", "filing_date", "settlement",
    "convertible", "exchange", "putable", "unit_deal", "exchangeable",
    "perpetual", "preferred_security"
  )
)

utils::globalVariables(
  c(
    "cusip_id", "trd_exctn_dt", "trd_exctn_tm",
    "rptd_pr", "entrd_vol_qt", "yld_pt", "rpt_side_cd", "cntra_mp_id",
    "asof_cd", "days_to_sttl_ct", "days_to_sttl_ct2", "msg_seq_nb", "orig_msg_seq_nb",
    "pr_trd_dt", "spcl_trd_fl", "stlmnt_dt", "trc_st", "trd_rpt_dt", "trd_rpt_tm", "wis_fl"
  )
)
