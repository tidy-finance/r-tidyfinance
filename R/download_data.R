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

list_supported_types_wrds <- function() {
  tribble(
    ~type, ~dataset_name,
    "wrds_crsp_monthly", "crsp.msf, crsp.msenames, crsp.msedelist",
    "wrds_crsp_daily", "crsp.dsf, crsp.msenames, crsp.msedelist",
    "wrds_compustat_annual", "comp.funda",
    "wrds_ccm_links", "crsp.ccmxpf_linktable"
  ) |>
    mutate(domain = "WRDS")
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
    list_supported_types_macro_predictors(),
    list_supported_types_wrds()
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

  if (grepl("wrds", type)) {
    processed_data <- download_data_wrds(type, start_date, end_date)
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

  download_french_data <- getNamespace("frenchdata")$download_french_data

  factors_ff_types <- list_supported_types_ff()
  dataset <- factors_ff_types$dataset_name[factors_ff_types$type == type]

  raw_data <- download_french_data(dataset)
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
#' This function acts as a wrapper to download data from various WRDS datasets including CRSP,
#' Compustat, and CCM links based on the specified type. It is designed to handle different data
#' types by redirecting to the appropriate specific data download function.
#'
#' @param type A string specifying the type of data to download. It should match one of the predefined
#'   patterns to indicate the dataset: "wrds_crsp" for CRSP data, "wrds_compustat" for Compustat data,
#'   or "wrds_ccm_links" for CCM links data.
#' @param start_date A date in 'YYYY-MM-DD' format indicating the start of the period for which data
#'   is requested.
#' @param end_date A date in 'YYYY-MM-DD' format indicating the end of the period for which data
#'   is requested.
#' @return A data frame containing the requested data, with the structure and contents depending on
#'   the specified `type`.
#'
#' @examples
#' \dontrun{
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
#' This function downloads and processes stock return data from the CRSP database for a specified period.
#' Users can choose between monthly and daily data types. The function also adjusts returns for delisting
#' and calculates market capitalization and excess returns over the risk-free rate.
#'
#' @param type A string specifying the type of CRSP data to download: "crsp_monthly" or "crsp_daily".
#' @param start_date A Date object or string specifying the start date of the period for which data is requested.
#' @param end_date A Date object or string specifying the end date of the period.
#' @param batch_size An optional integer specifying the batch size for processing daily data, with a default of 500.
#' @param ... Additional arguments to be passed to underlying functions.
#' @return A data frame containing CRSP stock returns, adjusted for delistings, along with calculated market capitalization
#'   and excess returns over the risk-free rate. The structure of the returned data frame depends on the selected data type.
#'
#' @examples
#' crsp_monthly <- download_data_wrds_crsp("wrds_crsp_monthly", "2020-01-01", "2020-12-31")
#' \dontrun{
#' crsp_daily <- download_data_wrds_crsp("wrds_crsp_daily", "2020-12-01", "2020-12-31")
#' }
#'
#' @import dplyr
#' @import lubridate
#'
#' @export
download_data_wrds_crsp <- function(type, start_date, end_date, ..., batch_size = 500) {

  con <- get_wrds_connection()

  check_if_package_installed("dbplyr", type)

  in_schema <- getNamespace("dbplyr")$in_schema

  if (grepl("crsp_monthly", type)) {

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
      select(-ret_adj, -risk_free)

    processed_data <- crsp_monthly |>
      drop_na(ret_excess, mktcap, mktcap_lag)
  }

  if (grepl("crsp_daily", type)) {
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
    processed_data <- bind_rows(crsp_daily_list)
  }

  processed_data
}

#' Download Data from WRDS Compustat
#'
#' This function downloads financial data from the WRDS Compustat database for a given
#' type of financial data, start date, and end date. It filters the data according to
#' industry format, data format, and consolidation level, and calculates book equity (be),
#' operating profitability (op), and investment (inv) for each company.
#'
#' @param type The type of financial data to download.
#' @param start_date The start date for the data retrieval in "YYYY-MM-DD" format.
#' @param end_date The end date for the data retrieval in "YYYY-MM-DD" format.
#' @param ... Additional Compustat variables that should be added from the raw data.
#' @return A data frame with financial data for the specified period, including variables
#'   for book equity (be), operating profitability (op), investment (inv), and others.
#'
#' @examples
#'
#' compustat <- download_data_wrds_compustat("wrds_compustat_annual", "2020-01-01", "2020-12-31")
#'
#' @import dplyr
#' @importFrom lubridate year
#'
#' @export
download_data_wrds_compustat <- function(type, start_date, end_date, ...) {

  con <- get_wrds_connection()

  check_if_package_installed("dbplyr", type)

  in_schema <- getNamespace("dbplyr")$in_schema

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
#' This function downloads data from the WRDS CRSP/Compustat Merged (CCM) links database.
#' It allows users to specify the type of links (`linktype`), the primacy of the link (`linkprim`),
#' and whether to use flagged links (`usedflag`).
#'
#' @param linktype A character vector indicating the type of link to download. The default
#'   is `c("LU", "LC")`, where "LU" stands for "Link Up" and "LC" for "Link CRSP".
#' @param linkprim A character vector indicating the primacy of the link. Default is
#'   `c("P", "C")`, where "P" indicates primary and "C" indicates conditional links.
#' @param usedflag An integer indicating whether to use flagged links. The default is `1`,
#'   indicating that only flagged links should be used.
#' @return A data frame with the columns `permno`, `gvkey`, `linkdt`, and `linkenddt`,
#'   where `linkenddt` is the end date of the link, and missing end dates are replaced
#'   with today's date.
#'
#' @examples
#' ccm_links <- download_data_wrds_ccm_links(linktype = "LU", linkprim = "P", usedflag = 1)
#'
#' @import dplyr
#' @importFrom lubridate today
#'
#' @export
download_data_wrds_ccm_links <- function(
    linktype = c("LU", "LC"), linkprim = c("P", "C"), usedflag = 1
  ) {

  con <- get_wrds_connection()

  check_if_package_installed("dbplyr", "wrds_ccm_links")

  in_schema <- getNamespace("dbplyr")$in_schema

  ccmxpf_linktable_db <- tbl(con, in_schema("crsp", "ccmxpf_linktable"))

  ccm_links <- ccmxpf_linktable_db |>
    filter(linktype %in% local(linktype) &
             linkprim %in% local(linkprim) &
             usedflag == local(usedflag)) |>
    select(permno = lpermno, gvkey, linkdt, linkenddt) |>
    collect() |>
    mutate(linkenddt = replace_na(linkenddt, lubridate::today()))

  disconnection_connection(con)

  ccm_links
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
    "xsga",
    "ret_excess"
  )
)
