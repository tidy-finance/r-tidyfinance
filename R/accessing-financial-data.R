library(dplyr)
library(frenchdata)
library(lubridate)

# TODO: discuss whether we really need month and date difference?

# Define types ------------------------------------------------------------
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

list_supported_types_macro_predictors <- function() {
  tribble(
    ~type, ~dataset_name,
    "macro_predictors_monthly", "PredictorData2022.xlsx",
    "macro_predictors_quarterly", "PredictorData2022.xlsx",
    "macro_predictors_annual", "PredictorData2022.xlsx",
  ) |>
    mutate(domain = "Welch-Goyal")
}

list_supported_types <- function() {
  bind_rows(
    list_supported_types_q(),
    list_supported_types_ff(),
    list_supported_types_macro_predictors()
  )
}

check_supported_type <- function(type) {
  supported_types <- list_supported_types()
  if (!any(type %in% supported_types$type)) {
    stop("Unsupported type specified. Choose one of the following: ",
         paste(supported_types$type, collapse = ", "))
  }
}

# Main function -----------------------------------------------------------
download_data <- function(type, start_date, end_date) {

  check_supported_type(type)

  if (grepl("factors", type)) {
    processed_data <- download_data_factors(type, start_date, end_date)
  }

  processed_data
}

# Factors -----------------------------------------------------------------
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

download_data_factors_ff <- function(type, start_date, end_date) {

  check_supported_type(type)

  if (!requireNamespace("frenchdata", quietly = TRUE)) {
    stop(paste0("The package 'frenchdata' is required for type = ", type,
                ", but not installed. Please install it using install.packages('frenchdata')."))
  }

  factors_ff_types <- list_supported_types_ff()
  dataset <- factors_ff_types$dataset_name[factors_ff_types$type == type]

  raw_data <- suppressMessages(frenchdata::download_french_data(dataset))
  raw_data <- raw_data$subsets$data[[1]]

  if (grepl("monthly", type)) {
    processed_data <- raw_data |>
      mutate(date = floor_date(ymd(paste0(date, "01")), "month"))
  } else {
    processed_data <- raw_data |>
      mutate(date = ymd(date))
  }

  processed_data <-  processed_data |>
    mutate(
      across(-c(date), ~as.numeric(.) / 100)
    ) |>
    rename_with(tolower) |>
    rename(mkt_excess = `mkt-rf`) |>
    filter(date >= start_date & date <= end_date)  |>
    select(date, risk_free, mkt_excess, everything())

  processed_data
}

download_data_factors_q <- function(type, start_date, end_date, url = "http://global-q.org/uploads/1/2/2/6/122679606/") {

  check_supported_type(type)

  factors_q_types <- list_supported_types_q()
  dataset <- factors_q_types$dataset_name[factors_q_types$type == type]
  raw_data <- suppressMessages(read.csv(paste0(url, dataset)) |> as_tibble())

  if (grepl("monthly", type)) {
    processed_data <- raw_data |>
      mutate(date = ymd(paste(year, month, "01", sep = "-"))) |>
      select(-c(year, month))
  }
  if (grepl("daily", type)) {
    processed_data <- raw_data |>
      mutate(DATE = ymd(DATE))
  }

  processed_data <- processed_data |>
    rename_with(~ stringr::str_remove(., "R_")) |>
    rename_with(~tolower(.)) |>
    mutate(across(-date, ~. / 100)) |>
    filter(date >= start_date & date <= end_date) |>
    select(date, risk_free = f, mkt_excess = mkt, everything())

  processed_data
}

download_data("factors_q5_monthly", ymd("1926-07-01"), Sys.Date()-1)
download_data("factors_q5_daily", ymd("1926-07-01"), Sys.Date()-1)

download_data("factors_ff3_monthly", ymd("1926-07-01"), Sys.Date()-1)
download_data("factors_ff5_daily", ymd("1926-07-01"), Sys.Date()-1)

# Macro predictors --------------------------------------------------------
library(readxl)

download_data_macro_predictors <- function(type, start_date, end_date, url = "https://docs.google.com/spreadsheets/d/1g4LOaRj4TvwJr9RIaA_nwrXXWTOy46bP/export?format=xlsx") {

  check_supported_type(type)

  temporary_file <- tempfile()

  download.file(url = url, destfile = temporary_file, mode = "wb", quiet = TRUE)

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
    na.omit()

  file.remove(temporary_file)

  processed_data
}

download_data_macro_predictors("macro_predictors_monthly", ymd("1926-07-01"), Sys.Date()-1)
download_data_macro_predictors("macro_predictors_quarterly", ymd("1926-07-01"), Sys.Date()-1)
download_data_macro_predictors("macro_predictors_annual", ymd("1926-07-01"), Sys.Date()-1)
