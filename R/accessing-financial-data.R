library(dplyr)
library(frenchdata)
library(lubridate)

# Define types ------------------------------------------------------------
factors_q_types <- tribble(
  ~type, ~dataset_name,
  "factors_q5_monthly", "q5_factors_monthly_2022"
) |>
  mutate(domain = "q-Factors")

factors_frenchdata_types <- tribble(
  ~type, ~dataset_name,
  "factors_ff3_daily", "Fama/French 3 Factors [Daily]",
  "factors_ff3_monthly", "Fama/French 3 Factors",
  "factors_ff5_daily", "Fama/French 5 Factors (2x3) [Daily]",
  "factors_ff5_monthly", "Fama/French 5 Factors (2x3)",
) |>
  mutate(domain = "Fama-French")

supported_types <- bind_rows(
  factors_q_types,
  factors_frenchdata_types
)

check_supported_type <- function(type) {
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

  if (grepl("factors_ff", type)) {
    processed_data <- download_data_factors_frenchdata(type, start_date, end_date)
  }
  if (grepl("factors_q", type)) {
    processed_data <- download_data_factors_q(type, start_date, end_date)
  }
  processed_data
}

download_data_factors_frenchdata <- function(type, start_date, end_date) {

  if (!requireNamespace("frenchdata", quietly = TRUE)) {
    stop(paste0("The package 'frenchdata' is required for type = ", type,
                ", but not installed. Please install it using install.packages('frenchdata')."))
  }

  dataset <- factors_frenchdata_types$dataset_name[factors_frenchdata_types$type==type]

  raw_data <- suppressMessages(frenchdata::download_french_data(dataset))
  raw_data <- raw_data$subsets$data[[1]]

  if (grepl("monthly", type)) {
    processed_data <- raw_data |>
      mutate(date = floor_date(ymd(paste0(date, "01")), "month"))
  } else {
    processed_data <- raw_data |>
      mutate(date = ymd(date))
  }

  processed_data <-  processed_data %>%
    mutate(
      across(-c(date), ~as.numeric(.) / 100)
    ) %>%
    rename_with(tolower) %>%
    rename(mkt_excess = `mkt-rf`) %>%
    filter(date >= start_date & date <= end_date)

  processed_data
}

download_data_factors_q <- function(type, start_date, end_date) {
  # TODO: make this more generic
  factors_q_monthly_link <-
    "https://global-q.org/uploads/1/2/2/6/122679606/q5_factors_monthly_2022.csv"

  factors_q_monthly <- read_csv(factors_q_monthly_link) |>
    mutate(month = ymd(str_c(year, month, "01", sep = "-"))) |>
    select(-R_F, -R_MKT, -year) |>
    rename_with(~ str_remove(., "R_")) |>
    rename_with(~ str_to_lower(.)) |>
    mutate(across(-month, ~ . / 100)) |>
    filter(month >= start_date & month <= end_date)
}


download_data("factors_ff3_monthly", ymd("1926-07-01"), Sys.Date()-1)

download_data("factors_ff5_daily", ymd("1926-07-01"), Sys.Date()-1)

# Macro predictors --------------------------------------------------------
# TODO: use temporary directory?
library(readxl)
library(googledrive)

drive_deauth()

macro_predictors_link <-
  "https://docs.google.com/spreadsheets/d/1g4LOaRj4TvwJr9RIaA_nwrXXWTOy46bP"

drive_download(
  macro_predictors_link,
  path = "macro_predictors.xlsx"
)

macro_predictors <- read_xlsx(
  "macro_predictors.xlsx",
  sheet = "Monthly"
) |>
  mutate(month = ym(yyyymm)) |>
  mutate(across(where(is.character), as.numeric)) |>
  mutate(
    IndexDiv = Index + D12,
    logret = log(IndexDiv) - log(lag(IndexDiv)),
    Rfree = log(Rfree + 1),
    rp_div = lead(logret - Rfree, 1), # Future excess market return
    dp = log(D12) - log(Index), # Dividend Price ratio
    dy = log(D12) - log(lag(Index)), # Dividend yield
    ep = log(E12) - log(Index), # Earnings price ratio
    de = log(D12) - log(E12), # Dividend payout ratio
    tms = lty - tbl, # Term spread
    dfy = BAA - AAA # Default yield spread
  ) |>
  select(month, rp_div, dp, dy, ep, de, svar,
         bm = `b/m`, ntis, tbl, lty, ltr,
         tms, dfy, infl
  ) |>
  filter(month >= start_date & month <= end_date) |>
  drop_na()

file.remove("macro_predictors.xlsx")
