# Script to download risk-free rate data from FRED, process it, and save
# pre-computed parquet files for upload to HuggingFace.
#
# Run by the GitHub Actions workflow:
#   .github/workflows/update-risk-free-rate.yaml
#
# The resulting files are uploaded to:
#   https://huggingface.co/datasets/tidy-finance/risk-free-rate
#
# MANUAL SETUP REQUIRED (one-time, by repository maintainers):
#   1. Create the HuggingFace dataset repository "tidy-finance/risk-free-rate"
#      at https://huggingface.co/new-dataset (visibility: public).
#   2. Generate a HuggingFace API token with WRITE access to the
#      "tidy-finance" organization at https://huggingface.co/settings/tokens.
#   3. Add the token as a GitHub Actions secret named "HF_TOKEN" at
#      https://github.com/tidy-finance/r-tidyfinance/settings/secrets/actions.
#   4. Trigger the workflow manually (Actions > Update Risk-Free Rate on
#      HuggingFace > Run workflow) to populate the dataset for the first time.

library(httr2)
library(arrow)
library(dplyr)
library(tibble)
library(tidyr)
library(lubridate)

# --- Helper: download a single FRED series as a tibble -------------------

download_fred_series <- function(series_id) {
  url <- paste0(
    "https://fred.stlouisfed.org/graph/fredgraph.csv?id=",
    series_id
  )
  resp <- httr2::request(url) |>
    httr2::req_perform()

  httr2::resp_body_string(resp) |>
    textConnection() |>
    read.csv() |>
    as_tibble() |>
    mutate(
      date = as.Date(.data$observation_date),
      value = suppressWarnings(as.numeric(.data[[series_id]])),
      .keep = "none"
    )
}

# --- Monthly risk-free rate -----------------------------------------------

compute_monthly_rf <- function() {
  splice_date <- as.Date("2001-07-01")

  message("Downloading TB3MS from FRED...")
  fred_tb3ms <- download_fred_series("TB3MS")

  message("Downloading DTB4WK from FRED...")
  fred_dtb4wk <- download_fred_series("DTB4WK")

  rf_tb3ms <- fred_tb3ms |>
    drop_na(value) |>
    mutate(
      ret_3m = (value / 100) * (90 / 360) / (1 - (value / 100) * (90 / 360)),
      risk_free = (1 + ret_3m)^(1 / 3) - 1
    ) |>
    select(date, risk_free)

  rf_dtb4wk <- fred_dtb4wk |>
    drop_na(value) |>
    mutate(date = floor_date(date, "month")) |>
    group_by(date) |>
    slice_tail(n = 1) |>
    ungroup() |>
    mutate(
      ret_4wk = (value / 100) * (28 / 360) / (1 - (value / 100) * (28 / 360)),
      risk_free = (1 + ret_4wk)^(365 / 28 / 12) - 1
    ) |>
    select(date, risk_free)

  bind_rows(
    rf_tb3ms |> filter(date < splice_date),
    rf_dtb4wk |> filter(date >= splice_date)
  ) |>
    arrange(date)
}

# --- Daily risk-free rate -------------------------------------------------

compute_daily_rf <- function() {
  splice_date <- as.Date("2001-07-31")

  message("Downloading DTB3 from FRED...")
  fred_dtb3 <- download_fred_series("DTB3")

  message("Downloading DTB4WK from FRED...")
  fred_dtb4wk <- download_fred_series("DTB4WK")

  rf_dtb3 <- fred_dtb3 |>
    arrange(date) |>
    fill(value, .direction = "down") |>
    drop_na(value) |>
    mutate(
      ret_3m = (value / 100) * (90 / 360) / (1 - (value / 100) * (90 / 360)),
      risk_free = (1 + ret_3m)^(1 / 63) - 1
    ) |>
    select(date, risk_free)

  rf_dtb4wk <- fred_dtb4wk |>
    arrange(date) |>
    fill(value, .direction = "down") |>
    drop_na(value) |>
    mutate(
      ret_4wk = (value / 100) * (28 / 360) / (1 - (value / 100) * (28 / 360)),
      risk_free = (1 + ret_4wk)^(1 / 20) - 1
    ) |>
    select(date, risk_free)

  bind_rows(
    rf_dtb3 |> filter(date < splice_date),
    rf_dtb4wk |> filter(date >= splice_date)
  ) |>
    arrange(date)
}

# --- Main -----------------------------------------------------------------

message("Computing monthly risk-free rates...")
monthly_rf <- compute_monthly_rf()
message(
  "Monthly data: ", nrow(monthly_rf), " rows from ",
  min(monthly_rf$date), " to ", max(monthly_rf$date)
)

message("Computing daily risk-free rates...")
daily_rf <- compute_daily_rf()
message(
  "Daily data: ", nrow(daily_rf), " rows from ",
  min(daily_rf$date), " to ", max(daily_rf$date)
)

message("Saving parquet files to /tmp/...")
write_parquet(monthly_rf, "/tmp/tidyfinance-risk-free-monthly.parquet")
write_parquet(daily_rf, "/tmp/tidyfinance-risk-free-daily.parquet")

message("Parquet files saved. Ready for upload to HuggingFace.")
