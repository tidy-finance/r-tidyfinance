# Script to download risk-free rate data from FRED, process it, and save
# pre-computed parquet files for upload to HuggingFace.
#
# Run by the GitHub Actions workflow:
#   .github/workflows/update-risk-free.yaml
#
# The resulting files are uploaded to:
#   https://huggingface.co/datasets/tidy-finance/risk-free

# Create monthly risk free rate
splice_date <- as.Date("2001-07-01")

fred_tb3ms <- suppressMessages(download_data_fred("TB3MS"))
fred_dtb4wk <- suppressMessages(download_data_fred("DTB4WK"))

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

risk_free_monthly <- bind_rows(
  rf_tb3ms |> filter(date < splice_date),
  rf_dtb4wk |> filter(date >= splice_date)
) |>
  arrange(date)

# Create daily risk free rate
splice_date <- as.Date("2001-07-31")
fred_dtb3 <- suppressMessages(download_data_fred("DTB3"))

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

risk_free_daily <- bind_rows(
  rf_dtb3 |> filter(date < splice_date),
  rf_dtb4wk |> filter(date >= splice_date)
) |>
  arrange(date)

write_parquet(risk_free_monthly, "/tmp/risk_free_monthly.parquet")
write_parquet(risk_free_daily, "/tmp/risk_free_daily.parquet")
