# A sheet in the layout of the recent files: metadata rows above the header.
mock_wacc_sheet <- function(local_header = "Cost of Capital (Local Currency)") {
  rows <- list(
    c("Date updated:", "46027", rep(NA, 10)),
    c("Created by:", "Aswath Damodaran", rep(NA, 10)),
    c(
      "Industry Name", "Number of Firms", "Beta", "Cost of Equity",
      "E/(D+E)", "Std Dev in Stock", "Cost of Debt", "Tax Rate",
      "After-tax Cost of Debt", "D/(D+E)", "Cost of Capital", local_header
    ),
    c(
      "Advertising", "52", "1.2", "0.09", "0.7", "0.5", "0.06", "0.1",
      "0.05", "0.3", "0.08", "0.081"
    ),
    c(
      "Total Market", "5994", "0.9", "0.08", "0.8", "0.4", "0.05", "0.2",
      "0.04", "0.2", "0.07", "0.071"
    ),
    rep(NA, 12)
  )
  sheet <- as_tibble(
    as.data.frame(do.call(rbind, rows)),
    .name_repair = "minimal"
  )
  names(sheet) <- paste0("...", seq_len(ncol(sheet)))
  sheet
}

mock_wacc_file <- function(year) {
  tibble(
    year = year,
    industry = "Advertising",
    number_of_firms = 52L,
    beta = 1.2,
    cost_of_equity = 0.09,
    equity_to_capital = 0.7,
    sd_stock = 0.5,
    cost_of_debt = 0.06,
    tax_rate = 0.1,
    after_tax_cost_of_debt = 0.05,
    debt_to_capital = 0.3,
    wacc = 0.08,
    wacc_local = NA_real_
  )
}

test_that("parses the industry table from the matching sheet", {
  local_mocked_bindings(
    excel_sheets = function(path) c("Variables & FAQ", "Industry Averages"),
    read_excel = function(path, sheet, ...) {
      if (sheet == "Variables & FAQ") tibble() else mock_wacc_sheet()
    },
    .package = "readxl"
  )

  result <- parse_damodaran_wacc("wacc.xls")

  expect_equal(
    names(result),
    c(
      "year", "industry", "number_of_firms", "beta", "cost_of_equity",
      "equity_to_capital", "sd_stock", "cost_of_debt", "tax_rate",
      "after_tax_cost_of_debt", "debt_to_capital", "wacc", "wacc_local"
    )
  )
  expect_equal(result$industry, c("Advertising", "Total Market"))
  expect_identical(result$number_of_firms, c(52L, 5994L))
  expect_equal(result$wacc, c(0.08, 0.07))
  expect_equal(result$wacc_local, c(0.081, 0.071))
  # Updated in January 2026, so the data refers to 2025.
  expect_equal(result$year, c(2025, 2025))
})

test_that("maps currency-specific local columns and fills missing ones", {
  local_mocked_bindings(
    excel_sheets = function(path) "Sheet1",
    read_excel = function(path, sheet, ...) {
      mock_wacc_sheet("Cost of Capital (Euros)")
    },
    .package = "readxl"
  )
  expect_equal(parse_damodaran_wacc("x.xls")$wacc_local, c(0.081, 0.071))

  local_mocked_bindings(
    read_excel = function(path, sheet, ...) {
      sheet <- mock_wacc_sheet()
      sheet[-1, -12]
    },
    .package = "readxl"
  )
  result <- parse_damodaran_wacc("x.xls")
  expect_true(all(is.na(result$wacc_local)))
  expect_true(all(is.na(result$year)))
})

test_that("aborts when no sheet holds the industry table", {
  local_mocked_bindings(
    excel_sheets = function(path) "Sheet1",
    read_excel = function(path, sheet, ...) tibble(x = "nothing"),
    .package = "readxl"
  )
  expect_error(parse_damodaran_wacc("x.xls"), "industry table")
})

test_that("aborts when a required column is missing", {
  local_mocked_bindings(
    excel_sheets = function(path) "Sheet1",
    read_excel = function(path, sheet, ...) mock_wacc_sheet()[, -3],
    .package = "readxl"
  )
  expect_error(parse_damodaran_wacc("x.xls"), "Missing columns")
})

test_that("downloads and parses a file from the data library", {
  requested_url <- NULL
  local_mocked_bindings(parse_damodaran_wacc = function(path) "parsed")
  httr2::local_mocked_responses(function(req) {
    requested_url <<- req$url
    httr2::response(status_code = 200)
  })

  expect_equal(download_damodaran_file("datasets/wacc.xls"), "parsed")
  expect_equal(
    requested_url,
    "https://pages.stern.nyu.edu/~adamodar/pc/datasets/wacc.xls"
  )
})

test_that("returns NULL for missing files and failed downloads", {
  local_mocked_bindings(
    parse_damodaran_wacc = function(path) stop("must not be called")
  )

  httr2::local_mocked_responses(function(req) {
    httr2::response(status_code = 404)
  })
  expect_null(download_damodaran_file("archives/wacc25.xls"))

  httr2::local_mocked_responses(function(req) {
    httr2::response(status_code = 500)
  })
  expect_message(
    expect_null(download_damodaran_file("datasets/wacc.xls")),
    "HTTP status 500"
  )

  httr2::local_mocked_responses(function(req) stop("offline"))
  expect_message(
    expect_null(download_damodaran_file("datasets/wacc.xls")),
    "Failed to download or process the resource"
  )
})

test_that("downloads the current file by default", {
  requested <- character()
  local_mocked_bindings(
    download_damodaran_file = function(file_path) {
      requested <<- c(requested, file_path)
      mock_wacc_file(2025)
    }
  )

  result <- download_data_damodaran()

  expect_equal(requested, "datasets/wacc.xls")
  expect_equal(result$region, "us")
  expect_identical(result$year, 2025L)
  expect_equal(names(result)[1:3], c("region", "year", "industry"))
})

test_that("stacks archived years and falls back to the current file", {
  requested <- character()
  local_mocked_bindings(
    download_damodaran_file = function(file_path) {
      requested <<- c(requested, file_path)
      switch(
        file_path,
        "archives/waccEurope23.xls" = mock_wacc_file(NA),
        "archives/waccEurope24.xls" = mock_wacc_file(NA),
        "datasets/waccEurope.xls" = mock_wacc_file(2025),
        NULL
      )
    }
  )

  result <- download_data_damodaran(region = "europe", year = 2023:2025)

  expect_equal(
    requested,
    c(
      "archives/waccEurope23.xls",
      "archives/waccEurope24.xls",
      "archives/waccEurope25.xls",
      "datasets/waccEurope.xls"
    )
  )
  expect_identical(result$year, 2023:2025)
  expect_equal(unique(result$region), "europe")
})

test_that("warns about unavailable years", {
  local_mocked_bindings(
    download_damodaran_file = function(file_path) {
      if (file_path == "archives/wacc98.xls") mock_wacc_file(NA) else NULL
    }
  )

  expect_warning(
    result <- download_data_damodaran(year = c(1998, 1997)),
    "us \\(1997\\)"
  )
  expect_identical(result$year, 1998L)
})

test_that("returns empty tibble after download failure", {
  local_mocked_bindings(download_damodaran_file = function(file_path) NULL)

  expect_message(
    expect_warning(result <- download_data_damodaran(), "No Damodaran data"),
    "Returning an empty data set due to download failure."
  )
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
})

test_that("validates arguments", {
  expect_error(download_data_damodaran("betas"), "Unsupported Damodaran")
  expect_error(download_data_damodaran(region = "mars"), "Unsupported region")
  expect_error(download_data_damodaran(year = 2020.5), "whole numbers")
})

test_that("download_data forwards to download_data_damodaran", {
  captured <- NULL
  local_mocked_bindings(
    download_data_damodaran = function(dataset, ...) {
      captured <<- list(dataset = dataset, ...)
      tibble(ok = TRUE)
    }
  )

  download_data("Damodaran", region = "japan")
  expect_equal(captured, list(dataset = "wacc", region = "japan"))
})
