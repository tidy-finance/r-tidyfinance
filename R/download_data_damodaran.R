#' Download and Process Damodaran Industry Data
#'
#' Downloads industry averages from
#' \href{https://pages.stern.nyu.edu/~adamodar/New_Home_Page/data.html}{Aswath
#' Damodaran's data library}. Currently, the cost of capital (WACC) by industry
#' is supported via `dataset = "wacc"`. The source files are Excel workbooks,
#' so the function requires the \pkg{readxl} package.
#'
#' Damodaran updates the data each January with the data at the end of the
#' previous year. The `year` column refers to that previous year, e.g., the
#' file published in January 2026 has `year = 2025`. Earlier years are read from
#' the archive of the data library. The archive starts in 1998 for the US, in
#' 2013 for Europe, Japan, emerging markets, global, China, and India, and in
#' 2015 for Australia, New Zealand, and Canada. China and India skip some years.
#'
#' @param dataset The dataset to download. Currently only `"wacc"`.
#' @param region A character vector of regions: `"us"` (the default),
#'   `"europe"`, `"japan"`, `"emerging"`, `"global"`, `"china"`, `"india"`, or
#'   `"rest"` (Australia, New Zealand, and Canada).
#' @param year Optional. An integer vector of years. If not provided, the most
#'   recent data is returned. Years that are not available are skipped with a
#'   warning.
#'
#' @returns A tibble with one row per region, year, and industry and the
#'   columns `region`, `year`, `industry`, `number_of_firms`, `beta`,
#'   `cost_of_equity`, `equity_to_capital` (E/(D+E)), `sd_stock` (standard
#'   deviation of stock returns), `cost_of_debt`, `tax_rate`,
#'   `after_tax_cost_of_debt`, `debt_to_capital` (D/(D+E)), `wacc` (cost of
#'   capital in US dollars), and `wacc_local` (cost of capital in local
#'   currency, `NA` where the source file does not provide it). Rates are
#'   decimals. The rows `"Total Market"` and, in recent files,
#'   `"Total Market (without financials)"` hold the market-wide averages.
#'
#' @family download functions
#' @export
#'
#' @examples
#' \donttest{
#'   wacc <- download_data_damodaran("wacc")
#'   wacc_panel <- download_data_damodaran(
#'     "wacc", region = c("us", "europe"), year = 2020:2024
#'   )
#' }
download_data_damodaran <- function(
  dataset = "wacc",
  region = "us",
  year = NULL
) {
  rlang::check_installed("readxl", reason = "to read Damodaran's Excel files.")

  if (!is.character(dataset) || length(dataset) != 1 || dataset != "wacc") {
    cli::cli_abort(c(
      "Unsupported Damodaran dataset: {.val {dataset}}",
      "i" = "Supported datasets: {.val wacc}"
    ))
  }

  file_stems <- list_supported_damodaran_regions()
  unsupported <- setdiff(region, names(file_stems))
  if (!is.character(region) || length(region) == 0 || length(unsupported)) {
    cli::cli_abort(c(
      "Unsupported region{?s}: {.val {unsupported}}",
      "i" = "Supported regions: {.val {names(file_stems)}}"
    ))
  }
  region <- unique(region)

  if (!is.null(year)) {
    if (!rlang::is_integerish(year) || anyNA(year) || length(year) == 0) {
      cli::cli_abort("{.arg year} must be a vector of whole numbers.")
    }
    year <- sort(unique(as.integer(year)))
  }

  results <- list()
  missing <- character()

  for (r in region) {
    stem <- file_stems[[r]]

    if (is.null(year)) {
      wacc <- download_damodaran_file(paste0("datasets/", stem, ".xls"))
      if (is.null(wacc)) {
        missing <- c(missing, r)
      } else {
        results <- c(results, list(mutate(wacc, region = r)))
      }
      next
    }

    unresolved <- integer()
    for (y in year) {
      wacc <- download_damodaran_file(
        paste0("archives/", stem, sprintf("%02d", y %% 100), ".xls")
      )
      if (is.null(wacc)) {
        unresolved <- c(unresolved, y)
      } else {
        results <- c(results, list(mutate(wacc, region = r, year = y)))
      }
    }

    # The most recent year is not archived yet, so look it up in the current
    # file.
    if (length(unresolved) > 0) {
      wacc <- download_damodaran_file(paste0("datasets/", stem, ".xls"))
      if (!is.null(wacc) && wacc$year[1] %in% unresolved) {
        results <- c(results, list(mutate(wacc, region = r)))
        unresolved <- setdiff(unresolved, wacc$year[1])
      }
    }
    if (length(unresolved) > 0) {
      missing <- c(missing, paste0(r, " (", unresolved, ")"))
    }
  }

  if (length(missing) > 0) {
    cli::cli_warn("No Damodaran data available for {.val {missing}}.")
  }

  if (length(results) == 0) {
    cli::cli_inform("Returning an empty data set due to download failure.")
    return(tibble())
  }

  bind_rows(results) |>
    mutate(year = as.integer(.data$year)) |>
    select(
      "region",
      "year",
      "industry",
      "number_of_firms",
      "beta",
      "cost_of_equity",
      "equity_to_capital",
      "sd_stock",
      "cost_of_debt",
      "tax_rate",
      "after_tax_cost_of_debt",
      "debt_to_capital",
      "wacc",
      "wacc_local"
    )
}

#' List Supported Damodaran Regions
#'
#' Maps the regions of Damodaran's data library to the stems of their file
#' names.
#'
#' @returns A named character vector of file stems.
#'
#' @keywords internal
#' @noRd
list_supported_damodaran_regions <- function() {
  c(
    "us" = "wacc",
    "europe" = "waccEurope",
    "japan" = "waccJapan",
    "emerging" = "waccemerg",
    "global" = "waccGlobal",
    "china" = "waccChina",
    "india" = "waccIndia",
    "rest" = "waccRest"
  )
}

#' Download and Parse One Damodaran WACC File
#'
#' @param file_path The path of the file relative to `pc/` in the data library.
#'
#' @returns A tibble with the year and the renamed WACC columns, or `NULL` if
#'   the file does not exist or the download fails.
#'
#' @keywords internal
#' @noRd
download_damodaran_file <- function(file_path) {
  url <- paste0("https://pages.stern.nyu.edu/~adamodar/pc/", file_path)

  tmp <- tempfile(fileext = ".xls")
  on.exit(unlink(tmp), add = TRUE)

  # Missing years return 404, which the caller reports in a single warning.
  status <- handle_download_error(
    function() {
      response <- httr2::request(url) |>
        httr2::req_user_agent(get_random_user_agent()) |>
        httr2::req_timeout(seconds = 60) |>
        httr2::req_retry(max_tries = 3) |>
        httr2::req_error(is_error = function(resp) FALSE) |>
        httr2::req_perform(path = tmp)
      httr2::resp_status(response)
    },
    fallback = NA_integer_
  )

  if (is.na(status) || status == 404) {
    return(NULL)
  }
  if (status >= 400) {
    cli::cli_inform("Failed to download {.url {url}} (HTTP status {status}).")
    return(NULL)
  }

  handle_download_error(parse_damodaran_wacc, tmp)
}

#' Parse a Damodaran WACC Workbook
#'
#' The position of the sheet and of the header row differ across years, so the
#' function searches every sheet for the row that starts with "Industry Name".
#'
#' @param path The path of the downloaded workbook.
#'
#' @returns A tibble with the column `year` (from the update date, `NA` if the
#'   file has none) and the renamed WACC columns.
#'
#' @keywords internal
#' @noRd
parse_damodaran_wacc <- function(path) {
  raw_data <- NULL
  for (sheet in readxl::excel_sheets(path)) {
    sheet_data <- suppressMessages(readxl::read_excel(
      path,
      sheet = sheet,
      col_names = FALSE,
      col_types = "text"
    ))
    if (ncol(sheet_data) > 0 && "Industry Name" %in% sheet_data[[1]]) {
      raw_data <- sheet_data
      break
    }
  }

  if (is.null(raw_data)) {
    cli::cli_abort("Could not find the industry table in the workbook.")
  }

  # The update date is an Excel date serial in January of the following year.
  date_updated <- raw_data[[2]][raw_data[[1]] %in% "Date updated:"]
  date_updated <- suppressWarnings(as.numeric(date_updated[1]))
  data_year <- year(as.Date(date_updated, origin = "1899-12-30")) - 1L

  header_row <- which(raw_data[[1]] == "Industry Name")[1]
  header <- trimws(gsub("\\s+", " ", unlist(raw_data[header_row, ])))

  column_map <- c(
    "Industry Name" = "industry",
    "Number of Firms" = "number_of_firms",
    "Beta" = "beta",
    "Cost of Equity" = "cost_of_equity",
    "E/(D+E)" = "equity_to_capital",
    "Std Dev in Stock" = "sd_stock",
    "Cost of Debt" = "cost_of_debt",
    "Tax Rate" = "tax_rate",
    "After-tax Cost of Debt" = "after_tax_cost_of_debt",
    "D/(D+E)" = "debt_to_capital",
    "Cost of Capital" = "wacc"
  )
  # The local currency column is named after the currency, e.g., "(Euros)".
  local_column <- grepl("^Cost of Capital \\(.+\\)$", header)
  new_names <- unname(column_map[header])
  new_names[local_column] <- "wacc_local"

  keep <- !is.na(new_names)
  missing_columns <- setdiff(unname(column_map), new_names)
  if (length(missing_columns) > 0) {
    cli::cli_abort("Missing columns: {.val {missing_columns}}")
  }

  processed_data <- raw_data[-seq_len(header_row), keep]
  names(processed_data) <- new_names[keep]

  if (!"wacc_local" %in% names(processed_data)) {
    processed_data$wacc_local <- NA_character_
  }

  processed_data |>
    filter(!is.na(.data$industry), !is.na(.data$number_of_firms)) |>
    mutate(
      industry = trimws(.data$industry),
      across(-"industry", ~ suppressWarnings(as.numeric(.))),
      number_of_firms = as.integer(.data$number_of_firms),
      year = data_year,
      .before = 1
    )
}
