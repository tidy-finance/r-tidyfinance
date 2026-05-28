#' Download a Fama-French Factor Data File
#'
#' Downloads a Kenneth French data library ZIP archive, unzips it, and parses
#' the first data table it contains. This is an internal replacement for
#' `frenchdata::download_french_data()` that only depends on `httr2` and base
#' R. Like the original, it returns the first table only, which is the most
#' commonly used one (e.g. value-weighted returns).
#'
#' @param file_url The path of the ZIP archive relative to the Kenneth French
#'   data library base URL, as stored in the `file_url` column of
#'   `list_supported_datasets_ff()`.
#' @param max_tries Number of download attempts before giving up.
#'
#' @returns A tibble with the parsed first table. The first column is named
#'   `date` and the remaining columns keep their original names from the source
#'   file.
#'
#' @keywords internal
#' @noRd
download_french_data_factors <- function(file_url, max_tries = 5) {
  base_url <- "https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/"
  url <- paste0(base_url, file_url)

  tmp_zip <- tempfile(fileext = ".zip")
  on.exit(unlink(tmp_zip), add = TRUE)

  httr2::request(url) |>
    httr2::req_user_agent(get_random_user_agent()) |>
    httr2::req_timeout(seconds = 60) |>
    httr2::req_retry(max_tries = max_tries) |>
    httr2::req_perform(path = tmp_zip)

  tmp_dir <- tempfile()
  dir.create(tmp_dir, showWarnings = FALSE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  utils::unzip(tmp_zip, exdir = tmp_dir)
  csv_files <- list.files(
    tmp_dir,
    pattern = "\\.csv$",
    ignore.case = TRUE,
    full.names = TRUE,
    recursive = TRUE
  )
  if (length(csv_files) == 0) {
    cli::cli_abort("No CSV file found in the downloaded archive.")
  }

  parse_french_data_factors(csv_files[1])
}

#' Parse the First Table of a Fama-French CSV File
#'
#' Kenneth French CSV files start with descriptive header lines, then contain
#' one or more data tables of consecutive lines that start with a digit (the
#' date key). This helper extracts the first such table. Most factor files
#' carry a column-name row directly above the data (e.g. ",Mkt-RF,SMB,HML,RF");
#' breakpoints files have no header row at all (a prose title and a blank line
#' precede the data), in which case columns are read positionally and named
#' `date`, `V2`, `V3`, ... If annual summary rows (4-digit keys) follow the
#' periodic rows without a blank separator, they are dropped by keeping only the
#' leading rows whose date-key width matches the first data row.
#'
#' @param csv_file Path to the unzipped CSV file.
#'
#' @returns A tibble with the first column named `date` and the remaining
#'   columns keeping their original names (or positional `V*` names when the
#'   source file has no column-name row).
#'
#' @keywords internal
#' @noRd
parse_french_data_factors <- function(csv_file) {
  lines <- readLines(csv_file, warn = FALSE)

  # Data rows begin with a date key, i.e. an optional run of spaces followed by
  # a digit. The column-name row sits directly above the first such row.
  is_data <- grepl("^\\s*[0-9]", lines)
  if (!any(is_data)) {
    cli::cli_abort("Could not locate a data table in the downloaded file.")
  }

  runs <- rle(is_data)
  first_run <- which(runs$values)[1]
  preceding <- sum(runs$lengths[seq_len(first_run - 1L)])
  first_data <- preceding + 1L
  last_data <- preceding + runs$lengths[first_run]

  # A column-name row sits directly above the data only when the preceding line
  # exists, is non-blank, and is comma-delimited. Standard factor files have
  # one; breakpoints files do not (a prose title and a blank line precede the
  # data), so they are parsed positionally with header = FALSE.
  header_line <- first_data - 1L
  has_header <- header_line >= 1L &&
    !grepl("^\\s*$", lines[header_line]) &&
    grepl(",", lines[header_line])

  # Annual summary rows (4-digit keys) sometimes follow periodic rows with no
  # blank separator. Keep only the leading rows whose date-key width matches
  # the first data row, so monthly/daily tables never absorb annual rows.
  data_lines <- lines[first_data:last_data]
  key_width <- nchar(sub("^\\s*([0-9]+).*", "\\1", data_lines))
  keep <- cumprod(key_width == key_width[1]) == 1
  if (sum(!keep) > 0) {
    cli::cli_warn(c(
      "Dropped {sum(!keep)} trailing row{?s} with a different date-key width.",
      "i" = paste(
        "These are usually annual summary rows appended to a periodic table."
      )
    ))
  }
  last_data <- first_data + sum(keep) - 1L

  if (has_header) {
    block <- lines[c(header_line, first_data:last_data)]
    raw_data <- utils::read.csv(
      text = block,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  } else {
    block <- lines[first_data:last_data]
    raw_data <- utils::read.csv(
      text = block,
      header = FALSE,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  }
  names(raw_data)[1] <- "date"

  # read.csv infers integer for whole-number columns (e.g. the "Count" column
  # in breakpoints files), but the downstream pipeline applies double-valued
  # operations such as `na_if(., -99.99)`. Coerce every value column to double
  # so the parser matches frenchdata's all-numeric output.
  value_cols <- setdiff(names(raw_data), "date")
  raw_data[value_cols] <- lapply(raw_data[value_cols], as.numeric)

  tibble::as_tibble(raw_data)
}
