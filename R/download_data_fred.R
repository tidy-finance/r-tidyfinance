#' Download and Process Data from FRED
#'
#' Downloads a specified data series from the Federal Reserve Economic Data
#' (FRED) website, processes the data, and returns it as a tibble.
#'
#' @details
#' Constructs the URL based on the provided FRED series ID, performs an HTTP
#' GET request to download the data in CSV format, and processes it to a tidy
#' tibble format. The resulting tibble includes the date, value, and the series
#' ID.
#'
#' This approach is inspired by `quantmod::getSymbolsFRED()` which uses a
#' different wrapper around the same FRED download data site. If you want to
#' systematically download FRED data via API, please consider using the `fredr`
#' package.
#'
#' @param series A character vector specifying the FRED series ID(s) to
#'   download.
#' @param start_date Optional. A character string or Date object in
#'   "YYYY-MM-DD" format specifying the start date for the data. If not
#'   provided, the full dataset is returned.
#' @param end_date Optional. A character string or Date object in
#'   "YYYY-MM-DD" format specifying the end date for the data. If not
#'   provided, the full dataset is returned.
#'
#' @returns A tibble containing the processed data with three columns:
#' \describe{
#'   \item{date}{The date corresponding to the data point.}
#'   \item{value}{The value of the data series at that date.}
#'   \item{series}{The FRED series ID corresponding to the data.}
#' }
#'
#' @family download functions
#' @export
#'
#' @examples
#' \donttest{
#'   download_data_fred("CPIAUCNS")
#'   download_data_fred(c("GDP", "CPIAUCNS"), "2010-01-01", "2010-12-31")
#' }
#'
download_data_fred <- function(series, start_date = NULL, end_date = NULL) {
  dates <- validate_dates(start_date, end_date)
  start_date <- dates$start_date
  end_date <- dates$end_date

  fred_processed <- list()

  cli::cli_progress_bar(
    "Downloading series",
    total = length(series),
    clear = TRUE
  )
  for (j in seq_along(series)) {
    url <- paste0(
      "https://fred.stlouisfed.org/graph/fredgraph.csv?id=",
      series[j]
    )

    response <- handle_download_error(
      function() {
        httr2::request(url) |>
          httr2::req_error(is_error = \(resp) FALSE) |>
          httr2::req_perform()
      },
      fallback = NULL
    )

    if (!is.null(response)) {
      if (response$status_code == 200) {
        fred_raw <- suppressWarnings(httr2::resp_body_string(response)) |>
          textConnection() |>
          read.csv() |>
          as_tibble()

        fred_processed[[j]] <- fred_raw |>
          mutate(
            date = as.Date(.data$observation_date),
            value = as.numeric(.data[[series[j]]]),
            series = series[j],
            .keep = "none"
          )
      } else {
        cli::cli_warn(
          paste(
            "Failed to retrieve data for series {series[j]}",
            "with status code {response$status_code}."
          )
        )
        fred_processed[[j]] <- tibble(
          date = Date(),
          value = numeric(),
          series = character()
        )
      }
    } else {
      fred_processed[[j]] <- tibble(
        date = Date(),
        value = numeric(),
        series = character()
      )
    }

    cli::cli_progress_update()
  }

  fred_processed <- bind_rows(fred_processed)

  if (!is.null(start_date) && !is.null(end_date)) {
    fred_processed <- fred_processed |>
      filter(
        .data$date >= start_date & .data$date <= end_date
      )
  }

  fred_processed
}

#' Download and Process FRED-MD / FRED-QD (McCracken-Ng) Databases
#'
#' Downloads a vintage of the FRED-MD (monthly) or FRED-QD (quarterly)
#' macroeconomic database - a curated, balanced panel of macro series
#' maintained by the Federal Reserve Bank of St. Louis - and returns it as a
#' wide tibble (one column per series), matching the layout of other
#' factor/predictor datasets such as `download_data_factors_ff()`.
#'
#' @details
#' Each series carries a McCracken-Ng stationarity transform code (tcode,
#' 1-7); `transform = TRUE` applies it per series (all transforms are causal,
#' i.e. point-in-time safe). FRED-MD/QD publish a new vintage every month, so
#' a specific historical release can be requested via `vintage`, or the
#' entire real-time panel via `vintage = "all"`, for point-in-time analysis
#' that avoids look-ahead bias from data revisions. Recent vintages are
#' hosted individually; older ones are extracted from the St. Louis Fed
#' historical vintage archive ZIP files.
#'
#' @param database Which database to download: `"FRED-MD"` (monthly) or
#'   `"FRED-QD"` (quarterly). The frequency is implied by the database.
#' @param transform Logical. If `TRUE`, apply each series' McCracken-Ng
#'   stationarity transform (tcode 1-7). If `FALSE` (the default), return raw
#'   levels.
#' @param vintage Which release(s) to download: `"latest"` (the default, the
#'   current vintage), a `"YYYY-MM"` label for one historical release (e.g.
#'   `"2020-03"`; recent ones are hosted individually, older ones are
#'   extracted from the St. Louis Fed vintage archives), or `"all"` for every
#'   archived vintage stacked (the full real-time panel).
#'
#' @returns A tibble `[date, <series...>]`. For a specific `vintage` or
#'   `"all"`, a `vintage` column (the `"YYYY-MM"` release label) is inserted
#'   after `date`.
#'
#' @references
#'   McCracken, M. W., & Ng, S. (2016). FRED-MD: A monthly database for
#'   macroeconomic research. *Journal of Business & Economic Statistics*,
#'   34(4), 574-589. \doi{10.1080/07350015.2015.1086655}
#'
#'   McCracken, M. W., & Ng, S. (2021). FRED-QD: A quarterly database for
#'   macroeconomic research. *Federal Reserve Bank of St. Louis Review*,
#'   103(1), 1-44. \doi{10.20955/r.103.1-44}
#'
#' @family download functions
#' @export
#'
#' @examples
#' \donttest{
#'   download_data_fred_md("FRED-MD")
#'   download_data_fred_md("FRED-MD", transform = TRUE)
#'   download_data_fred_md("FRED-MD", vintage = "2020-03")
#'   download_data_fred_md("FRED-MD", vintage = "all")
#'   download_data_fred_md("FRED-QD")
#' }
download_data_fred_md <- function(
  database = "FRED-MD",
  transform = FALSE,
  vintage = "latest"
) {
  check_supported_dataset_fred_md(database)
  spec <- fred_md_spec()[[database]]

  if (vintage == "latest") {
    url <- paste0(fred_md_base_url(), "/", spec$sub, "/current.csv")
    text <- handle_download_error(
      function() fetch_fred_md_text(url),
      fallback = ""
    )
    if (!looks_like_fred_md(text)) {
      cli::cli_abort(paste(
        "The FRED-MD/QD {.val current} file was not a valid CSV;",
        "try again later."
      ))
    }
    return(fred_md_wide(text, transform))
  }

  if (vintage == "all") {
    return(download_fred_md_all(database, transform))
  }

  if (!grepl("^\\d{4}-\\d{2}$", vintage)) {
    cli::cli_abort(paste0(
      "{.arg vintage} must be {.val latest}, {.val all}, or a ",
      "{.val YYYY-MM} label; got {.val {vintage}}."
    ))
  }

  frame <- download_fred_md_vintage(database, vintage, transform)
  tibble::add_column(frame, vintage = vintage, .after = "date")
}

# Helper functions --------------------------------------------------------

#' Base URL for the FRED-MD/QD (McCracken-Ng) data files
#' @noRd
fred_md_base_url <- function() {
  paste0(
    "https://www.stlouisfed.org/-/media/project/frbstl/stlouisfed/",
    "research/fred-md"
  )
}

#' Per-database spec: subdirectory, individual-vintage filename suffix
#' ('md'/'qd'), and full-history archive routing ranges (first, last, zip
#' url). A specific historical vintage is extracted from whichever archive
#' covers it; more recent vintages are hosted individually.
#' @noRd
fred_md_spec <- function() {
  base_url <- fred_md_base_url()
  list(
    "FRED-MD" = list(
      sub = "monthly",
      suffix = "md",
      archives = list(
        list(
          lo = "1999-01",
          hi = "2014-12",
          url = paste0(base_url, "/historical_fred-md.zip")
        ),
        list(
          lo = "2015-01",
          hi = "2024-12",
          url = paste0(
            base_url,
            "/historical-vintages-of-fred-md-2015-01-to-2024-12.zip"
          )
        )
      )
    ),
    "FRED-QD" = list(
      sub = "quarterly",
      suffix = "qd",
      archives = list(
        list(
          lo = "2018-05",
          hi = "2024-12",
          url = paste0(
            base_url,
            "/historical-vintages-of-fred-qd-2018-05-to-2024-12.zip"
          )
        )
      )
    )
  )
}

#' Check that a FRED-MD/QD database name is supported
#' @noRd
check_supported_dataset_fred_md <- function(database) {
  if (!database %in% names(fred_md_spec())) {
    cli::cli_abort(c(
      "Unsupported database: {.val {database}}",
      "i" = "Use {.val FRED-MD} or {.val FRED-QD}."
    ))
  }
}

#' Apply a McCracken-Ng stationarity transform (tcode 1-7) to a level series
#'
#' 1 level, 2 diff, 3 diff^2, 4 log, 5 dlog, 6 dlog^2, 7 diff(x_t/x_{t-1} - 1).
#' All are causal (use only current and past values).
#' @noRd
apply_fred_md_tcode <- function(x, code) {
  x <- as.numeric(x)
  if (code == 1) {
    x
  } else if (code == 2) {
    c(NA, diff(x))
  } else if (code == 3) {
    c(NA, NA, diff(x, differences = 2))
  } else if (code == 4) {
    log(x)
  } else if (code == 5) {
    c(NA, diff(log(x)))
  } else if (code == 6) {
    c(NA, NA, diff(log(x), differences = 2))
  } else if (code == 7) {
    g <- c(NA, x[-1] / x[-length(x)] - 1)
    c(NA, diff(g))
  } else {
    cli::cli_abort("Unknown FRED-MD tcode {.val {code}} (expected 1-7).")
  }
}

#' GET a FRED-MD/QD CSV as text; raises on a non-200 response
#'
#' Forces HTTP/1.1: the St. Louis Fed media host (Akamai-fronted) has been
#' observed to hang or reset the connection on HTTP/2 requests, and on
#' requests carrying a spoofed browser User-Agent whose TLS fingerprint
#' doesn't match a real browser - so, unlike the other download functions in
#' this package, no `User-Agent` override is sent here.
#' @noRd
fetch_fred_md_text <- function(url) {
  response <- httr2::request(url) |>
    httr2::req_timeout(seconds = 60) |>
    httr2::req_options(http_version = 2) |>
    httr2::req_error(is_error = \(resp) FALSE) |>
    httr2::req_perform()
  if (response$status_code != 200) {
    cli::cli_abort(
      "Failed to download {.url {url}} (status {response$status_code})."
    )
  }
  httr2::resp_body_string(response)
}

#' GET a FRED-MD/QD vintage archive (ZIP) as raw bytes
#' @noRd
fetch_fred_md_bytes <- function(url) {
  response <- httr2::request(url) |>
    httr2::req_timeout(seconds = 180) |>
    httr2::req_options(http_version = 2) |>
    httr2::req_error(is_error = \(resp) FALSE) |>
    httr2::req_perform()
  if (response$status_code != 200) {
    cli::cli_abort(
      "Failed to download {.url {url}} (status {response$status_code})."
    )
  }
  httr2::resp_body_raw(response)
}

#' `TRUE` if `text` looks like a FRED-MD/QD CSV (first cell is `sasdate`)
#'
#' Guards against 200-OK HTML placeholder pages for unpublished months.
#' @noRd
looks_like_fred_md <- function(text) {
  if (is.null(text) || !nzchar(trimws(text))) {
    return(FALSE)
  }
  lines <- strsplit(text, "\r?\n")[[1]]
  first_line <- trimws(lines[nzchar(trimws(lines))][1])
  first_cell <- trimws(strsplit(first_line, ",")[[1]][1])
  identical(tolower(first_cell), "sasdate")
}

#' Extract a `"YYYY-MM"` vintage label from an archived filename, or `NA`
#' @noRd
vintage_label <- function(name) {
  m <- regmatches(name, regexpr("\\d{4}-\\d{2}", name))
  if (length(m) == 1) {
    parts <- as.integer(strsplit(m, "-")[[1]])
    return(sprintf("%04d-%02d", parts[1], parts[2]))
  }
  m <- regmatches(name, regexpr("\\d{4}m\\d{1,2}", name))
  if (length(m) == 1) {
    year <- as.integer(substr(m, 1, 4))
    month <- as.integer(sub("^\\d{4}m", "", m))
    return(sprintf("%04d-%02d", year, month))
  }
  NA_character_
}

#' Parse one FRED-MD/QD vintage CSV into a wide frame `[date, <series...>]`
#'
#' Row 1 is the header (`sasdate` + series), row 2 the `Transform:` row of
#' tcodes, and the body the levels. With `transform = TRUE` each series'
#' McCracken-Ng stationarity transform (tcode) is applied per vintage
#' (causal, so point-in-time safe); `transform = FALSE` keeps raw levels.
#' @noRd
fred_md_wide <- function(text, transform) {
  raw <- utils::read.csv(
    text = text,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  date_col <- names(raw)[1]
  series_cols <- names(raw)[-1]
  tcodes <- vapply(
    series_cols,
    function(col) as.integer(round(as.numeric(raw[[col]][1]))),
    integer(1)
  )

  body <- raw[-1, , drop = FALSE]
  dates <- as.Date(body[[date_col]], format = "%m/%d/%Y")
  keep <- !is.na(dates)
  body <- body[keep, , drop = FALSE]
  dates <- dates[keep]

  levels <- lapply(series_cols, function(col) as.numeric(body[[col]]))
  names(levels) <- series_cols

  if (transform) {
    levels <- lapply(series_cols, function(col) {
      apply_fred_md_tcode(levels[[col]], tcodes[[col]])
    })
    names(levels) <- series_cols
  }

  dplyr::bind_cols(tibble::tibble(date = dates), tibble::as_tibble(levels))
}

#' Download a vintage archive (ZIP) and parse its CSVs
#'
#' Returns a named list `vintage_label -> wide_frame`. `want` returns just
#' that one vintage (stops early); otherwise every vintage in the archive.
#' @noRd
read_fred_md_archive <- function(url, transform, want = NULL) {
  bytes <- fetch_fred_md_bytes(url)

  tmp_zip <- tempfile(fileext = ".zip")
  on.exit(unlink(tmp_zip), add = TRUE)
  writeBin(bytes, tmp_zip)

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

  out <- list()
  for (path in csv_files) {
    label <- vintage_label(basename(path))
    if (is.na(label) || label %in% names(out)) {
      next
    }
    if (!is.null(want) && label != want) {
      next
    }
    text <- paste(
      readLines(path, warn = FALSE, encoding = "latin1"),
      collapse = "\n"
    )
    if (looks_like_fred_md(text)) {
      out[[label]] <- fred_md_wide(text, transform)
      if (!is.null(want)) {
        break
      }
    }
  }
  out
}

#' Candidate individually-hosted filenames for a vintage
#'
#' E.g. `"2025-04-md.csv"`, `"fred-md_2025m04.csv"`, `"2025-04.csv"`.
#' @noRd
fred_md_individual_names <- function(label, suffix) {
  parts <- strsplit(label, "-")[[1]]
  year <- parts[1]
  month <- parts[2]
  c(
    paste0(label, "-", suffix, ".csv"),
    paste0("fred-", suffix, "_", year, "m", month, ".csv"),
    paste0(label, ".csv")
  )
}

#' One historical release: from the covering archive if any, else the
#' individually-hosted file
#' @noRd
download_fred_md_vintage <- function(database, vintage, transform) {
  spec <- fred_md_spec()[[database]]

  for (archive in spec$archives) {
    if (vintage >= archive$lo && vintage <= archive$hi) {
      found <- read_fred_md_archive(archive$url, transform, want = vintage)
      if (vintage %in% names(found)) {
        return(found[[vintage]])
      }
      cli::cli_abort(
        "Vintage {.val {vintage}} not found in its FRED-MD/QD archive."
      )
    }
  }

  for (name in fred_md_individual_names(vintage, spec$suffix)) {
    url <- paste0(fred_md_base_url(), "/", spec$sub, "/", name)
    text <- tryCatch(fetch_fred_md_text(url), error = function(e) NULL)
    if (!is.null(text) && looks_like_fred_md(text)) {
      return(fred_md_wide(text, transform))
    }
  }

  cli::cli_abort(paste(
    "Could not fetch FRED-MD/QD vintage {.val {vintage}} (not in an",
    "archive and not individually hosted - it may be unpublished)."
  ))
}

#' Every archived vintage plus the individually-hosted recent ones, stacked
#' into a wide real-time panel
#' @noRd
download_fred_md_all <- function(database, transform) {
  spec <- fred_md_spec()[[database]]
  frames <- list()
  last_archived <- NULL
  for (archive in spec$archives) {
    frames <- utils::modifyList(
      frames,
      read_fred_md_archive(archive$url, transform)
    )
    last_archived <- if (is.null(last_archived)) {
      archive$hi
    } else {
      max(last_archived, archive$hi)
    }
  }

  recent_start <- if (is.null(last_archived)) "2015-01" else last_archived
  recent_labels <- format(
    seq(
      as.Date(paste0(recent_start, "-01")),
      as.Date(paste0(format(Sys.Date(), "%Y-%m"), "-01")),
      by = "month"
    ),
    "%Y-%m"
  )

  for (label in recent_labels) {
    if (label %in% names(frames)) {
      next
    }
    for (name in fred_md_individual_names(label, spec$suffix)) {
      url <- paste0(fred_md_base_url(), "/", spec$sub, "/", name)
      text <- tryCatch(fetch_fred_md_text(url), error = function(e) NULL)
      if (!is.null(text) && looks_like_fred_md(text)) {
        frames[[label]] <- fred_md_wide(text, transform)
        break
      }
    }
  }

  if (length(frames) == 0) {
    cli::cli_abort("No FRED-MD/QD vintages could be downloaded.")
  }

  parts <- lapply(names(frames), function(label) {
    tibble::add_column(frames[[label]], vintage = label, .after = "date")
  })

  dplyr::bind_rows(parts) |>
    dplyr::arrange(.data$vintage, .data$date)
}
