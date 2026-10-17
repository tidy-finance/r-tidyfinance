#' Download and Process Open Source Asset Pricing Data
#'
#' Downloads the data from
#' [Open Source Asset Pricing](https://www.openassetpricing.com/data/)
#' from Google Drive using a specified file ID, processes the data
#' by converting column names to snake_case, aligning the date to the
#' beginning of the month, scaling the percentage long-short returns to
#' numeric values, and optionally filters the data based on a provided
#' date range.
#'
#' The dataset contains monthly long-short returns of the predictor
#' portfolios. Every column other than `date` is a return expressed in
#' percent, so all of them are divided by 100 to convert them into plain
#' numeric (decimal) returns.
#'
#' @param start_date Optional. A character string or Date object in
#'   "YYYY-MM-DD" format specifying the start date for the data. If
#'   not provided, the full dataset is returned.
#' @param end_date Optional. A character string or Date object in
#'   "YYYY-MM-DD" format specifying the end date for the data. If not
#'   provided, the full dataset is returned.
#' Google Drive limits how often a shared file can be downloaded. When the
#' limit is reached, Google Drive returns an error page instead of the data
#' and the function returns an empty tibble. The limit is usually lifted
#' within 24 hours.
#'
#' @param sheet_id A character string with the Google Drive file ID of the
#'   wide CSV of monthly long-short returns. The default,
#'   `"10sOryk_ddjkXagaajTKUk1nwJs2ZLRiI"`, is the October 2025 release
#'   (v2.0.0). Pass the ID of another release to download it instead.
#'
#' @returns A tibble containing the processed data. The column names
#'   are converted to snake_case, the `date` column is aligned to the
#'   beginning of the month, all predictor columns (long-short returns
#'   in percent) are divided by 100 to obtain plain numeric (decimal)
#'   returns, and the data is filtered by the specified date range if
#'   `start_date` and `end_date` are provided.
#'
#' @family download functions
#' @export
#'
#' @examples
#' \donttest{
#'   osap <- download_data_osap(
#'     start_date = "2020-01-01", end_date = "2020-06-30"
#'   )
#' }
download_data_osap <- function(
  start_date = NULL,
  end_date = NULL,
  sheet_id = "10sOryk_ddjkXagaajTKUk1nwJs2ZLRiI"
) {
  dates <- validate_dates(start_date, end_date)
  start_date <- dates$start_date
  end_date <- dates$end_date

  url <- paste0("https://drive.google.com/uc?export=download&id=", sheet_id)

  raw_data <- handle_download_error(
    function() {
      response <- httr2::request(url) |>
        httr2::req_user_agent(get_random_user_agent()) |>
        httr2::req_timeout(seconds = 120) |>
        httr2::req_perform()
      body <- httr2::resp_body_string(response)

      # Google Drive answers with an HTML page instead of the CSV, e.g., when
      # the download quota of the file is exceeded.
      content_type <- httr2::resp_content_type(response)
      if (grepl("text/html", content_type, fixed = TRUE)) {
        if (grepl("Quota exceeded", body, fixed = TRUE)) {
          cli::cli_abort(paste(
            "The Google Drive download quota of the file is exceeded.",
            "Try again later, usually within 24 hours."
          ))
        }
        cli::cli_abort("Google Drive returned an HTML page instead of a CSV.")
      }

      suppressWarnings(as_tibble(read.csv(text = body)))
    },
    fallback = tibble()
  )

  if (nrow(raw_data) == 0) {
    cli::cli_inform("Returning an empty data set due to download failure.")
    return(raw_data)
  }

  processed_data <- raw_data |>
    mutate(date = floor_date(ymd(.data$date), "month"))

  colnames(processed_data) <- to_snake_case(colnames(processed_data))

  # All columns except the date are long-short returns in percent, so
  # scale them to plain numeric (decimal) returns.
  processed_data <- processed_data |>
    mutate(across(-"date", \(x) x / 100))

  if (!is.null(start_date) && !is.null(end_date)) {
    processed_data <- processed_data |>
      filter(between(.data$date, start_date, end_date))
  }

  processed_data
}

to_snake_case <- function(x) {
  x <- gsub("([a-z])([A-Z])", "\\1_\\2", x)
  x <- gsub("[^[:alnum:]_]+", "_", x)
  x <- tolower(x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  x
}
