#' Download and Process Open Source Asset Pricing Data
#'
#' This function downloads the data from [Open Source Asset Pricing](https://www.openassetpricing.com/data/)
#' from Google Sheets using a specified sheet ID, processes the data by converting column names to
#' snake_case, and optionally filters the data based on a provided date range.
#'
#' @param start_date Optional. A character string or Date object in "YYYY-MM-DD" format
#'   specifying the start date for the data. If not provided, the full dataset is returned.
#' @param end_date Optional. A character string or Date object in "YYYY-MM-DD" format
#'   specifying the start date for the data. If not provided, the full dataset is returned.
#' @param sheet_id A character string representing the Google Sheet ID from which to download the data.
#' Default is `"1JyhcF5PRKHcputlioxlu5j5GyLo4JYyY"`.
#'
#' @return A tibble containing the processed data. The column names are converted to snake_case,
#' and the data is filtered by the specified date range if `start_date` and `end_date` are provided.
#'
#' @export
#'
#' @examples
#' \donttest{
#'   osap_monthly <- download_data_osap(start_date = "2020-01-01", end_date = "2020-06-30")
#' }
download_data_osap <- function(
  start_date = NULL, end_date = NULL, sheet_id = "1JyhcF5PRKHcputlioxlu5j5GyLo4JYyY"
) {

  dates <- validate_dates(start_date, end_date)
  start_date <- dates$start_date
  end_date <- dates$end_date

  url <- paste0("https://drive.google.com/uc?export=download&id=", sheet_id)

  raw_data <- handle_download_error(
    function() suppressWarnings(
      as_tibble(read.csv(url))
    ),
    fallback = tibble()
  )

  if (nrow(raw_data) == 0) {
    cli::cli_inform("Returning an empty data set due to download failure.")
    return(raw_data)
  }

  processed_data <- raw_data |>
    mutate(date = ymd(date))

  colnames(processed_data) <- to_snake_case(colnames(processed_data))

  if (!is.null(start_date) && !is.null(end_date)) {
    processed_data <- processed_data |>
      filter(between(date, start_date, end_date))
  }

  return(processed_data)
}

to_snake_case <- function(x) {
  x <- gsub("([a-z])([A-Z])", "\\1_\\2", x)
  x <- gsub("[^[:alnum:]_]+", "_", x)
  x <- tolower(x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  return(x)
}
