#' Download and Process Pastor-Stambaugh Liquidity Factors
#'
#' Downloads and processes the liquidity factor data of Pastor and Stambaugh
#' (2003) from
#' [Lubos Pastor's data library](https://faculty.chicagobooth.edu/lubos-pastor/data).
#' The source is a whitespace-delimited text file whose header lines start with
#' a percent sign. The function reads the three liquidity series, aligns the
#' monthly date to the beginning of the month, and optionally filters the data
#' based on a provided date range.
#'
#' The series are already expressed as plain numeric (decimal) values in the
#' source data, so no rescaling is applied. The traded liquidity factor is only
#' available from 1968 onward; earlier observations are coded as `-99` in the
#' source file and are returned as `NA`.
#'
#' @param start_date Optional. A character string or Date object in
#'   "YYYY-MM-DD" format specifying the start date for the data. If not
#'   provided, the full dataset is returned.
#' @param end_date Optional. A character string or Date object in "YYYY-MM-DD"
#'   format specifying the end date for the data. If not provided, the full
#'   dataset is returned.
#' @param url A character string with the URL of the liquidity data file.
#'   Because the file name embeds the last year of data, the default points to
#'   the most recent file known at release time; override it when a newer file
#'   becomes available.
#'
#' @returns A tibble with the columns `date` (aligned to the beginning of the
#'   month), `agg_liq` (levels of aggregate liquidity), `innov_liq`
#'   (innovations in aggregate liquidity, the non-traded liquidity factor), and
#'   `traded_liq` (the traded liquidity factor LIQ_V), filtered by the
#'   specified date range if `start_date` and `end_date` are provided.
#'
#' @references
#'   Pastor, L., & Stambaugh, R. F. (2003). Liquidity risk and expected stock
#'   returns. *Journal of Political Economy*, 111(3), 642-685.
#'   \doi{10.1086/374184}
#'
#' @family download functions
#' @export
#'
#' @examples
#' \donttest{
#'   pastor_stambaugh <- download_data_pastor_stambaugh(
#'     start_date = "2020-01-01", end_date = "2020-12-31"
#'   )
#' }
download_data_pastor_stambaugh <- function(
  start_date = NULL,
  end_date = NULL,
  url = paste0(
    "https://faculty.chicagobooth.edu/-/media/faculty/lubos-pastor/data/",
    "liq_data_1962_2025.txt"
  )
) {
  dates <- validate_dates(start_date, end_date)
  start_date <- dates$start_date
  end_date <- dates$end_date

  raw_data <- handle_download_error(
    function() {
      suppressWarnings(
        as_tibble(read.table(
          url,
          comment.char = "%",
          header = FALSE,
          col.names = c("month", "agg_liq", "innov_liq", "traded_liq")
        ))
      )
    },
    fallback = tibble()
  )

  if (nrow(raw_data) == 0) {
    cli::cli_inform("Returning an empty data set due to download failure.")
    return(raw_data)
  }

  # The traded factor is coded -99 before it becomes available (1968).
  processed_data <- raw_data |>
    mutate(
      date = floor_date(ym(.data$month), "month"),
      across(
        c("agg_liq", "innov_liq", "traded_liq"),
        ~ na_if(., -99)
      )
    ) |>
    select("date", "agg_liq", "innov_liq", "traded_liq")

  if (!is.null(start_date) && !is.null(end_date)) {
    processed_data <- processed_data |>
      filter(between(.data$date, start_date, end_date))
  }

  processed_data
}
