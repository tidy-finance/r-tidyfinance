#' List parquet files in a Hugging Face dataset
#'
#' Query the Hugging Face Datasets API and return a tibble of files with a
#' `.parquet` suffix. The function follows pagination links returned in the
#' response `Link` header and returns path, size and a resolved URL.
#'
#' @param organization Character(1). Hugging Face organization or user name.
#' @param dataset Character(1). Dataset name under the organization.
#' @return A tibble with columns: path (character), size (numeric) and url (character).
#' @details Uses httr2 to perform HTTP requests. Requires internet access and
#'   the dataset to be publicly accessible or accessible with appropriate auth.
#' @examples
#' \dontrun{
#' get_available_hf_files("voigtstefan", "sp500")
#' }
#'
#' @export
get_available_hf_files <- function(organization, dataset) {
  api_url <- paste0(
    "https://huggingface.co/api/datasets/",
    organization,
    "/",
    dataset,
    "/tree/main?recursive=1"
  )

  out <- tibble::tibble(NULL)
  next_url <- api_url

  repeat {
    resp <- httr2::request(next_url) |>
      httr2::req_user_agent("httr2") |>
      httr2::req_perform()

    body <- resp |>
      httr2::resp_body_string() |>
      jsonlite::fromJSON(simplifyDataFrame = TRUE) |>
      tibble::tibble() |>
      dplyr::filter(
        .data$type == "file" &
          grepl("\\.parquet$", .data$path, ignore.case = TRUE)
      ) |>
      dplyr::select("path", "size")

    out <- out |>
      dplyr::bind_rows(tibble::tibble(data = list(body)))
    link <- httr2::resp_headers(resp)$link
    if (is.null(link) || !grepl('rel="next"', link)) {
      break
    }
    next_url <- sub(".*<([^>]+)>; rel=\"next\".*", "\\1", link)
  }

  out |>
    tidyr::unnest(.data$data) |>
    dplyr::mutate(
      url = glue::glue(
        "https://huggingface.co/datasets/{organization}/{dataset}/resolve/main/{path}"
      )
    )
}

#' Download 5-second aggregated orderbook data
#' for the SPY ETF (S&P 500 tracker) from a Hugging Face dataset.
#'
#' Find parquet files in a dataset whose paths include `date=YYYY-MM-DD`,
#' filter them to the provided date interval, then read and row-bind their
#' contents using `arrow::read_parquet()`.
#'
#' @param type description
#' @param start_date Date or character. Start date (inclusive) in "YYYY-MM-DD" format. If NULL, the internal default is used.
#' @param end_date Date or character. End date (inclusive) in "YYYY-MM-DD" format. If NULL, the internal default is used.
#'
#' @return A tibble with processed data, including dates and the relevant
#'   financial metrics, filtered by the specified date range.
#'
#' @details The function locates parquet files in the specified Hugging Face dataset whose paths include `date=YYYY-MM-DD`,
#'   filters them to the provided date interval, and reads/row-binds them with arrow::read_parquet().
#'
#' @examples
#' \dontrun{
#' # Download 5-second aggregated orderbook snapshots for SPY.
#'   download_data_hf("hf_high_frequency_sp500", "2007-07-26", "2007-07-27")
#' }
#'
#' @export
download_data_hf <- function(
  type,
  start_date = "2007-06-27",
  end_date = "2007-07-27"
) {
  if (type == "hf_high_frequency_sp500") {
    organization = "voigtstefan"
    dataset = "sp500"

    date_pattern <- "date=([0-9]{4}-[0-9]{2}-[0-9]{2})"
    available_files <- get_available_hf_files(organization, dataset) |>
      dplyr::mutate(
        date = as.Date(stringr::str_match(.data$path, date_pattern)[, 2])
      )

    tibble::tibble(
      date = seq.Date(as.Date(start_date), as.Date(end_date), by = "day")
    ) |>
      dplyr::inner_join(available_files, by = "date") |>
      dplyr::transmute(
        data = purrr::map(url, ~ arrow::read_parquet(.x))
      ) |>
      tidyr::unnest(.data$data)
  }
}
