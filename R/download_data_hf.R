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
#' get_available_hugging_face_files("voigtstefan", "sp500")
#' }
#' @importFrom jsonlite fromJSON
#' @importFrom stringr str_match
#' @importFrom arrow read_parquet
#' @export
get_available_hugging_face_files <- function(organization, dataset) {
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
        type == "file" & grepl("\\.parquet$", path, ignore.case = TRUE)
      ) |>
      dplyr::select(path, size)

    out <- out |> dplyr::bind_rows(tibble::tibble(data = list(body)))
    link <- httr2::resp_headers(resp)$link
    if (is.null(link) || !grepl('rel="next"', link)) {
      break
    }
    next_url <- sub(".*<([^>]+)>; rel=\"next\".*", "\\1", link)
  }

  out |>
    tidyr::unnest(data) |>
    dplyr::mutate(
      url = glue::glue(
        "https://huggingface.co/datasets/{organization}/{dataset}/resolve/main/{path}"
      )
    )
}

#' Read parquet files from a Hugging Face dataset for a date range
#'
#' Find parquet files in a dataset whose paths include `date=YYYY-MM-DD`,
#' filter them to the provided date interval, then read and row-bind their
#' contents using `arrow::read_parquet()`.
#'
#' @param start_date Date or character. Start date (inclusive) in `YYYY-MM-DD` format.
#' @param end_date Date or character. End date (inclusive) in `YYYY-MM-DD` format.
#' @param organization Character(1). Hugging Face organization or user name.
#' @param dataset Character(1). Dataset name under the organization.
#' @return A tibble created by row-binding the parquet files that match the date range.
#' @details The function expects file paths to contain a substring matching
#'   `date=YYYY-MM-DD`. It calls `get_available_hugging_face_files()` internally.
#'   Reading remote parquet files relies on `arrow` supporting the provided URL.
#' @examples
#' \dontrun{
#' request_hf_data("2025-01-01", "2025-01-10", "voigtstefan", "sp500")
#' }
#' @importFrom arrow read_parquet
#' @importFrom stringr str_match
#' @importFrom purrr map
#' @importFrom tidyr unnest
#' @importFrom tibble tibble
#' @importFrom dplyr inner_join transmute mutate
#' @export
request_hf_data <- function(
  start_date = "2007-06-27",
  end_date = "2007-07-27",
  organization = "voigtstefan",
  dataset = "sp500"
) {
  date_pattern <- "date=([0-9]{4}-[0-9]{2}-[0-9]{2})"
  available_files <- get_available_hugging_face_files(organization, dataset) |>
    dplyr::mutate(date = as.Date(stringr::str_match(path, date_pattern)[, 2]))

  tibble::tibble(
    date = seq.Date(as.Date(start_date), as.Date(end_date), by = "day")
  ) |>
    dplyr::inner_join(available_files, by = "date") |>
    dplyr::transmute(
      data = purrr::map(url, ~ arrow::read_parquet(.x))
    ) |>
    tidyr::unnest(data)
}

#' Download SPY 5-second orderbook data from Hugging Face
#'
#' Wrapper around request_hf_data() to download 5-second aggregated orderbook data
#' for the SPY ETF (S&P 500 tracker) from a Hugging Face dataset.
#'
#' @param start_date Date or character. Start date (inclusive) in "YYYY-MM-DD" format. If NULL, the internal default is used.
#' @param end_date Date or character. End date (inclusive) in "YYYY-MM-DD" format. If NULL, the internal default is used.
#' @param organization Character(1). Hugging Face organization or user name. Defaults to "voigtstefan".
#' @param dataset Character(1). Dataset name under the organization. Defaults to "sp500".
#' @return A tibble of 5-second level observations. Typical columns:
#'   \itemize{
#'     \item ts: POSIXct timestamp of the row
#'     \item midquote: numeric mid price
#'     \item signed_volume: numeric signed volume
#'     \item trading_volume: numeric trading volume
#'     \item depth0_ask, depth0_bid, depth5_ask, depth5_bid: numeric orderbook depth measures
#'     \item spread: numeric bid–ask spread
#'   }
#' @details The function locates parquet files in the specified Hugging Face dataset whose paths include `date=YYYY-MM-DD`,
#'   filters them to the provided date interval, and reads/row-binds them with arrow::read_parquet().
#'   The dataset contains 5-second aggregated orderbook snapshots for SPY.
#' @examples
#' \dontrun{
#' download_data_hf("2025-01-01", "2025-01-10")
#' }
#' @seealso request_hf_data, get_available_hugging_face_files
#' @export
download_data_hf <- function(
  start_date = NULL,
  end_date = NULL,
  organization = "voigtstefan",
  dataset = "sp500"
) {
  request_hf_data("2025-01-01", "2025-01-10")
}
