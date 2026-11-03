#' Download a Parquet File from a URL
#'
#' Internal helper that downloads a parquet file from Hugging Face. It retries
#' a download that Hugging Face refused because of its rate limit (HTTP 429)
#' after the limit resets, twice at most.
#'
#' @param url Character(1). URL of the parquet file.
#' @param path Character(1). Path to write the file to.
#'
#' @returns The path, invisibly.
#' @keywords internal
#' @noRd
download_parquet_url <- function(url, path) {
  httr2::request(url) |>
    httr2::req_user_agent(get_random_user_agent()) |>
    httr2::req_retry(
      max_tries = 3,
      is_transient = \(resp) httr2::resp_status(resp) == 429,
      after = rate_limit_reset
    ) |>
    httr2::req_perform(path = path)
  invisible(path)
}

#' Read a Parquet File from a URL
#'
#' Internal helper that downloads a parquet file to a temporary location
#' via httr2 and reads it with nanoparquet. Used by HuggingFace-backed
#' downloaders.
#'
#' @param url Character(1). URL of the parquet file.
#'
#' @returns A data frame.
#' @keywords internal
#' @noRd
read_parquet_url <- function(url) {
  tmp <- tempfile(fileext = ".parquet")
  on.exit(unlink(tmp), add = TRUE)
  download_parquet_url(url, tmp)
  nanoparquet::read_parquet(tmp)
}
