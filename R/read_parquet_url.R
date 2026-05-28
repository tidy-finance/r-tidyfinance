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
  httr2::request(url) |>
    httr2::req_user_agent(get_random_user_agent()) |>
    httr2::req_perform(path = tmp)
  nanoparquet::read_parquet(tmp)
}
