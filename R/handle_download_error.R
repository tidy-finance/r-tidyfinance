#' @keywords internal
#'
handle_download_error <- function(download_function, ..., fallback = NULL) {
  tryCatch(
    {
      download_function(...)
    },
    error = function(e) {
      cli::cli_inform(paste0(
        "Failed to download or process the dataset. ",
        "The resource may not be available, or the URL may have changed. ",
        "Error message: {.message {e$message}}"
      ))
      fallback
    }
  )
}
