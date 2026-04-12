#' Handle download errors gracefully
#'
#' Wraps a download function in a `tryCatch` block. On error, emits an
#' informational message and returns `fallback` instead of stopping.
#'
#' @param download_function A function to call for downloading or processing a
#'   resource.
#' @param ... Additional arguments passed to `download_function`.
#' @param fallback The value to return when `download_function` raises an
#'   error. Defaults to `NULL`.
#'
#' @returns The result of `download_function(...)`, or `fallback` on error.
#'
#' @keywords internal
#' @noRd
handle_download_error <- function(download_function, ..., fallback = NULL) {
  tryCatch(
    {
      download_function(...)
    },
    error = function(e) {
      cli::cli_inform(
        paste0(
          "Failed to download or process the resource. ",
          "The resource may not be available, or the URL may have changed. ",
          "Error message: {.message {e$message}}"
        )
      )
      fallback
    }
  )
}
