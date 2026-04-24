#' Check That New Columns Do Not Overwrite Existing Data
#'
#' Internal helper used before creating columns in a data frame to warn about
#' silently overwriting user-supplied data. Warns with an informative error if
#' any of the supplied column names already exist in `data`.
#'
#' @param data A data frame (or tibble) that is about to receive new columns.
#' @param cols A character vector of one or more column names that are about
#'   to be created.
#'
#' @returns Invisibly returns `data` to allow the helper to be used inside a
#'   pipe. When a column would be overwritten, the function issues a warning.
#'
#' @keywords internal
#' @noRd
check_new_col <- function(data, cols) {
  existing <- cols[cols %in% colnames(data)]
  if (length(existing) > 0L) {
    cli::cli_warn(
      c(
        "New column{?s} would overwrite existing column{?s} in {.arg data}.",
        "x" = "Already present: {.val {existing}}.",
        "i" = "Rename or remove the existing column{?s} before proceeding."
      )
    )
  }
  invisible(data)
}
