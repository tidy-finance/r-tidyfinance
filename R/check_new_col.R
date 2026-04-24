#' Check That New Columns Do Not Overwrite Existing Data
#'
#' Internal helper used before creating columns in a data frame to warn about
#' silently overwriting user-supplied data. Warns with an informative message
#' if any of the supplied column names already exist in `data`.
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
check_new_col <- function(data, cols, call = rlang::caller_env()) {
  existing <- cols[cols %in% colnames(data)]
  n_existing <- length(existing)
  if (n_existing > 0L) {
    caller_name <- rlang::call_name(rlang::caller_call())
    cli::cli_warn(
      c(
        paste0(
          "{.fn {caller_name}} overwrites ",
          "{n_existing} existing column{?s}: {.val {existing}}."
        ),
        "i" = "Consider renaming or removing existing columns."
      ),
      call = call,
    )
  }
  invisible(data)
}
