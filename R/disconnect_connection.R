#' Disconnect Database Connection
#'
#' This function safely disconnects an established database connection using the
#' DBI package.
#'
#' @param con A database connection object created by DBI::dbConnect or any
#'   similar function that establishes a connection to a database.
#'
#' @returns A logical value: `TRUE` if disconnection was successful, `FALSE`
#'   otherwise.
#'
#' @export
disconnection_connection <- function(con) {
  rlang::check_installed(
    "DBI",
    reason = "to download types wrds_*."
  )

  DBI::dbDisconnect(con)
}
