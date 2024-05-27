#' Disconnect Database Connection
#'
#' This function safely disconnects an established database connection using the
#' DBI package.
#'
#' @param con A database connection object created by DBI::dbConnect or any
#'   similar function that establishes a connection to a database.
#' @return A logical value: `TRUE` if disconnection was successful, `FALSE`
#'   otherwise.
#'
#' @export
disconnection_connection <- function(con) {

  check_if_package_installed("DBI", "wrds_*")

  dbDisconnect <- getNamespace("DBI")$dbDisconnect

  dbDisconnect(con)
}
