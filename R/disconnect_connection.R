#' Disconnect Database Connection
#'
#' Safely disconnects an established database connection using the DBI package.
#'
#' @param con A database connection object created by DBI::dbConnect or any
#'   similar function that establishes a connection to a database.
#'
#' @returns A logical value: `TRUE` if disconnection was successful, `FALSE`
#'   otherwise.
#'
#' @export
disconnect_connection <- function(con) {
  DBI::dbDisconnect(con)
}
