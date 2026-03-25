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
disconnect_connection <- function(con) {
  DBI::dbDisconnect(con)
}

#' @rdname disconnect_connection
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `disconnection_connection()` was renamed to `disconnect_connection()`.
#' @export
disconnection_connection <- function(con) {
  lifecycle::deprecate_warn(
    "0.5.0",
    "disconnection_connection()",
    "disconnect_connection()"
  )
  disconnect_connection(con)
}
