#' Establish a Connection to the WRDS Database
#'
#' Establishes a connection to the Wharton Research Data Services (WRDS)
#' database using the `RPostgres` package. It requires that the `RPostgres`
#' package be installed and that valid WRDS credentials be set as environment
#' variables.
#'
#' @details The function checks if the `RPostgres` package is installed
#'   before attempting to establish a connection. It uses the host, dbname,
#'   port, and sslmode as fixed parameters for the connection. Users must
#'   set their WRDS username and password as environment variables
#'   `WRDS_USER` and `WRDS_PASSWORD`, respectively, before using this
#'   function.
#'
#' @section WRDS account setup:
#' A WRDS username and password alone are not enough to connect. Before the
#' first connection:
#' \enumerate{
#'   \item Get a WRDS account through an institution that subscribes to the
#'     data you need (e.g., CRSP and Compustat).
#'   \item Enroll in two-factor authentication (Duo) for your WRDS account.
#'     WRDS requires it for all connections, including connections from R.
#'   \item Log in to the WRDS website (<https://wrds-www.wharton.upenn.edu>)
#'     once and accept the terms of use, if WRDS asks you to.
#'   \item Store your username and password with [set_wrds_credentials()],
#'     or set the `WRDS_USER` and `WRDS_PASSWORD` environment variables, and
#'     restart R.
#'   \item When you connect, approve the Duo push notification on your
#'     registered device. The connection waits until you approve it.
#' }
#' If the connection fails, the error message lists these steps.
#'
#' @returns An object of class `DBIConnection` representing the connection
#'   to the WRDS database. This object can be used with other DBI-compliant
#'   functions to interact with the database.
#'
#' @seealso [RPostgres::Postgres()], [DBI::dbDisconnect()] for more
#'   information on managing database connections.
#'
#' @family WRDS functions
#' @export
#'
#' @examples
#' \dontrun{
#'   # Before using this function, set your WRDS credentials:
#'   # Sys.setenv(WRDS_USER = "your_username", WRDS_PASSWORD = "your_password")
#'
#'   # con <- get_wrds_connection()
#'   # Use `con` with DBI-compliant functions to interact with the WRDS database
#'   # Remember to disconnect after use:
#'   # disconnect_connection(con)
#' }
get_wrds_connection <- function() {
  if (
    !nzchar(Sys.getenv("WRDS_USER")) || !nzchar(Sys.getenv("WRDS_PASSWORD"))
  ) {
    cli::cli_inform(
      paste(
        "WRDS credentials not found. Please set them using",
        "{.fn set_wrds_credentials}."
      )
    )
  }

  tryCatch(
    DBI::dbConnect(
      RPostgres::Postgres(),
      host = "wrds-pgdata.wharton.upenn.edu",
      dbname = "wrds",
      port = 9737,
      sslmode = "require",
      user = Sys.getenv("WRDS_USER"),
      password = Sys.getenv("WRDS_PASSWORD")
    ),
    error = function(e) {
      cli::cli_abort(
        c(
          "Could not connect to WRDS.",
          "i" = paste(
            "Check that {.envvar WRDS_USER} and {.envvar WRDS_PASSWORD} are",
            "correct, e.g., with {.fn set_wrds_credentials}."
          ),
          "i" = paste(
            "WRDS requires two-factor authentication (Duo): enroll your",
            "account and approve the push notification when you connect."
          ),
          "i" = paste(
            "Log in to {.url https://wrds-www.wharton.upenn.edu} once and",
            "accept the terms of use if WRDS asks you to."
          ),
          "i" = "See {.topic tidyfinance::get_wrds_connection} for details."
        ),
        parent = e
      )
    }
  )
}
