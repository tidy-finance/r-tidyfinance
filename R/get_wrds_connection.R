#' Establish a Connection to the WRDS Database
#'
#' This function establishes a connection to the Wharton Research Data Services
#' (WRDS) database using the `RPostgres` package. It requires that the
#' `RPostgres` package is installed and that valid WRDS credentials are set as
#' environment variables.
#'
#' @details The function checks if the `RPostgres` package is installed before
#'   attempting to establish a connection. It uses the host, dbname, port, and
#'   sslmode as fixed parameters for the connection. Users must set their WRDS
#'   username and password as environment variables `WRDS_USER` and
#'   `WRDS_PASSWORD`, respectively, before using this function.
#'
#' @returns An object of class `DBIConnection` representing the connection to the
#'   WRDS database. This object can be used with other DBI-compliant functions
#'   to interact with the database.
#'
#' @seealso \code{\link[RPostgres]{Postgres}}, \code{\link[DBI]{dbDisconnect}} for more
#'          information on managing database connections.
#' @export
#'
#' @examples
#' \donttest{
#'   # Before using this function, set your WRDS credentials:
#'   # Sys.setenv(WRDS_USER = "your_username", WRDS_PASSWORD = "your_password")
#'
#'   con <- get_wrds_connection()
#'   # Use `con` with DBI-compliant functions to interact with the WRDS database
#'   # Remember to disconnect after use:
#'   # disconnect_connection(con)
#' }
get_wrds_connection <- function() {
  if (
    !nzchar(Sys.getenv("WRDS_USER")) || !nzchar(Sys.getenv("WRDS_PASSWORD"))
  ) {
    cli::cli_inform(
      "WRDS credentials not found. Please set them using {.fn set_wrds_credentials}."
    )
  }

  rlang::check_installed(
    "RPostgres",
    reason = "to download types wrds_*."
  )

  DBI::dbConnect(
    RPostgres::Postgres(),
    host = "wrds-pgdata.wharton.upenn.edu",
    dbname = "wrds",
    port = 9737,
    sslmode = "require",
    user = Sys.getenv("WRDS_USER"),
    password = Sys.getenv("WRDS_PASSWORD")
  )
}
