#' Establish a Connection to the WRDS Database
#'
#' This function establishes a connection to the Wharton Research Data Services
#' (WRDS) database using the `RPostgres` package. It requires that the
#' `RPostgres` package is installed and that valid WRDS credentials are set as
#' environment variables.
#'
#' @return An object of class `DBIConnection` representing the connection to the
#'   WRDS database. This object can be used with other DBI-compliant functions
#'   to interact with the database.
#'
#' @details The function checks if the `RPostgres` package is installed before
#'   attempting to establish a connection. It uses the host, dbname, port, and
#'   sslmode as fixed parameters for the connection. Users must set their WRDS
#'   username and password as environment variables `WRDS_USER` and
#'   `WRDS_PASSWORD`, respectively, before using this function.
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
#'
#' @export
#'
#' @seealso \code{\link[RPostgres]{Postgres}}, \code{\link[DBI]{dbDisconnect}} for more
#'          information on managing database connections.
get_wrds_connection <- function() {

  if (Sys.getenv("WRDS_USER") == "" || Sys.getenv("WRDS_PASSWORD") == "") {
    message("WRDS credentials not found. Please set them using set_wrds_credentials().")
  }

  check_if_package_installed("RPostgres", "wrds_*")

  dbConnect <- getNamespace("DBI")$dbConnect
  Postgres <- getNamespace("RPostgres")$Postgres

  dbConnect(
    Postgres(),
    host = "wrds-pgdata.wharton.upenn.edu",
    dbname = "wrds",
    port = 9737,
    sslmode = "require",
    user = Sys.getenv("WRDS_USER"),
    password = Sys.getenv("WRDS_PASSWORD")
  )
}
