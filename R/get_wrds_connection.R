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
#' If the connection fails, the error message lists these steps. Unless
#' `quiet = TRUE`, the function says when it starts connecting and when the
#' connection succeeds, so you know when to approve the Duo request and when
#' the download starts.
#'
#' @param quiet A logical indicating whether to suppress the messages about
#'   the WRDS connection and the download progress. Defaults to `FALSE`.
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
get_wrds_connection <- function(quiet = FALSE) {
  if (!is.logical(quiet) || length(quiet) != 1 || is.na(quiet)) {
    cli::cli_abort("{.arg quiet} must be a single logical.")
  }

  wrds_user <- Sys.getenv("WRDS_USER")

  if (!quiet && (!nzchar(wrds_user) || !nzchar(Sys.getenv("WRDS_PASSWORD")))) {
    cli::cli_inform(
      paste(
        "WRDS credentials not found. Please set them using",
        "{.fn set_wrds_credentials}."
      )
    )
  }

  if (!quiet) {
    cli::cli_progress_step(
      # The username is deliberately not shown, as messages can end up in
      # public logs (e.g., CI or rendered documents).
      "Connecting to WRDS. Approve the Duo request if prompted.",
      msg_done = "Connected to WRDS.",
      msg_failed = "Connection to WRDS failed."
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
        parent = redact_wrds_credentials(e)
      )
    }
  )
}

#' Remove WRDS credentials from a connection error
#'
#' The error of a failed connection can contain the username (e.g.,
#' "password authentication failed for user ..."). Error messages can end up
#' in public logs, so the username and password are replaced by placeholders.
#'
#' @param e The condition raised by [DBI::dbConnect()].
#'
#' @returns The condition with a redacted message.
#' @noRd
redact_wrds_credentials <- function(e) {
  message <- conditionMessage(e)
  credentials <- c(
    "<WRDS_PASSWORD>" = Sys.getenv("WRDS_PASSWORD"),
    "<WRDS_USER>" = Sys.getenv("WRDS_USER")
  )
  for (placeholder in names(credentials)) {
    value <- credentials[[placeholder]]
    if (nzchar(value)) {
      message <- gsub(value, placeholder, message, fixed = TRUE)
    }
  }
  e$message <- message
  e
}

#' Report the progress of a WRDS download
#'
#' Shows a progress step that ends, with the elapsed time, when the calling
#' download function returns.
#'
#' @param what A string naming the data, e.g., `"CRSP monthly data"`.
#' @param quiet A logical; if `TRUE`, nothing is shown.
#' @param .envir The environment whose exit ends the step.
#'
#' @returns Invisibly, the id of the progress step, or `NULL` if `quiet`.
#' @noRd
wrds_download_step <- function(what, quiet, .envir = parent.frame()) {
  if (quiet) {
    return(invisible(NULL))
  }
  invisible(cli::cli_progress_step(
    paste("Downloading", what, "from WRDS. This can take a few minutes."),
    msg_done = paste("Downloaded", what, "from WRDS."),
    msg_failed = paste("Downloading", what, "from WRDS failed."),
    .envir = .envir
  ))
}
