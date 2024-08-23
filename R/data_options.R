#' Create Data Options
#'
#' This function generates a structured list of options for defining column names which contain required information.
#' Examples include dates or firm identifiers along with additional optional parameters.
#'
#' @param date Integer, optional. The number of portfolios to create. Must be a positive integer.
#' If not provided, defaults to \code{NULL}.
#' @param id Numeric vector, optional. A vector of percentile thresholds for defining breakpoints.
#' Each value should be between 0 and 1. If not provided, defaults to \code{NULL}.
#' @param ... Additional optional arguments. These will be captured in the resulting structure as a list.
#'
#' @return A list of class \code{"tidyfinance_data_options"} containing the provided column names,
#' including any additional arguments passed via \code{...}.
#'
#' @export
#'
#' @examples
#' # Example usage
#' options <- data_options(
#'   id = "permno",
#'   date = "date",
#'   exchange = "exchange"
#' )
#' print(options)
#'
data_options <- function(
    date = "date",
    id = "permno",
    ...
) {
  # Error handling for date
  if (!is.character(date) || length(date) != 1) {
    cli::cli_abort("{.arg date} must be a single character")
  }

  # Error handling for id
  if (!is.character(id) || length(id) != 1) {  # Corrected this to check the 'id' argument, not 'date'
    cli::cli_abort("{.arg id} must be a single character.")
  }

  # Create the list structure with class attribute
  structure(
    list(
      "date" = date,
      "id" = id,
      ...
    ),
    class = "tidyfinance_data_options"
  )
}
