#' Create Data Options
#'
#' This function creates a list of data options used in financial data analysis,
#' specifically for TidyFinance-related functions. It allows users to specify
#' key parameters such as `id`, `date`, `exchange`, `mktcap_lag`, and `ret_excess`
#' along with other additional options passed through `...`.
#'
#' @param id A character string representing the identifier variable (e.g., "permno").
#' @param date A character string representing the date variable (e.g., "date").
#' @param exchange A character string representing the exchange variable (e.g., "exchange").
#' @param mktcap_lag A character string representing the market capitalization lag variable
#'  (e.g., "mktcap_lag").
#' @param ret_excess A character string representing the excess return variable
#'  (e.g., "ret_excess").
#' @param portfolio A character string representing the portfolio variable (e.g., "portfolio").
#' @param ... Additional arguments to be included in the data options list.
#'
#' @return A list of class `tidyfinance_data_options` containing the specified data options.
#'
#' @export
#'
#' @examples
#' data_options(
#'   id = "permno",
#'   date = "date",
#'   exchange = "exchange"
#' )
#'
data_options <- function(
    id = "permno",
    date = "date",
    exchange = "exchange",
    mktcap_lag = "mktcap_lag",
    ret_excess = "ret_excess",
    portfolio = "portfolio",
    ...
) {
  # Error handling for id
  if (!is.character(id) || length(id) != 1) {  # Corrected this to check the 'id' argument, not 'date'
    cli::cli_abort("{.arg id} must be a single character.")
  }

  # Error handling for date
  if (!is.character(date) || length(date) != 1) {
    cli::cli_abort("{.arg date} must be a single character")
  }

  # Error handling for exchange
  if (!is.character(exchange) || length(exchange) != 1) {
    cli::cli_abort("{.arg exchange} must be a single character")
  }

  # Error handling for mktcap_lag
  if (!is.character(mktcap_lag) || length(mktcap_lag) != 1) {
    cli::cli_abort("{.arg mktcap_lag} must be a single character")
  }

  # Error handling for ret_excess
  if (!is.character(ret_excess) || length(mktcap_lag) != 1) {
    cli::cli_abort("{.arg ret_excess} must be a single character")
  }

  # Create the list structure with class attribute
  structure(
    list(
      "id" = id,
      "date" = date,
      "exchange" = exchange,
      "mktcap_lag" = mktcap_lag,
      "ret_excess" = ret_excess,
      ...
    ),
    class = "tidyfinance_data_options"
  )
}
