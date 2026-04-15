#' Create Data Options
#'
#' Creates a list of data options of class `tidyfinance_data_options` used for 
#' TidyFinance-related functions. These options map the specific data variables
#' to the TidyFinance naming conventions, allowing functions to flexibly work 
#' with different datasets by specifying the relevant column names. The 
#' function accepts key parameters such as `id`, `date`, `exchange`, 
#' `mktcap_lag`, `ret_excess`, `portfolio`, along with other additional options
#' passed through `...`.
#'
#' @param id A character string representing the identifier variable
#'  (defaults to "permno").
#' @param date A character string representing the date variable (defaults to 
#'  "date").
#' @param exchange A character string representing the exchange variable
#'  (defaults to "exchange").
#' @param mktcap_lag A character string representing the market
#'  capitalization lag variable (defaults to "mktcap_lag").
#' @param ret_excess A character string representing the excess return
#'  variable (defaults to "ret_excess").
#' @param portfolio A character string representing the portfolio
#'  variable (defaults to "portfolio").
#' @param ... Additional arguments to be included in the data options
#'  list.
#'
#' @returns A list of class `tidyfinance_data_options` containing the
#'  specified data options.
#'
#' @family portfolio functions
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
  if (!is.character(id) || length(id) != 1) {
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
  if (!is.character(ret_excess) || length(ret_excess) != 1) {
    cli::cli_abort("{.arg ret_excess} must be a single character")
  }

  # Error handling for portfolio
  if (!is.character(portfolio) || length(portfolio) != 1) {
    cli::cli_abort("{.arg portfolio} must be a single character")
  }

  # Create the list structure with class attribute
  structure(
    list(
      "id" = id,
      "date" = date,
      "exchange" = exchange,
      "mktcap_lag" = mktcap_lag,
      "ret_excess" = ret_excess,
      "portfolio" = portfolio,
      ...
    ),
    class = "tidyfinance_data_options"
  )
}
