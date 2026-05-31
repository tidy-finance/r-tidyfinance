#' Create Data Options
#'
#' Creates a list of data options of class `tidyfinance_data_options` used for
#' TidyFinance-related functions. These options map the specific data variables
#' to the TidyFinance naming conventions, allowing functions to flexibly work
#' with different datasets by specifying the relevant column names. Additional
#' options can be passed through `...`.
#'
#' @param id A character string representing the entity variable (defaults to
#'   `"permno"`).
#' @param date A character string representing the date variable (defaults to
#'   `"date"`).
#' @param exchange A character string representing the exchange variable
#'   (defaults to `"exchange"`).
#' @param mktcap_lag A character string representing the market capitalization
#'   lag variable (defaults to `"mktcap_lag"`).
#' @param ret_excess A character string representing the excess return variable
#'   (defaults to `"ret_excess"`).
#' @param portfolio A character string representing the portfolio variable
#'   (defaults to `"portfolio"`).
#' @param siccd A character string representing the Standard Industrial
#'   Classification code variable (defaults to `"siccd"`).
#' @param price A character string representing the (adjusted) price variable
#'   (defaults to `"prc_adj"`).
#' @param listing_age A character string representing the listing age variable
#'   (defaults to `"listing_age"`).
#' @param be A character string representing the book equity variable (defaults
#'   to `"be"`).
#' @param earnings A character string representing the earnings variable
#'   (defaults to `"ib"`, the Compustat income before extraordinary items
#'   column).
#' @param ... Additional arguments to be included in the data options list.
#'
#' @returns A list of class `tidyfinance_data_options` containing the specified
#'   data options.
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
  siccd = "siccd",
  price = "prc_adj",
  listing_age = "listing_age",
  be = "be",
  earnings = "ib",
  ...
) {
  validate_column_name(id, "id", "entity")
  validate_column_name(date, "date", "date")
  validate_column_name(exchange, "exchange", "exchange")
  validate_column_name(mktcap_lag, "mktcap_lag", "market capitalization lag")
  validate_column_name(ret_excess, "ret_excess", "excess return")
  validate_column_name(portfolio, "portfolio", "portfolio")
  validate_column_name(
    siccd,
    "siccd",
    "Standard Industrial Classification code"
  )
  validate_column_name(price, "price", "(adjusted) price")
  validate_column_name(listing_age, "listing_age", "listing age")
  validate_column_name(be, "be", "book equity")
  validate_column_name(earnings, "earnings", "earnings")

  # Create the list structure with class attribute
  structure(
    list(
      "id" = id,
      "date" = date,
      "exchange" = exchange,
      "mktcap_lag" = mktcap_lag,
      "ret_excess" = ret_excess,
      "portfolio" = portfolio,
      "siccd" = siccd,
      "price" = price,
      "listing_age" = listing_age,
      "be" = be,
      "earnings" = earnings,
      ...
    ),
    class = "tidyfinance_data_options"
  )
}
