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
  # Error handling for id
  if (!is.character(id) || length(id) != 1) {
    cli::cli_abort(
      paste0(
        "{.arg id} must be a string indicating the column name ",
        "for the entity variable."
      )
    )
  }

  # Error handling for date
  if (!is.character(date) || length(date) != 1) {
    cli::cli_abort(
      paste0(
        "{.arg date} must be a string indicating the column name ",
        "for the date variable."
      )
    )
  }

  # Error handling for exchange
  if (!is.character(exchange) || length(exchange) != 1) {
    cli::cli_abort(
      paste0(
        "{.arg exchange} must be a string indicating the column name ",
        "for the exchange variable."
      )
    )
  }

  # Error handling for mktcap_lag
  if (!is.character(mktcap_lag) || length(mktcap_lag) != 1) {
    cli::cli_abort(
      paste0(
        "{.arg mktcap_lag} must be a string indicating the column name ",
        "for the market capitalization lag variable."
      )
    )
  }

  # Error handling for ret_excess
  if (!is.character(ret_excess) || length(ret_excess) != 1) {
    cli::cli_abort(
      paste0(
        "{.arg ret_excess} must be a string indicating the column name ",
        "for the excess return variable."
      )
    )
  }

  # Error handling for portfolio
  if (!is.character(portfolio) || length(portfolio) != 1) {
    cli::cli_abort(
      paste0(
        "{.arg portfolio} must be a string indicating the column name ",
        "for the portfolio variable."
      )
    )
  }

  # Error handling for siccd
  if (!is.character(siccd) || length(siccd) != 1) {
    cli::cli_abort(
      paste0(
        "{.arg siccd} must be a string indicating the column name for ",
        "the Standard Industrial Classification code variable."
      )
    )
  }

  # Error handling for price
  if (!is.character(price) || length(price) != 1) {
    cli::cli_abort(
      paste0(
        "{.arg price} must be a string indicating the column name ",
        "for the (adjusted) price variable."
      )
    )
  }

  # Error handling for listing_age
  if (!is.character(listing_age) || length(listing_age) != 1) {
    cli::cli_abort(
      paste0(
        "{.arg listing_age} must be a string indicating the column name ",
        "for the listing age variable."
      )
    )
  }

  # Error handling for be
  if (!is.character(be) || length(be) != 1) {
    cli::cli_abort(
      paste0(
        "{.arg be} must be a string indicating the column name ",
        "for the book equity variable."
      )
    )
  }

  # Error handling for earnings
  if (!is.character(earnings) || length(earnings) != 1) {
    cli::cli_abort("{.arg earnings} must be a single character")
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
