#' Calculate Adjusted Prices from CRSP Alternative Price Data
#'
#' Computes adjusted prices using the CRSP alternative price (`altprc`) and the
#' cumulative adjustment factor for prices (`cfacpr`). The alternative price is
#' typically the average of bid and ask when the closing price is not available.
#'
#' @param data A data frame containing CRSP stock price data with the following
#'   columns:
#'   \describe{
#'     \item{`altprc`}{Alternative price (negative bid/ask average or closing price)}
#'     \item{`cfacpr`}{Cumulative adjustment factor for prices}
#'   }
#'
#' @returns The input data frame with an additional column:
#'   \describe{
#'     \item{`prc_adj`}{Adjusted price, calculated as |altprc| / cfacpr}
#'   }
#'   Zero values in `altprc` are treated as missing, and infinite values in
#'   `prc_adj` are converted to `NA`.
#'
#' @export
#'
#' @examples
#' library(tidyfinance)
#'
#' # Example with CRSP-like data
#' crsp_data <- data.frame(
#'   permno = c(10001, 10001, 10001),
#'   date = as.Date(c("2020-01-01", "2020-01-02", "2020-01-03")),
#'   altprc = c(-25.50, 30.00, 0),  # Negative when bid/ask average
#'   cfacpr = c(1.0, 1.0, 1.0)
#' )
#'
#' crsp_data |> crsp_adjust_prices()

crsp_adjust_prices <- function(data) {
  data |>
    mutate(
      prc_adj = abs(na_if(altprc, 0)) / cfacpr,
      prc_adj = if_else(is.infinite(prc_adj), NA_real_, prc_adj)
    )
}

#' Adjust NASDAQ Trading Volume (Gao & Ritter 2010)
#'
#' Applies the Gao and Ritter (2010) adjustment to NASDAQ trading volume data
#' to correct for double-counting of trades. NASDAQ historically reported
#' trading volume differently than other exchanges, requiring adjustment for
#' accurate cross-sectional comparisons. This function also calculates adjusted
#' prices.
#'
#' @details
#' The adjustment divides NASDAQ trading volume (`exchcd == 3`) by different
#' factors depending on the time period:
#' \itemize{
#'   \item Before 2001-02-01: divide by 2.0
#'   \item 2001-02-01 to 2001-12-31: divide by 1.8
#'   \item 2002-01-01 to 2003-12-31: divide by 1.6
#'   \item 2004-01-01 onwards: divide by 1.0 (no adjustment)
#' }
#' Non-NASDAQ stocks retain their original volume.
#'
#' @param data A data frame containing CRSP stock data with the following
#'   columns:
#'   \describe{
#'     \item{`vol`}{Trading volume (shares traded)}
#'     \item{`prc`}{Closing price}
#'     \item{`cfacpr`}{Cumulative adjustment factor for prices}
#'     \item{`exchcd`}{Exchange code (3 = NASDAQ)}
#'     \item{`date`}{Trading date}
#'   }
#'
#' @returns The input data frame with the following modifications and additions:
#'   \describe{
#'     \item{`vol`}{Volume with -99 values converted to `NA`}
#'     \item{`prc`}{Price with 0 values converted to `NA`}
#'     \item{`prc_adj`}{Adjusted price, calculated as |prc| / cfacpr}
#'     \item{`vol_adj`}{Adjusted volume based on exchange and time period}
#'   }
#'
#' @references
#' Gao, X., & Ritter, J. R. (2010). The marketing of seasoned equity offerings.
#' *Journal of Financial Economics*, 97(1), 33–52.
#' \doi{10.1016/j.jfineco.2010.03.007}
#'
#' @export
#'
#' @examples
#' library(tidyfinance)
#'
#' # Example with CRSP-like data including NASDAQ and NYSE stocks
#' crsp_daily <- data.frame(
#'   permno = c(10001, 10002, 10001, 10002),
#'   date = as.Date(c("2000-06-15", "2000-06-15", "2003-06-15", "2003-06-15")),
#'   exchcd = c(3, 1, 3, 1),  # 3 = NASDAQ, 1 = NYSE
#'   prc = c(25.50, 30.00, 40.25, 35.75),
#'   vol = c(1000000, 500000, 2000000, 750000),
#'   cfacpr = c(1.0, 1.0, 1.0, 1.0)
#' )
#'
#' crsp_daily |> volume_adjustment_gao_ritter()

volume_adjustment_gao_ritter <- function(data) {
  gr_date_1 <- as.Date("2001-02-01")
  gr_date_2 <- as.Date("2002-01-01")
  gr_date_3 <- as.Date("2004-01-01")

  data |>
    mutate(
      vol = na_if(vol, -99),
      prc = na_if(prc, 0),
      prc_adj = abs(prc) / cfacpr,
      prc_adj = if_else(is.infinite(prc_adj), NA_real_, prc_adj)
    ) |>
    mutate(
      vol_adj = case_when(
        exchcd == 3 & date < gr_date_1 ~ vol / 2.0,
        exchcd == 3 & date >= gr_date_1 & date < gr_date_2 ~ vol / 1.8,
        exchcd == 3 & date >= gr_date_2 & date < gr_date_3 ~ vol / 1.6,
        exchcd == 3 & date >= gr_date_3 ~ vol / 1.0,
        .default = vol
      )
    )
}


#' Add CCM Links to CRSP Data
#'
#' Merges CRSP stock data with CRSP-Compustat Merged (CCM) linking table to
#' add Compustat identifiers (`gvkey`) to CRSP data. The function ensures that
#' links are only added when they are valid based on the linking date range
#' specified in the CCM link table.
#'
#' @details
#' The CCM link table connects CRSP's stock identifier (`permno`) to Compustat's
#' company identifier (`gvkey`). This function filters the links to ensure that
#' only valid links are used, where the CRSP date falls within the link's valid
#' date range (`linkdt` to `linkenddt`).
#'
#' @param crsp_data A data frame containing CRSP stock data (monthly or daily)
#'   with at least the following columns:
#'   \describe{
#'     \item{`permno`}{CRSP permanent security identifier}
#'     \item{`date`}{Observation date}
#'   }
#' @param ccm_link A data frame containing the CCM linking table with the
#'   following columns:
#'   \describe{
#'     \item{`permno`}{CRSP permanent security identifier}
#'     \item{`gvkey`}{Compustat global company key}
#'     \item{`linkdt`}{First effective date of the link}
#'     \item{`linkenddt`}{Last effective date of the link}
#'   }
#'
#' @returns The CRSP data frame with an additional `gvkey` column containing
#'   the Compustat identifier for observations where a valid link exists. Rows
#'   without a valid link will have `NA` for `gvkey`.
#'
#' @export
#'
#' @examples
#' library(tidyfinance)
#'
#' # Example CRSP monthly data
#' crsp_monthly <- data.frame(
#'   permno = c(10001, 10001, 10002, 10002),
#'   date = as.Date(c("2020-01-31", "2020-02-29", "2020-01-31", "2020-02-29")),
#'   ret = c(0.05, -0.02, 0.03, 0.01)
#' )
#'
#' # Example CCM link table
#' ccm_link <- data.frame(
#'   permno = c(10001, 10002),
#'   gvkey = c("001234", "005678"),
#'   linkdt = as.Date(c("2010-01-01", "2015-01-01")),
#'   linkenddt = as.Date(c("2025-12-31", "2025-12-31"))
#' )
#'
#' # Add CCM links to CRSP data
#' crsp_monthly |> add_ccm_link_to_crsp(ccm_link)
add_ccm_link_to_crsp <- function(crsp_data, ccm_link) {
  valid_links <- crsp_data |>
    inner_join(
      ccm_link,
      join_by(permno),
      relationship = "many-to-many",
      multiple = "all"
    ) |>
    filter(!is.na(gvkey) & (date >= linkdt & date <= linkenddt)) |>
    select(permno, gvkey, date)

  crsp_data |>
    left_join(valid_links, join_by(permno, date))
}
