#' Download Data from WRDS Compustat
#'
#' This function downloads financial data from the WRDS Compustat database for a
#' given type of financial data, start date, and end date. It filters the data
#' according to industry format, data format, and consolidation level, and
#' returns the most current data for each reporting period. Additionally, the
#' annual data also includes the calculated calculates book equity (be),
#' operating profitability (op), and investment (inv) for each company.
#'
#' @param type The type of financial data to download.
#' @param start_date The start date for the data retrieval in "YYYY-MM-DD" format.
#' @param end_date The end date for the data retrieval in "YYYY-MM-DD" format.
#' @param additional_columns Additional columns from the Compustat table
#'   as a character vector.
#'
#' @return A data frame with financial data for the specified period, including
#'   variables for book equity (be), operating profitability (op), investment
#'   (inv), and others.
#'
#' @examples
#' \donttest{
#'   compustat <- download_data_wrds_compustat("wrds_compustat_annual", "2020-01-01", "2020-12-31")
#'   compustat_quarterly <- download_data_wrds_compustat("wrds_compustat_quarterly", "2020-01-01", "2020-12-31")
#'
#'   # Add additional columns
#'   download_data_wrds_compustat("wrds_compustat_annual", "2020-01-01", "2020-12-31",
#'                                additional_columns = c("aodo", "aldo"))
#' }
#'
#' @import dplyr
#' @importFrom lubridate year
#'
#' @export
download_data_wrds_compustat <- function(type, start_date, end_date, additional_columns = NULL) {

  check_if_package_installed("dbplyr", type)

  in_schema <- getNamespace("dbplyr")$in_schema

  con <- get_wrds_connection()

  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)

  if (grepl("compustat_annual", type, fixed = TRUE)) {
    funda_db <- tbl(con, in_schema("comp", "funda"))

    compustat <- funda_db |>
      filter(
        indfmt == "INDL" &
          datafmt == "STD" &
          consol == "C" &
          between(datadate, start_date, end_date)
      ) |>
      select(
        gvkey, datadate, seq, ceq, at, lt, txditc,
        txdb, itcb, pstkrv, pstkl, pstk, capx, oancf,
        sale, cogs, xint, xsga,
        all_of(additional_columns)
      ) |>
      collect()

    disconnection_connection(con)

    compustat <- compustat |>
      mutate(
        be = coalesce(seq, ceq + pstk, at - lt) +
          coalesce(txditc, txdb + itcb, 0) -
          coalesce(pstkrv, pstkl, pstk, 0),
        be = if_else(be <= 0, NA, be),
        op = (sale - coalesce(cogs, 0) -
                coalesce(xsga, 0) - coalesce(xint, 0)) / be,
      )

    compustat <- compustat |>
      mutate(year = lubridate::year(datadate)) |>
      group_by(gvkey, year) |>
      filter(datadate == max(datadate)) |>
      ungroup()

    processed_data <- compustat |>
      left_join(
        compustat |>
          select(gvkey, year, at_lag = at) |>
          mutate(year = year + 1),
        join_by(gvkey, year)
      ) |>
      mutate(
        inv = at / at_lag - 1,
        inv = if_else(at_lag <= 0, NA, inv)
      )
  }

  if (grepl("compustat_quarterly", type, fixed = TRUE)) {
    fundq_db <- tbl(con, in_schema("comp", "fundq"))

    compustat <- fundq_db |>
      filter(
        indfmt == "INDL" &
          datafmt == "STD" &
          consol == "C" &
          between(datadate, start_date, end_date)
      ) |>
      select(
        gvkey, datadate, rdq, fqtr, fyearq,
        all_of(additional_columns)
      ) |>
      collect()

    disconnection_connection(con)

    compustat <- compustat |>
      drop_na(fqtr)|>
      mutate(date = ceiling_date(datadate, "quarter") %m-% months(1),
             timepoint = paste0(fyearq, fqtr)) |>
      group_by(gvkey, timepoint) |>
      filter(datadate == max(datadate)) |>
      ungroup() |>
      filter(if_else(is.na(rdq), TRUE, date < rdq)) |>
      arrange(gvkey, datadate) |>
      add_count(gvkey, date) |>
      filter(n == 1) |>
      select(-n)

    processed_data <- compustat |>
      select(gvkey, datadate, date,
             all_of(additional_columns))

  }

  processed_data
}
