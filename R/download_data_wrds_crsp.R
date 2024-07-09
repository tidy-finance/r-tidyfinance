#' Download Data from WRDS CRSP
#'
#' This function downloads and processes stock return data from the CRSP
#' database for a specified period. Users can choose between monthly and daily
#' data types. The function also adjusts returns for delisting and calculates
#' market capitalization and excess returns over the risk-free rate.
#'
#' @param type A string specifying the type of CRSP data to download:
#'   "crsp_monthly" or "crsp_daily".
#' @param start_date The start date for the data retrieval in "YYYY-MM-DD" format.
#' @param end_date The end date for the data retrieval in "YYYY-MM-DD" format.
#' @param batch_size An optional integer specifying the batch size for
#'   processing daily data, with a default of 500.
#' @param version An optional character specifying which CRSP version to use.
#'   "v2" (the default) uses the updated second version of CRSP, and "v1"
#'   downloads the legacy version of CRSP.
#' @param additional_columns Additional columns from the CRSP monthly or
#'   daily data as a character vector.
#'
#' @return A data frame containing CRSP stock returns, adjusted for delistings,
#'   along with calculated market capitalization and excess returns over the
#'   risk-free rate. The structure of the returned data frame depends on the
#'   selected data type.
#'
#' @examples
#' \donttest{
#'   crsp_monthly <- download_data_wrds_crsp("wrds_crsp_monthly", "2020-11-01", "2020-12-31")
#'   crsp_daily <- download_data_wrds_crsp("wrds_crsp_daily", "2020-12-01", "2020-12-31")
#'
#'   # Add additional columns
#'   download_data_wrds_crsp("wrds_crsp_monthly", "2020-11-01", "2020-12-31",
#'                           additional_columns = c("mthvol", "mthvolflg"))
#' }
#'
#' @import dplyr
#' @import tidyr
#' @import lubridate
#'
#' @export
download_data_wrds_crsp <- function(type, start_date, end_date, batch_size = 500, version = "v2", additional_columns = NULL) {

  if (!(version %in% c("v1", "v2"))) stop("Parameter version must be equal to v1 or v2.")

  check_if_package_installed("dbplyr", type)
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)

  in_schema <- getNamespace("dbplyr")$in_schema

  con <- get_wrds_connection()

  if (grepl("crsp_monthly", type, fixed = TRUE)) {

    if (version == "v1") {

      msf_db <- tbl(con, in_schema("crsp", "msf"))
      msenames_db <- tbl(con, in_schema("crsp", "msenames"))
      msedelist_db <- tbl(con, in_schema("crsp", "msedelist"))

      crsp_monthly <- msf_db |>
        filter(between(date, start_date, end_date)) |>
        inner_join(
          msenames_db |>
            filter(shrcd %in% c(10, 11)) |>
            select(permno, exchcd, siccd, namedt, nameendt),
          join_by(permno)
        ) |>
        filter(between(date, namedt, nameendt)) |>
        mutate(month = floor_date(date, "month")) |>
        left_join(
          msedelist_db |>
            select(permno, dlstdt, dlret, dlstcd) |>
            mutate(month = floor_date(dlstdt, "month")),
          join_by(permno, month)
        ) |>
        select(
          permno, date, month, ret, shrout, altprc,
          exchcd, siccd, dlret, dlstcd,
          additional_columns
        ) |>
        collect() |>
        mutate(
          month = ymd(month),
          shrout = shrout * 1000
        )

      disconnection_connection(con)

      crsp_monthly <- crsp_monthly |>
        mutate(
          mktcap = abs(shrout * altprc) / 10^6,
          mktcap = na_if(mktcap, 0)
        )

      mktcap_lag <- crsp_monthly |>
        mutate(month = month %m+% months(1)) |>
        select(permno, month, mktcap_lag = mktcap)

      crsp_monthly <- crsp_monthly |>
        left_join(mktcap_lag, join_by(permno, month))

      crsp_monthly <- crsp_monthly |>
        mutate(exchange = case_when(
          exchcd %in% c(1, 31) ~ "NYSE",
          exchcd %in% c(2, 32) ~ "AMEX",
          exchcd %in% c(3, 33) ~ "NASDAQ",
          .default = "Other"
        ))

      crsp_monthly <- crsp_monthly |>
        mutate(industry = case_when(
          siccd >= 1 & siccd <= 999 ~ "Agriculture",
          siccd >= 1000 & siccd <= 1499 ~ "Mining",
          siccd >= 1500 & siccd <= 1799 ~ "Construction",
          siccd >= 2000 & siccd <= 3999 ~ "Manufacturing",
          siccd >= 4000 & siccd <= 4899 ~ "Transportation",
          siccd >= 4900 & siccd <= 4999 ~ "Utilities",
          siccd >= 5000 & siccd <= 5199 ~ "Wholesale",
          siccd >= 5200 & siccd <= 5999 ~ "Retail",
          siccd >= 6000 & siccd <= 6799 ~ "Finance",
          siccd >= 7000 & siccd <= 8999 ~ "Services",
          siccd >= 9000 & siccd <= 9999 ~ "Public",
          .default = "Missing"
        ))

      crsp_monthly <- crsp_monthly |>
        mutate(ret_adj = case_when(
          is.na(dlstcd) ~ ret,
          !is.na(dlstcd) & !is.na(dlret) ~ dlret,
          dlstcd %in% c(500, 520, 580, 584) |
            (dlstcd >= 551 & dlstcd <= 574) ~ -0.30,
          dlstcd == 100 ~ ret,
          .default = -1
        )) |>
        select(-c(dlret, dlstcd))

      factors_ff3_monthly <- download_data_factors_ff(
        "factors_ff3_monthly", start_date, end_date
      ) |>
        rename(month = date)

      crsp_monthly <- crsp_monthly |>
        left_join(factors_ff3_monthly,
                  join_by(month)
        ) |>
        mutate(
          ret_excess = ret_adj - risk_free,
          ret_excess = pmax(ret_excess, -1)
        ) |>
        select(-risk_free, -mkt_excess, -hml, -smb)

      processed_data <- crsp_monthly |>
        drop_na(ret_excess, mktcap, mktcap_lag)

    } else {

      msf_db <- tbl(con, in_schema("crsp", "msf_v2"))
      stksecurityinfohist_db <- tbl(con, in_schema("crsp", "stksecurityinfohist"))

      crsp_monthly <- msf_db |>
        filter(between(mthcaldt, start_date, end_date)) |>
        select(-c(siccd, primaryexch, conditionaltype, tradingstatusflg)) |>
        inner_join(
          stksecurityinfohist_db |>
            filter(sharetype == "NS" &
                     securitytype == "EQTY" &
                     securitysubtype == "COM" &
                     usincflg == "Y" &
                     issuertype %in% c("ACOR", "CORP") &
                     primaryexch %in% c("N", "A", "Q") &
                     conditionaltype %in% c("RW", "NW") &
                     tradingstatusflg == "A") |>
            select(permno, secinfostartdt, secinfoenddt,
                   primaryexch, siccd),
          join_by(permno)
        )  |>
        filter(between(mthcaldt, secinfostartdt, secinfoenddt)) |>
        mutate(month = floor_date(mthcaldt, "month")) |>
        select(
          permno,
          date = mthcaldt,
          month,
          ret = mthret,
          shrout,
          prc = mthprc,
          primaryexch,
          siccd,
          additional_columns
        ) |>
        collect() |>
        mutate(
          month = ymd(month),
          shrout = shrout * 1000
        )

      disconnection_connection(con)

      crsp_monthly <- crsp_monthly |>
        mutate(
          mktcap = shrout * prc / 10^6,
          mktcap = na_if(mktcap, 0)
        )

      mktcap_lag <- crsp_monthly |>
        mutate(month = month %m+% months(1)) |>
        select(permno, month, mktcap_lag = mktcap)

      crsp_monthly <- crsp_monthly |>
        left_join(mktcap_lag, join_by(permno, month))

      crsp_monthly <- crsp_monthly |>
        mutate(exchange = case_when(
          primaryexch == "N" ~ "NYSE",
          primaryexch == "A" ~ "AMEX",
          primaryexch == "Q" ~ "NASDAQ",
          .default = "Other"
        ))

      crsp_monthly <- crsp_monthly |>
        mutate(industry = case_when(
          siccd >= 1 & siccd <= 999 ~ "Agriculture",
          siccd >= 1000 & siccd <= 1499 ~ "Mining",
          siccd >= 1500 & siccd <= 1799 ~ "Construction",
          siccd >= 2000 & siccd <= 3999 ~ "Manufacturing",
          siccd >= 4000 & siccd <= 4899 ~ "Transportation",
          siccd >= 4900 & siccd <= 4999 ~ "Utilities",
          siccd >= 5000 & siccd <= 5199 ~ "Wholesale",
          siccd >= 5200 & siccd <= 5999 ~ "Retail",
          siccd >= 6000 & siccd <= 6799 ~ "Finance",
          siccd >= 7000 & siccd <= 8999 ~ "Services",
          siccd >= 9000 & siccd <= 9999 ~ "Public",
          .default = "Missing"
        ))

      factors_ff3_monthly <- download_data_factors_ff(
        "factors_ff3_monthly", start_date, end_date
      ) |>
        rename(month = date)

      crsp_monthly <- crsp_monthly |>
        left_join(factors_ff3_monthly,
                  join_by(month)
        ) |>
        mutate(
          ret_excess = ret - risk_free,
          ret_excess = pmax(ret_excess, -1)
        ) |>
        select(-risk_free, -mkt_excess, -hml, -smb)

      processed_data <- crsp_monthly |>
        drop_na(ret_excess, mktcap, mktcap_lag)
    }
  }

  if (grepl("crsp_daily", type, fixed = TRUE)) {

    if (version == "v1") {

      dsf_db <- tbl(con, in_schema("crsp", "dsf")) |>
        filter(between(date, start_date, end_date))
      msenames_db <- tbl(con, in_schema("crsp", "msenames"))
      msedelist_db <- tbl(con, in_schema("crsp", "msedelist"))

      permnos <- dsf_db |>
        distinct(permno) |>
        pull()

      factors_ff3_daily <- download_data_factors_ff(
        "factors_ff3_daily", start_date, end_date
      )

      batches <- ceiling(length(permnos) / batch_size)

      crsp_daily_list <- list()

      for (j in 1:batches) {

        permno_batch <- permnos[
          ((j - 1) * batch_size + 1):min(j * batch_size, length(permnos))
        ]

        crsp_daily_sub <- dsf_db |>
          filter(permno %in% permno_batch) |>
          inner_join(
            msenames_db |>
              filter(shrcd %in% c(10, 11)),
            join_by(permno)
          ) |>
          filter(between(date, namedt, nameendt)) |>
          select(permno, date, ret, additional_columns) |>
          collect() |>
          drop_na()

        if (nrow(crsp_daily_sub) > 0) {

          msedelist_sub <- msedelist_db |>
            filter(permno %in% permno_batch) |>
            select(permno, dlstdt, dlret) |>
            collect() |>
            drop_na()

          crsp_daily_sub <- crsp_daily_sub |>
            left_join(msedelist_sub, join_by(permno, date == dlstdt)) |>
            bind_rows(msedelist_sub |>
                        anti_join(crsp_daily_sub,
                                  join_by(permno, dlstdt == date))) |>
            mutate(ret = if_else(!is.na(dlret), dlret, ret),
                   date = if_else(!is.na(dlstdt), dlstdt, date)) |>
            select(-c(dlret, dlstdt)) |>
            left_join(msedelist_sub |>
                        select(permno, dlstdt), join_by(permno)) |>
            mutate(dlstdt = replace_na(dlstdt, as.Date(end_date))) |>
            filter(date <= dlstdt) |>
            select(-dlstdt)

          crsp_daily_list[[j]] <- crsp_daily_sub |>
            mutate(month = floor_date(date, "month")) |>
            left_join(factors_ff3_daily |>
                        select(date, risk_free), join_by(date)) |>
            mutate(
              ret_excess = ret - risk_free,
              ret_excess = pmax(ret_excess, -1)
            ) |>
            select(-risk_free)
        }
      }

      disconnection_connection(con)

      processed_data <- bind_rows(crsp_daily_list)

    } else {

      dsf_db <- tbl(con, in_schema("crsp", "dsf_v2"))
      stksecurityinfohist_db <- tbl(con, in_schema("crsp", "stksecurityinfohist"))

      permnos <- dsf_db |>
        distinct(permno) |>
        pull()

      factors_ff3_daily <- download_data_factors_ff(
        "factors_ff3_daily", start_date, end_date
      )

      batches <- ceiling(length(permnos) / batch_size)

      crsp_daily_list <- list()

      for (j in 1:batches) {

        permno_batch <- permnos[
          ((j - 1) * batch_size + 1):min(j * batch_size, length(permnos))
        ]

        crsp_daily_sub <- dsf_db |>
          filter(
            permno %in% permno_batch,
            between(dlycaldt, start_date, end_date)
          ) |>
          inner_join(
            stksecurityinfohist_db |>
              filter(sharetype == "NS" &
                       securitytype == "EQTY" &
                       securitysubtype == "COM" &
                       usincflg == "Y" &
                       issuertype %in% c("ACOR", "CORP") &
                       primaryexch %in% c("N", "A", "Q") &
                       conditionaltype %in% c("RW", "NW") &
                       tradingstatusflg == "A") |>
              select(permno, secinfostartdt, secinfoenddt),
            join_by(permno)
          ) |>
          filter(between(dlycaldt, secinfostartdt, secinfoenddt))  |>
          select(permno, date = dlycaldt, ret = dlyret, additional_columns) |>
          collect() |>
          drop_na()

        if (nrow(crsp_daily_sub) > 0) {

          crsp_daily_list[[j]] <- crsp_daily_sub |>
            mutate(month = floor_date(date, "month")) |>
            left_join(factors_ff3_daily |>
                        select(date, risk_free), join_by(date)) |>
            mutate(
              ret_excess = ret - risk_free,
              ret_excess = pmax(ret_excess, -1)
            ) |>
            select(-risk_free)
        }
      }

      disconnection_connection(con)

      processed_data <- bind_rows(crsp_daily_list)

    }

  }

  processed_data
}
