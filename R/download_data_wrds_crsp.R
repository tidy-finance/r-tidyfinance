#' Download Data from WRDS CRSP
#'
#' This function downloads and processes stock return data from the CRSP
#' database for a specified period. Users can choose between monthly and daily
#' datasets. The function also adjusts returns for delisting and calculates
#' market capitalization and excess returns over the risk-free rate.
#'
#' @param dataset A string specifying the CRSP dataset to download:
#'   "crsp_monthly" or "crsp_daily".
#' @param start_date Optional. A character string or Date object in "YYYY-MM-DD" format
#'   specifying the start date for the data. If not provided, a subset of the dataset is returned.
#' @param end_date Optional. A character string or Date object in "YYYY-MM-DD" format
#'   specifying the end date for the data. If not provided, a subset of the dataset is returned.
#' @param type `r lifecycle::badge("deprecated")` Use `dataset` instead.
#' @param batch_size An optional integer specifying the batch size for
#'   processing daily data, with a default of 500.
#' @param version An optional character specifying which CRSP version to use.
#'   "v2" (the default) uses the updated second version of CRSP, and "v1"
#'   downloads the legacy version of CRSP.
#' @param additional_columns Additional columns from the CRSP monthly or
#'   daily data as a character vector.
#' @param add_ccm_links A logical indicating whether CRSP-Compustat links should be
#'   added automtically using [download_data_wrds_ccm_links()].
#' @param adjust_volume A logical indicating whether daily CRSP trading volume data
#'   should be adjusted according to Gao & Ritter (2010).
#'
#' @returns A data frame containing CRSP stock returns, adjusted for delistings,
#'   along with calculated market capitalization and excess returns over the
#'   risk-free rate. The structure of the returned data frame depends on the
#'   selected dataset.
#'
#' @references
#'   Gao, X., & Ritter, J. R. (2010). The marketing of seasoned equity offerings.
#'   *Journal of Financial Economics*, 97(1), 33–52.
#'   \doi{10.1016/j.jfineco.2010.03.007}
#'
#' @export
#' @examples
#' \dontrun{
#'   crsp_monthly <- download_data_wrds_crsp("crsp_monthly", "2020-11-01", "2020-12-31")
#'   crsp_daily <- download_data_wrds_crsp("crsp_daily", "2020-12-01", "2020-12-31")
#'
#'   # Add additional columns
#'   download_data_wrds_crsp("crsp_monthly", "2020-11-01", "2020-12-31",
#'                           additional_columns = c("mthvol", "mthvolflg"))
#' }
download_data_wrds_crsp <- function(
  dataset = NULL,
  start_date = NULL,
  end_date = NULL,
  type = deprecated(),
  batch_size = 500,
  version = "v2",
  additional_columns = NULL,
  add_ccm_links = FALSE,
  adjust_volume = FALSE
) {
  # Handle explicit type argument
  if (lifecycle::is_present(type)) {
    lifecycle::deprecate_warn(
      when = "0.5.0",
      what = "download_data_wrds_crsp(type)",
      details = "Use the `dataset` argument instead."
    )
    dataset <- sub("^wrds_", "", type)
  }

  # Handle legacy type passed as dataset argument
  if (!is.null(dataset) && is_legacy_type_wrds(dataset)) {
    lifecycle::deprecate_warn(
      when = "0.5.0",
      what = "download_data_wrds_crsp(type)",
      details = paste0(
        "The `type` argument is deprecated. ",
        "Use `dataset` instead (e.g., 'crsp_monthly' instead of 'wrds_crsp_monthly')."
      )
    )
    dataset <- sub("^wrds_", "", dataset)
  }

  if (is.null(dataset)) {
    cli::cli_abort("Argument {.arg dataset} is required.")
  }

  check_supported_dataset_wrds_crsp(dataset)

  batch_size <- as.integer(batch_size)
  if (batch_size <= 0) {
    cli::cli_abort("{.arg batch_size} must be an integer larger than 0.")
  }

  if (!(version %in% c("v1", "v2"))) {
    cli::cli_abort(
      "{.arg version} must be a character equal to {.str v1} or {.str v2}."
    )
  }

  dates <- validate_dates(start_date, end_date, use_default_range = TRUE)
  start_date <- dates$start_date
  end_date <- dates$end_date

  con <- get_wrds_connection()

  if (dataset == "crsp_monthly") {
    if (version == "v1") {
      msf_db <- tbl(con, I("crsp.msf"))
      msenames_db <- tbl(con, I("crsp.msenames"))
      msedelist_db <- tbl(con, I("crsp.msedelist"))

      msf_db_columns <- c(
        "permno",
        colnames(msf_db)[-which(colnames(msf_db) %in% colnames(msenames_db))]
      )

      crsp_monthly <- msf_db |>
        filter(between(date, start_date, end_date)) |>
        select(all_of(msf_db_columns)) |>
        inner_join(
          msenames_db |>
            filter(shrcd %in% c(10, 11)),
          join_by(permno)
        ) |>
        filter(between(date, namedt, nameendt)) |>
        mutate(calculation_date = date, date = floor_date(date, "month")) |>
        left_join(
          msedelist_db |>
            select(permno, dlstdt, dlret, dlstcd) |>
            mutate(date = floor_date(dlstdt, "month")),
          join_by(permno, date)
        ) |>
        select(
          permno,
          date,
          calculation_date,
          ret,
          shrout,
          altprc,
          cfacpr,
          exchcd,
          siccd,
          dlret,
          dlstcd,
          all_of(additional_columns)
        ) |>
        collect() |>
        mutate(
          date = ymd(date),
          shrout = shrout * 1000
        )

      disconnection_connection(con)

      crsp_monthly <- crsp_monthly |>
        mutate(
          mktcap = abs(shrout * altprc) / 10^6,
          mktcap = na_if(mktcap, 0)
        )

      mktcap_lag <- crsp_monthly |>
        mutate(date = date %m+% months(1)) |>
        select(permno, date, mktcap_lag = mktcap)

      crsp_monthly <- crsp_monthly |>
        left_join(mktcap_lag, join_by(permno, date))

      crsp_monthly <- crsp_monthly |>
        mutate(
          exchange = case_when(
            exchcd %in% c(1, 31) ~ "NYSE",
            exchcd %in% c(2, 32) ~ "AMEX",
            exchcd %in% c(3, 33) ~ "NASDAQ",
            .default = "Other"
          )
        )

      crsp_monthly <- crsp_monthly |>
        mutate(
          industry = case_when(
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
          )
        )

      crsp_monthly <- crsp_monthly |>
        mutate(
          ret_adj = case_when(
            is.na(dlstcd) ~ ret,
            !is.na(dlstcd) & !is.na(dlret) ~ dlret,
            dlstcd %in%
              c(500, 520, 580, 584) |
              (dlstcd >= 551 & dlstcd <= 574) ~
              -0.30,
            dlstcd == 100 ~ ret,
            .default = -1
          )
        ) |>
        select(-c(dlret, dlstcd))

      crsp_monthly <- crsp_monthly |>
        mutate(
          prc_adj = abs(na_if(altprc, 0)) / cfacpr,
          prc_adj = if_else(is.infinite(prc_adj), NA_real_, prc_adj)
        )

      factors_ff_3_monthly <- download_data_factors_ff(
        dataset = "Fama/French 3 Factors",
        start_date = start_date,
        end_date = end_date
      )

      crsp_monthly <- crsp_monthly |>
        left_join(factors_ff_3_monthly, join_by(date)) |>
        mutate(
          ret_excess = ret_adj - risk_free,
          ret_excess = pmax(ret_excess, -1)
        ) |>
        select(-risk_free, -mkt_excess, -hml, -smb)

      processed_data <- crsp_monthly |>
        tidyr::drop_na(ret_excess, mktcap)
    } else {
      msf_db <- tbl(con, I("crsp.msf_v2"))
      stksecurityinfohist_db <- tbl(con, I("crsp.stksecurityinfohist"))

      msf_db_columns <- c(
        "permno",
        colnames(msf_db)[
          -which(colnames(msf_db) %in% colnames(stksecurityinfohist_db))
        ]
      )

      crsp_monthly <- msf_db |>
        filter(between(mthcaldt, start_date, end_date)) |>
        select(all_of(msf_db_columns)) |>
        inner_join(
          stksecurityinfohist_db |>
            filter(
              sharetype == "NS" &
                securitytype == "EQTY" &
                securitysubtype == "COM" &
                usincflg == "Y" &
                issuertype %in% c("ACOR", "CORP") &
                primaryexch %in% c("N", "A", "Q") &
                conditionaltype %in% c("RW", "NW") &
                tradingstatusflg == "A"
            ),
          join_by(permno)
        ) |>
        filter(between(mthcaldt, secinfostartdt, secinfoenddt)) |>
        mutate(date = floor_date(mthcaldt, "month")) |>
        select(
          permno,
          date,
          calculation_date = mthcaldt,
          ret = mthret,
          shrout,
          prc = mthprc,
          primaryexch,
          siccd,
          all_of(additional_columns)
        ) |>
        collect() |>
        mutate(
          date = ymd(date),
          shrout = shrout * 1000
        )

      disconnection_connection(con)

      crsp_monthly <- crsp_monthly |>
        mutate(
          mktcap = shrout * prc / 10^6,
          mktcap = na_if(mktcap, 0)
        )

      mktcap_lag <- crsp_monthly |>
        mutate(date = date %m+% months(1)) |>
        select(permno, date, mktcap_lag = mktcap)

      crsp_monthly <- crsp_monthly |>
        left_join(mktcap_lag, join_by(permno, date))

      crsp_monthly <- crsp_monthly |>
        mutate(
          exchange = case_when(
            primaryexch == "N" ~ "NYSE",
            primaryexch == "A" ~ "AMEX",
            primaryexch == "Q" ~ "NASDAQ",
            .default = "Other"
          )
        )

      crsp_monthly <- crsp_monthly |>
        mutate(
          industry = case_when(
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
          )
        )

      factors_ff_3_monthly <- download_data_factors_ff(
        dataset = "Fama/French 3 Factors",
        start_date = start_date,
        end_date = end_date
      )

      crsp_monthly <- crsp_monthly |>
        left_join(factors_ff_3_monthly, join_by(date)) |>
        mutate(
          ret_excess = ret - risk_free,
          ret_excess = pmax(ret_excess, -1)
        ) |>
        select(-risk_free, -mkt_excess, -hml, -smb)

      processed_data <- crsp_monthly |>
        tidyr::drop_na(ret_excess, mktcap)
    }
  } else if (dataset == "crsp_daily") {
    if (version == "v1") {
      if (isTRUE(adjust_volume)) {
        if (!all(c("prc", "vol", "cfacpr", "exchcd") %in% additional_columns)) {
          cli::cli_abort(
            "{.val prc}, {.val vol}, {.val exchcd}, and {.val cfacpr} must be contained in {.arg additional_columns} for {.arg adjust_volume = TRUE}."
          )
        }
      }

      dsf_db <- tbl(con, I("crsp.dsf")) |>
        filter(between(date, start_date, end_date))
      msenames_db <- tbl(con, I("crsp.msenames"))
      msedelist_db <- tbl(con, I("crsp.msedelist"))

      dsf_db_columns <- c(
        "permno",
        colnames(dsf_db)[-which(colnames(dsf_db) %in% colnames(msenames_db))]
      )

      permnos <- dsf_db |>
        distinct(permno) |>
        pull()

      factors_ff_3_daily <- download_data_factors_ff(
        dataset = "Fama/French 3 Factors [Daily]",
        start_date = start_date,
        end_date = end_date
      )

      batches <- ceiling(length(permnos) / batch_size)

      crsp_daily_list <- list()

      cli::cli_progress_bar(
        "Downloading batches",
        total = batches,
        clear = TRUE
      )

      for (j in 1:batches) {
        permno_batch <- permnos[
          ((j - 1) * batch_size + 1):min(j * batch_size, length(permnos))
        ]

        crsp_daily_sub <- dsf_db |>
          filter(permno %in% permno_batch) |>
          select(all_of(dsf_db_columns)) |>
          inner_join(
            msenames_db |>
              filter(shrcd %in% c(10, 11)),
            join_by(permno)
          ) |>
          filter(between(date, namedt, nameendt)) |>
          select(permno, date, ret, all_of(additional_columns)) |>
          collect() |>
          tidyr::drop_na(permno, date, ret)

        if (nrow(crsp_daily_sub) > 0) {
          msedelist_sub <- msedelist_db |>
            filter(permno %in% permno_batch) |>
            select(permno, dlstdt, dlret) |>
            collect() |>
            tidyr::drop_na()

          crsp_daily_sub <- crsp_daily_sub |>
            left_join(msedelist_sub, join_by(permno, date == dlstdt)) |>
            bind_rows(
              msedelist_sub |>
                anti_join(crsp_daily_sub, join_by(permno, dlstdt == date))
            ) |>
            mutate(
              ret = if_else(!is.na(dlret), dlret, ret),
              date = if_else(!is.na(dlstdt), dlstdt, date)
            ) |>
            select(-c(dlret, dlstdt)) |>
            left_join(
              msedelist_sub |>
                select(permno, dlstdt),
              join_by(permno)
            ) |>
            mutate(dlstdt = tidyr::replace_na(dlstdt, as.Date(end_date))) |>
            filter(date <= dlstdt) |>
            select(-dlstdt)

          crsp_daily_list[[j]] <- crsp_daily_sub |>
            left_join(
              factors_ff_3_daily |>
                select(date, risk_free),
              join_by(date)
            ) |>
            mutate(
              ret_excess = ret - risk_free,
              ret_excess = pmax(ret_excess, -1)
            ) |>
            select(-risk_free)
        }
        cli::cli_progress_update()
      }

      disconnection_connection(con)

      processed_data <- bind_rows(crsp_daily_list)

      if (isTRUE(adjust_volume)) {
        # Gao and Ritter (2010) volume adjustment for NASDAQ trading volume
        gr_date_1 <- as.Date("2001-02-01")
        gr_date_2 <- as.Date("2002-01-01")
        gr_date_3 <- as.Date("2004-01-01")

        processed_data <- processed_data |>
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
    } else {
      dsf_db <- tbl(con, I("crsp.dsf_v2")) |>
        filter(between(dlycaldt, start_date, end_date))
      stksecurityinfohist_db <- tbl(con, I("crsp.stksecurityinfohist"))

      dsf_db_columns <- c(
        "permno",
        colnames(dsf_db)[
          -which(colnames(dsf_db) %in% colnames(stksecurityinfohist_db))
        ]
      )

      permnos <- dsf_db |>
        distinct(permno) |>
        pull()

      factors_ff_3_daily <- download_data_factors_ff(
        dataset = "Fama/French 3 Factors [Daily]",
        start_date = start_date,
        end_date = end_date
      )

      batches <- ceiling(length(permnos) / batch_size)

      crsp_daily_list <- list()

      cli::cli_progress_bar(
        "Downloading batches",
        total = batches,
        clear = TRUE
      )

      for (j in 1:batches) {
        permno_batch <- permnos[
          ((j - 1) * batch_size + 1):min(j * batch_size, length(permnos))
        ]

        crsp_daily_sub <- dsf_db |>
          filter(permno %in% permno_batch) |>
          select(all_of(dsf_db_columns)) |>
          inner_join(
            stksecurityinfohist_db |>
              filter(
                sharetype == "NS" &
                  securitytype == "EQTY" &
                  securitysubtype == "COM" &
                  usincflg == "Y" &
                  issuertype %in% c("ACOR", "CORP") &
                  primaryexch %in% c("N", "A", "Q") &
                  conditionaltype %in% c("RW", "NW") &
                  tradingstatusflg == "A"
              ),
            join_by(permno)
          ) |>
          filter(between(dlycaldt, secinfostartdt, secinfoenddt)) |>
          select(
            permno,
            date = dlycaldt,
            ret = dlyret,
            all_of(additional_columns)
          ) |>
          collect() |>
          tidyr::drop_na(permno, date, ret)

        if (nrow(crsp_daily_sub) > 0) {
          crsp_daily_list[[j]] <- crsp_daily_sub |>
            left_join(
              factors_ff_3_daily |>
                select(date, risk_free),
              join_by(date)
            ) |>
            mutate(
              ret_excess = ret - risk_free,
              ret_excess = pmax(ret_excess, -1)
            ) |>
            select(-risk_free)
        }
        cli::cli_progress_update()
      }

      disconnection_connection(con)

      processed_data <- bind_rows(crsp_daily_list)
    }
  } else {
    cli::cli_abort("Unsupported CRSP dataset: {.val {dataset}}")
  }

  if (isTRUE(add_ccm_links)) {
    ccm_links <- download_data_wrds_ccm_links()

    valid_links <- processed_data |>
      inner_join(
        ccm_links,
        join_by(permno),
        relationship = "many-to-many",
        multiple = "all"
      ) |>
      filter(!is.na(gvkey) & (date >= linkdt & date <= linkenddt)) |>
      select(permno, gvkey, date)

    processed_data <- processed_data |>
      left_join(valid_links, join_by(permno, date))
  }

  processed_data
}

#' Check if WRDS CRSP dataset is supported
#' @noRd
check_supported_dataset_wrds_crsp <- function(dataset) {
  supported_datasets <- c("crsp_monthly", "crsp_daily")

  if (!dataset %in% supported_datasets) {
    cli::cli_abort(c(
      "Unsupported CRSP dataset: {.val {dataset}}",
      "i" = "Supported datasets: {.val {supported_datasets}}"
    ))
  }
}
