#' Download Data from WRDS Compustat
#'
#' This function downloads financial data from the WRDS Compustat database for a
#' given dataset, start date, and end date. It filters the data
#' according to industry format, data format, and consolidation level, and
#' returns the most current data for each reporting period. Additionally, the
#' annual data also includes the calculated book equity (be),
#' operating profitability (op), and investment (inv) for each company.
#'
#' @param dataset The dataset to download ("compustat_annual" or "compustat_quarterly").
#' @param start_date Optional. A character string or Date object in "YYYY-MM-DD" format
#'   specifying the start date for the data. If not provided, a subset of the dataset is returned.
#' @param end_date Optional. A character string or Date object in "YYYY-MM-DD" format
#'   specifying the end date for the data. If not provided, a subset of the dataset is returned.
#' @param type `r lifecycle::badge("deprecated")` Use `dataset` instead.
#' @param additional_columns Additional columns from the Compustat table
#'   as a character vector.
#' @param only_us A logical indicating whether only US firms should be returned
#'   (i.e., excluding Canadian firms).
#'
#' @returns A data frame with financial data for the specified period, including
#'   variables for book equity (be), operating profitability (op), investment
#'   (inv), and others.
#'
#' @export
#' @examples
#' \dontrun{
#'   download_data_wrds_compustat("compustat_annual", "2020-01-01", "2020-12-31")
#'   download_data_wrds_compustat("compustat_quarterly", "2020-01-01", "2020-12-31")
#'
#'   # Add additional columns
#'   download_data_wrds_compustat("compustat_annual", additional_columns = c("aodo", "aldo"))
#' }
download_data_wrds_compustat <- function(
  dataset = NULL,
  start_date = NULL,
  end_date = NULL,
  type = deprecated(),
  additional_columns = NULL,
  only_us = FALSE
) {
  # Handle explicit type argument
  if (lifecycle::is_present(type)) {
    lifecycle::deprecate_warn(
      when = "0.5.0",
      what = "download_data_wrds_compustat(type)",
      details = "Use the `dataset` argument instead."
    )
    dataset <- sub("^wrds_", "", type)
  }

  # Handle legacy type passed as dataset argument
  if (!is.null(dataset) && is_legacy_type_wrds(dataset)) {
    lifecycle::deprecate_warn(
      when = "0.5.0",
      what = "download_data_wrds_compustat(type)",
      details = paste0(
        "The `type` argument is deprecated. ",
        "Use `dataset` instead (e.g., 'compustat_annual' instead of 'wrds_compustat_annual')."
      )
    )
    dataset <- sub("^wrds_", "", dataset)
  }

  if (is.null(dataset)) {
    cli::cli_abort("Argument {.arg dataset} is required.")
  }

  check_supported_dataset_wrds_compustat(dataset)

  dates <- validate_dates(start_date, end_date, use_default_range = TRUE)
  start_date <- dates$start_date
  end_date <- dates$end_date

  con <- get_wrds_connection()

  if (dataset == "compustat_annual") {
    funda_db <- tbl(con, I("comp.funda"))

    compustat <- funda_db |>
      filter(
        indfmt == "INDL" &
          datafmt == "STD" &
          consol == "C" &
          between(datadate, start_date, end_date)
      ) |>
      select(
        gvkey,
        datadate,
        seq,
        ceq,
        at,
        lt,
        txditc,
        txdb,
        itcb,
        pstkrv,
        pstkl,
        pstk,
        capx,
        oancf,
        sale,
        cogs,
        xint,
        xsga,
        curcd,
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
        op = (sale -
          coalesce(cogs, 0) -
          coalesce(xsga, 0) -
          coalesce(xint, 0)) /
          be
      )

    compustat <- compustat |>
      mutate(year = year(datadate)) |>
      group_by(gvkey, year) |>
      filter(datadate == max(datadate)) |>
      ungroup() |>
      mutate(date = floor_date(datadate, "month"))

    if (isTRUE(only_us)) {
      compustat <- compustat |>
        filter(.data$curcd == "USD")
    }

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
      ) |>
      select(gvkey, date, datadate, everything(), -year)
  } else if (dataset == "compustat_quarterly") {
    fundq_db <- tbl(con, I("comp.fundq"))

    compustat <- fundq_db |>
      filter(
        indfmt == "INDL" &
          datafmt == "STD" &
          consol == "C" &
          between(datadate, start_date, end_date)
      ) |>
      select(
        gvkey,
        datadate,
        rdq,
        fqtr,
        fyearq,
        atq,
        ceqq,
        curcdq,
        all_of(additional_columns)
      ) |>
      collect()

    disconnection_connection(con)

    compustat <- compustat |>
      tidyr::drop_na(gvkey, datadate, fyearq, fqtr) |>
      mutate(date = floor_date(datadate, "month")) |>
      group_by(gvkey, fyearq, fqtr) |>
      filter(datadate == max(datadate)) |>
      slice_head(n = 1) |>
      ungroup() |>
      group_by(gvkey, date) |>
      arrange(gvkey, date, rdq) |>
      slice_head(n = 1) |>
      ungroup() |>
      filter(if_else(is.na(rdq), TRUE, date < rdq))

    if (isTRUE(only_us)) {
      compustat <- compustat |>
        filter(.data$curcdq == "USD")
    }

    processed_data <- compustat |>
      select(gvkey, date, datadate, atq, ceqq, all_of(additional_columns))
  } else {
    cli::cli_abort("Unsupported Compustat dataset: {.val {dataset}}")
  }

  processed_data
}

#' Check if WRDS Compustat dataset is supported
#' @noRd
check_supported_dataset_wrds_compustat <- function(dataset) {
  supported_datasets <- c("compustat_annual", "compustat_quarterly")

  if (!dataset %in% supported_datasets) {
    cli::cli_abort(c(
      "Unsupported Compustat dataset: {.val {dataset}}",
      "i" = "Supported datasets: {.val {supported_datasets}}"
    ))
  }
}
