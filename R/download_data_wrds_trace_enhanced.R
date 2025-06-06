#' Download Enhanced TRACE Data from WRDS
#'
#' Establishes a connection to the WRDS database to download the specified
#' CUSIPs trade messages from the Trade Reporting and Compliance Engine (TRACE).
#' The trade data is cleaned as suggested by Dick-Nielsen (2009, 2014).
#'
#' @param cusips A character vector specifying the 9-digit CUSIPs to download.
#' @param start_date Optional. A character string or Date object in "YYYY-MM-DD" format
#'   specifying the start date for the data. If not provided, a subset of the dataset is returned.
#' @param end_date Optional. A character string or Date object in "YYYY-MM-DD" format
#'   specifying the end date for the data. If not provided, a subset of the dataset is returned.
#'
#' @returns A data frame containing the cleaned trade messages from TRACE for the
#'   selected CUSIPs over the time window specified. Output variables include
#'   identifying information (i.e., CUSIP, trade date/time) and trade-specific
#'   information (i.e., price/yield, volume, counterparty, and reporting side).
#'
#' @export
#' @examples
#' \donttest{
#'   trace_enhanced <- download_data_wrds_trace_enhanced("00101JAH9", "2019-01-01", "2021-12-31")
#' }
download_data_wrds_trace_enhanced <- function(
  cusips,
  start_date = NULL,
  end_date = NULL
) {
  rlang::check_installed(
    "dbplyr",
    reason = "to download type clean_trace."
  )

  if (!is.character(cusips) || anyNA(cusips) || !all(nchar(cusips) == 9)) {
    cli::cli_abort(
      "{.arg cusip} must be a character vector of 9-digit CUSIPs, not {.obj_type_friendly {cusips}}."
    )
  }

  dates <- validate_dates(start_date, end_date, use_default_range = TRUE)
  start_date <- dates$start_date
  end_date <- dates$end_date

  con <- get_wrds_connection()

  trace_enhanced_db <- tbl(con, I("trace.trace_enhanced"))

  trace_all <- trace_enhanced_db |>
    filter(
      cusip_id %in% cusips,
      between(trd_exctn_dt, start_date, end_date)
    ) |>
    select(
      cusip_id,
      msg_seq_nb,
      orig_msg_seq_nb,
      entrd_vol_qt,
      rptd_pr,
      yld_pt,
      rpt_side_cd,
      cntra_mp_id,
      trd_exctn_dt,
      trd_exctn_tm,
      trd_rpt_dt,
      trd_rpt_tm,
      pr_trd_dt,
      trc_st,
      asof_cd,
      wis_fl,
      days_to_sttl_ct,
      stlmnt_dt,
      spcl_trd_fl
    ) |>
    collect()

  disconnection_connection(con)

  # Enhanced Trace: Post 06-02-2012 -----------------------------------------
  # Trades (trc_st = T) and correction (trc_st = R)
  trace_post_TR <- trace_all |>
    filter((trc_st == "T" | trc_st == "R"), trd_rpt_dt >= as.Date("2012-02-06"))

  # Cancelations (trc_st = X) and correction cancelations (trc_st = C)
  trace_post_XC <- trace_all |>
    filter((trc_st == "X" | trc_st == "C"), trd_rpt_dt >= as.Date("2012-02-06"))

  # Cleaning corrected and cancelled trades
  trace_post_TR <- trace_post_TR |>
    anti_join(
      trace_post_XC,
      by = c(
        "cusip_id",
        "msg_seq_nb",
        "entrd_vol_qt",
        "rptd_pr",
        "rpt_side_cd",
        "cntra_mp_id",
        "trd_exctn_dt",
        "trd_exctn_tm"
      )
    )

  # Reversals (trc_st = Y)
  trace_post_Y <- trace_all |>
    filter(trc_st == "Y", trd_rpt_dt >= as.Date("2012-02-06"))

  # Clean reversals
  ## match the orig_msg_seq_nb of the Y-message to
  ## the msg_seq_nb of the main message
  trace_post <- trace_post_TR |>
    anti_join(
      trace_post_Y,
      by = join_by(
        cusip_id,
        msg_seq_nb == orig_msg_seq_nb,
        entrd_vol_qt,
        rptd_pr,
        rpt_side_cd,
        cntra_mp_id,
        trd_exctn_dt,
        trd_exctn_tm
      )
    )

  # Enhanced TRACE: Pre 06-02-2012 ------------------------------------------
  # Cancelations (trc_st = C)
  trace_pre_C <- trace_all |>
    filter(trc_st == "C", trd_rpt_dt < as.Date("2012-02-06"))

  # Trades w/o cancelations
  ## match the orig_msg_seq_nb of the C-message
  ## to the msg_seq_nb of the main message
  trace_pre_T <- trace_all |>
    filter(trc_st == "T", trd_rpt_dt < as.Date("2012-02-06")) |>
    anti_join(
      trace_pre_C,
      by = join_by(
        cusip_id,
        msg_seq_nb == orig_msg_seq_nb,
        entrd_vol_qt,
        rptd_pr,
        rpt_side_cd,
        cntra_mp_id,
        trd_exctn_dt,
        trd_exctn_tm
      )
    )

  # Corrections (trc_st = W) - W can also correct a previous W
  trace_pre_W <- trace_all |>
    filter(trc_st == "W", trd_rpt_dt < as.Date("2012-02-06"))

  # Implement corrections in a loop
  ## Correction control
  correction_control <- nrow(trace_pre_W)
  correction_control_last <- nrow(trace_pre_W)

  ## Correction loop
  while (correction_control > 0) {
    # Corrections that correct some msg
    trace_pre_W_correcting <- trace_pre_W |>
      semi_join(
        trace_pre_T,
        by = join_by(cusip_id, trd_exctn_dt, orig_msg_seq_nb == msg_seq_nb)
      )

    # Corrections that do not correct some msg
    trace_pre_W <- trace_pre_W |>
      anti_join(
        trace_pre_T,
        by = join_by(cusip_id, trd_exctn_dt, orig_msg_seq_nb == msg_seq_nb)
      )

    # Delete msgs that are corrected and add correction msgs
    trace_pre_T <- trace_pre_T |>
      anti_join(
        trace_pre_W_correcting,
        by = join_by(cusip_id, trd_exctn_dt, msg_seq_nb == orig_msg_seq_nb)
      ) |>
      union_all(trace_pre_W_correcting)

    # Escape if no corrections remain or they cannot be matched
    correction_control <- nrow(trace_pre_W)

    if (correction_control == correction_control_last) {
      correction_control <- 0
    }

    correction_control_last <- nrow(trace_pre_W)
  }

  # Clean reversals
  ## Record reversals
  trace_pre_R <- trace_pre_T |>
    filter(asof_cd == 'R') |>
    group_by(
      cusip_id,
      trd_exctn_dt,
      entrd_vol_qt,
      rptd_pr,
      rpt_side_cd,
      cntra_mp_id
    ) |>
    arrange(trd_exctn_tm, trd_rpt_dt, trd_rpt_tm) |>
    mutate(seq = row_number()) |>
    ungroup()

  ## Remove reversals and the reversed trade
  trace_pre <- trace_pre_T |>
    filter(is.na(asof_cd) | !(asof_cd %in% c('R', 'X', 'D'))) |>
    group_by(
      cusip_id,
      trd_exctn_dt,
      entrd_vol_qt,
      rptd_pr,
      rpt_side_cd,
      cntra_mp_id
    ) |>
    arrange(trd_exctn_tm, trd_rpt_dt, trd_rpt_tm) |>
    mutate(seq = row_number()) |>
    ungroup() |>
    anti_join(
      trace_pre_R,
      by = join_by(
        cusip_id,
        trd_exctn_dt,
        entrd_vol_qt,
        rptd_pr,
        rpt_side_cd,
        cntra_mp_id,
        seq
      )
    ) |>
    select(-seq)

  # Agency trades -----------------------------------------------------------
  # Combine pre and post trades
  trace_clean <- trace_post |>
    union_all(trace_pre)

  # Keep angency sells and unmatched agency buys
  ## Agency sells
  trace_agency_sells <- trace_clean |>
    filter(cntra_mp_id == "D", rpt_side_cd == "S")

  # Agency buys that are unmatched
  trace_agency_buys_filtered <- trace_clean |>
    filter(cntra_mp_id == "D", rpt_side_cd == "B") |>
    anti_join(
      trace_agency_sells,
      by = join_by(cusip_id, trd_exctn_dt, entrd_vol_qt, rptd_pr)
    )

  # Agency clean
  trace_clean <- trace_clean |>
    filter(cntra_mp_id == "C") |>
    union_all(trace_agency_sells) |>
    union_all(trace_agency_buys_filtered)

  # Additional Filters ------------------------------------------------------
  trace_add_filters <- trace_clean |>
    mutate(days_to_sttl_ct2 = stlmnt_dt - trd_exctn_dt) |>
    filter(
      is.na(days_to_sttl_ct) | as.numeric(days_to_sttl_ct) <= 7,
      is.na(days_to_sttl_ct2) | as.numeric(days_to_sttl_ct2) <= 7,
      wis_fl == "N",
      is.na(spcl_trd_fl) | spcl_trd_fl == "",
      is.na(asof_cd) | asof_cd == ""
    )

  # Output ------------------------------------------------------------------
  # Only keep necessary columns
  trace_final <- trace_add_filters |>
    arrange(cusip_id, trd_exctn_dt, trd_exctn_tm) |>
    select(
      cusip_id,
      trd_exctn_dt,
      trd_exctn_tm,
      rptd_pr,
      entrd_vol_qt,
      yld_pt,
      rpt_side_cd,
      cntra_mp_id
    ) |>
    mutate(
      trd_exctn_tm = format(
        as_datetime(trd_exctn_tm, tz = "America/New_York"),
        "%H:%M:%S"
      )
    )

  trace_final
}
