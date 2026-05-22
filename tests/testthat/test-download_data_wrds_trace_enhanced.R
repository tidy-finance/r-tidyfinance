test_that("download_data_wrds_trace_enhanced validates cusips", {
  expect_error(
    download_data_wrds_trace_enhanced(c("123", NA_character_)),
    "cusips"
  )
})

test_that("download_data_wrds_trace_enhanced cleans TRACE data", {
  trace <- local({
    row <- function(
      id,
      msg,
      orig,
      vol,
      pr,
      side,
      contra,
      date,
      rpt,
      status,
      asof = "",
      wis = "N",
      settle = 1,
      stlmnt = date + 1,
      spcl = ""
    ) {
      tibble::tibble(
        cusip_id = id,
        msg_seq_nb = msg,
        orig_msg_seq_nb = orig,
        entrd_vol_qt = vol,
        rptd_pr = pr,
        yld_pt = 4,
        rpt_side_cd = side,
        cntra_mp_id = contra,
        trd_exctn_dt = date,
        trd_exctn_tm = "10:00:00",
        trd_rpt_dt = rpt,
        trd_rpt_tm = "10:01:00",
        pr_trd_dt = date,
        trc_st = status,
        asof_cd = asof,
        wis_fl = wis,
        days_to_sttl_ct = settle,
        stlmnt_dt = stlmnt,
        spcl_trd_fl = spcl
      )
    }

    d1 <- as.Date("2013-01-02")
    d0 <- as.Date("2011-01-02")

    dplyr::bind_rows(
      row("00101JAH9", 10, NA, 100, 99, "B", "C", d1, d1, "T"),
      row("00101JAH9", 11, NA, 101, 99, "S", "D", d1, d1, "T"),
      row("00101JAH9", 11, NA, 101, 99, "S", "D", d1, d1, "X"),
      row("00101JAH9", 12, NA, 102, 99, "S", "C", d1, d1, "T"),
      row("00101JAH9", 99, 12, 102, 99, "S", "C", d1, d1, "Y"),
      row("00101JAH9", 13, NA, 103, 99, "S", "D", d1, d1, "T"),
      row("00101JAH9", 14, NA, 103, 99, "B", "D", d1, d1, "T"),
      row("00101JAH9", 15, NA, 104, 99, "B", "D", d1, d1, "T"),
      row("00101JAH9", 16, NA, 105, 99, "B", "C", d1, d1, "T", settle = 8),
      row("00101JAH9", 17, NA, 106, 99, "B", "C", d1, d1, "T", stlmnt = d1 + 8),
      row("00101JAH9", 18, NA, 107, 99, "B", "C", d1, d1, "T", wis = "Y"),
      row("00101JAH9", 19, NA, 108, 99, "B", "C", d1, d1, "T", spcl = "Y"),
      row("00101JAH9", 20, NA, 109, 99, "B", "C", d1, d1, "T", asof = "A"),
      row("00101JAH9", 21, NA, 201, 99, "B", "C", d0, d0, "T"),
      row("00101JAH9", 90, 21, 201, 99, "B", "C", d0, d0, "C"),
      row("00101JAH9", 22, NA, 202, 99, "B", "C", d0, d0, "T"),
      row("00101JAH9", 23, 22, 203, 99, "B", "C", d0, d0, "W"),
      row("00101JAH9", 24, 23, 204, 99, "B", "C", d0, d0, "W"),
      row("00101JAH9", 25, 999, 205, 99, "B", "C", d0, d0, "W"),
      row("00101JAH9", 30, NA, 301, 99, "B", "C", d0, d0, "T"),
      row("00101JAH9", 31, NA, 301, 99, "B", "C", d0, d0, "T", asof = "R")
    )
  })

  con <- list()

  testthat::local_mocked_bindings(
    validate_dates = function(start_date, end_date, use_default_range) {
      expect_true(use_default_range)

      list(
        start_date = as.Date("2010-01-01"),
        end_date = as.Date("2014-01-01")
      )
    },
    get_wrds_connection = function() con,
    disconnect_connection = function(x) expect_identical(x, con),
    tbl = function(src, from) trace
  )

  out <- download_data_wrds_trace_enhanced(
    "00101JAH9",
    "2010-01-01",
    "2014-01-01"
  )

  expect_equal(
    out$entrd_vol_qt,
    c(204, 100, 103, 104)
  )

  expect_equal(
    out$rpt_side_cd,
    c("B", "B", "S", "B")
  )

  expect_equal(
    out$cntra_mp_id,
    c("C", "C", "D", "D")
  )

  expect_s3_class(out, "data.frame")
  expect_named(
    out,
    c(
      "cusip_id",
      "trd_exctn_dt",
      "trd_exctn_tm",
      "rptd_pr",
      "entrd_vol_qt",
      "yld_pt",
      "rpt_side_cd",
      "cntra_mp_id"
    )
  )
})
