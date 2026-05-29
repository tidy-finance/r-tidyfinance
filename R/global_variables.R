# Set global variables ------------------------------------------------
# The remaining entries cannot be expressed with the `.data[["x"]]` /
# `"x"` idioms used elsewhere in the package:
#   * column names referenced inside `dplyr::join_by()` / `closest()`,
#     which only accept bare symbols; and
#   * a few bare column references (e.g. the exchange/symbol/name lookups
#     in `download_data_constituents()`) plus locally bound helpers
#     (`first_crsp_date`, `mktcap_lag`, `url`).
utils::globalVariables(
  c(
    # join_by() / closest() keys
    "..date",
    "..id",
    ".date",
    ".lower",
    ".src_date",
    ".upper",
    "cntra_mp_id",
    "cusip_id",
    "date",
    "dlstdt",
    "entrd_vol_qt",
    "gvkey",
    "id",
    "issuer_id",
    "lower_bound",
    "msg_seq_nb",
    "n_portfolios_main",
    "orig_msg_seq_nb",
    "permno",
    "rpt_side_cd",
    "rptd_pr",
    "seq",
    "sorting_method",
    "sorting_variable",
    "sorting_variable_lag",
    "trd_exctn_dt",
    "trd_exctn_tm",
    "upper_bound",
    "year",
    # remaining bare column references / locally bound helpers
    "exchange",
    "first_crsp_date",
    "mktcap_lag",
    "name",
    "symbol",
    "url"
  )
)
