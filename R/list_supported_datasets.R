#' List Supported Fama-French Datasets
#'
#' Returns a tibble with the supported Fama-French datasets, including
#' their names and frequencies (daily, weekly, monthly). Each dataset type
#' is associated with a specific Fama-French model (e.g., 3 factors, 5
#' factors). Additionally, it annotates each dataset with the domain
#' "Fama-French".
#'
#' @returns A tibble with columns: `type` (the type of dataset),
#'   `dataset_name` (a descriptive name of the dataset), `file_url` (the
#'   path of the source ZIP relative to Kenneth French's data library), and
#'   `domain` (the domain to which the dataset belongs, always "Fama-French").
#'
#' @family utility functions
list_supported_datasets_ff <- function() {
  # nolint start
  # To update this list, run data-raw/list_supported_datasets_ff.R and replace
  # the tribble() call below with its output.
  tribble(
    ~type, ~dataset_name, ~file_url,
    "factors_ff_3_monthly", "Fama/French 3 Factors", "ftp/F-F_Research_Data_Factors_CSV.zip",
    "factors_ff_3_weekly", "Fama/French 3 Factors [Weekly]", "ftp/F-F_Research_Data_Factors_weekly_CSV.zip",
    "factors_ff_3_daily", "Fama/French 3 Factors [Daily]", "ftp/F-F_Research_Data_Factors_daily_CSV.zip",
    "factors_ff_5_2x3_monthly", "Fama/French 5 Factors (2x3)", "ftp/F-F_Research_Data_5_Factors_2x3_CSV.zip",
    "factors_ff_5_2x3_daily", "Fama/French 5 Factors (2x3) [Daily]", "ftp/F-F_Research_Data_5_Factors_2x3_daily_CSV.zip",
    "factors_ff_size_monthly", "Portfolios Formed on Size", "ftp/Portfolios_Formed_on_ME_CSV.zip",
    "factors_ff_size_exdividends_monthly", "Portfolios Formed on Size [ex.Dividends]", "ftp/Portfolios_Formed_on_ME_Wout_Div_CSV.zip",
    "factors_ff_size_daily", "Portfolios Formed on Size [Daily]", "ftp/Portfolios_Formed_on_ME_Daily_CSV.zip",
    "factors_ff_bm_monthly", "Portfolios Formed on Book-to-Market", "ftp/Portfolios_Formed_on_BE-ME_CSV.zip",
    "factors_ff_bm_exdividends_monthly", "Portfolios Formed on Book-to-Market [ex. Dividends]", "ftp/Portfolios_Formed_on_BE-ME_Wout_Div_CSV.zip",
    "factors_ff_bm_daily", "Portfolios Formed on Book-to-Market [Daily]", "ftp/Portfolios_Formed_on_BE-ME_Daily_CSV.zip",
    "factors_ff_op_monthly", "Portfolios Formed on Operating Profitability", "ftp/Portfolios_Formed_on_OP_CSV.zip",
    "factors_ff_op_exdividends_monthly", "Portfolios Formed on Operating Profitability [ex. Dividends]", "ftp/Portfolios_Formed_on_OP_Wout_Div_CSV.zip",
    "factors_ff_op_daily", "Portfolios Formed on Operating Profitability [Daily]", "ftp/Portfolios_Formed_on_OP_Daily_CSV.zip",
    "factors_ff_inv_monthly", "Portfolios Formed on Investment", "ftp/Portfolios_Formed_on_INV_CSV.zip",
    "factors_ff_inv_exdividends_monthly", "Portfolios Formed on Investment [ex. Dividends]", "ftp/Portfolios_Formed_on_INV_Wout_Div_CSV.zip",
    "factors_ff_inv_daily", "Portfolios Formed on Investment [Daily]", "ftp/Portfolios_Formed_on_INV_Daily_CSV.zip",
    "factors_ff_6_size_and_bm_2_x_3_monthly", "6 Portfolios Formed on Size and Book-to-Market (2 x 3)", "ftp/6_Portfolios_2x3_CSV.zip",
    "factors_ff_6_size_and_bm_2_x_3_exdividends_monthly", "6 Portfolios Formed on Size and Book-to-Market (2 x 3) [ex. Dividends]", "ftp/6_Portfolios_2x3_Wout_Div_CSV.zip",
    "factors_ff_6_size_and_bm_2_x_3_weekly", "6 Portfolios Formed on Size and Book-to-Market (2 x 3) [Weekly]", "ftp/6_Portfolios_2x3_weekly_CSV.zip",
    "factors_ff_6_size_and_bm_2_x_3_daily", "6 Portfolios Formed on Size and Book-to-Market (2 x 3) [Daily]", "ftp/6_Portfolios_2x3_daily_CSV.zip",
    "factors_ff_25_size_and_bm_5_x_5_monthly", "25 Portfolios Formed on Size and Book-to-Market (5 x 5)", "ftp/25_Portfolios_5x5_CSV.zip",
    "factors_ff_25_size_and_bm_5_x_5_exdividends_monthly", "25 Portfolios Formed on Size and Book-to-Market (5 x 5) [ex. Dividends]", "ftp/25_Portfolios_5x5_Wout_Div_CSV.zip",
    "factors_ff_25_size_and_bm_5_x_5_daily", "25 Portfolios Formed on Size and Book-to-Market (5 x 5) [Daily]", "ftp/25_Portfolios_5x5_Daily_CSV.zip",
    "factors_ff_100_size_and_bm_10_x_10_monthly", "100 Portfolios Formed on Size and Book-to-Market (10 x 10)", "ftp/100_Portfolios_10x10_CSV.zip",
    "factors_ff_100_size_and_bm_10_x_10_exdividends_monthly", "100 Portfolios Formed on Size and Book-to-Market (10 x 10) [ex. Dividends]", "ftp/100_Portfolios_10x10_Wout_Div_CSV.zip",
    "factors_ff_100_size_and_bm_10_x_10_daily", "100 Portfolios Formed on Size and Book-to-Market (10 x 10) [Daily]", "ftp/100_Portfolios_10x10_Daily_CSV.zip",
    "factors_ff_6_size_and_op_2_x_3_monthly", "6 Portfolios Formed on Size and Operating Profitability (2 x 3)", "ftp/6_Portfolios_ME_OP_2x3_CSV.zip",
    "factors_ff_6_size_and_op_2_x_3_exdividends_monthly", "6 Portfolios Formed on Size and Operating Profitability (2 x 3) [ex. Dividends]", "ftp/6_Portfolios_ME_OP_2x3_Wout_Div_CSV.zip",
    "factors_ff_6_size_and_op_2_x_3_daily", "6 Portfolios Formed on Size and Operating Profitability (2 x 3) [Daily]", "ftp/6_Portfolios_ME_OP_2x3_daily_CSV.zip",
    "factors_ff_25_size_and_op_5_x_5_monthly", "25 Portfolios Formed on Size and Operating Profitability (5 x 5)", "ftp/25_Portfolios_ME_OP_5x5_CSV.zip",
    "factors_ff_25_size_and_op_5_x_5_exdividends_monthly", "25 Portfolios Formed on Size and Operating Profitability (5 x 5) [ex. Dividends]", "ftp/25_Portfolios_ME_OP_5x5_Wout_Div_CSV.zip",
    "factors_ff_25_size_and_op_5_x_5_daily", "25 Portfolios Formed on Size and Operating Profitability (5 x 5) [Daily]", "ftp/25_Portfolios_ME_OP_5x5_daily_CSV.zip",
    "factors_ff_100_size_and_op_10_x_10_monthly", "100 Portfolios Formed on Size and Operating Profitability (10 x 10)", "ftp/100_Portfolios_ME_OP_10x10_CSV.zip",
    "factors_ff_100_size_and_op_10_x_10_exdividends_monthly", "100 Portfolios Formed on Size and Operating Profitability (10 x 10) [ex. Dividends]", "ftp/100_Portfolios_10x10_ME_OP_Wout_Div_CSV.zip",
    "factors_ff_100_size_and_op_10_x_10_daily", "100 Portfolios Formed on Size and Operating Profitability (10 x 10) [Daily]", "ftp/100_Portfolios_ME_OP_10x10_daily_CSV.zip",
    "factors_ff_6_size_and_inv_2_x_3_monthly", "6 Portfolios Formed on Size and Investment (2 x 3)", "ftp/6_Portfolios_ME_INV_2x3_CSV.zip",
    "factors_ff_6_size_and_inv_2_x_3_exdividends_monthly", "6 Portfolios Formed on Size and Investment (2 x 3) [ex. Dividends]", "ftp/6_Portfolios_ME_INV_2x3_Wout_Div_CSV.zip",
    "factors_ff_6_size_and_inv_2_x_3_daily", "6 Portfolios Formed on Size and Investment (2 x 3) [Daily]", "ftp/6_Portfolios_ME_INV_2x3_daily_CSV.zip",
    "factors_ff_25_size_and_inv_5_x_5_monthly", "25 Portfolios Formed on Size and Investment (5 x 5)", "ftp/25_Portfolios_ME_INV_5x5_CSV.zip",
    "factors_ff_25_size_and_inv_5_x_5_exdividends_monthly", "25 Portfolios Formed on Size and Investment (5 x 5) [ex. Dividends]", "ftp/25_Portfolios_ME_INV_5x5_Wout_Div_CSV.zip",
    "factors_ff_25_size_and_inv_5_x_5_daily", "25 Portfolios Formed on Size and Investment (5 x 5) [Daily]", "ftp/25_Portfolios_ME_INV_5x5_daily_CSV.zip",
    "factors_ff_100_size_and_inv_10_x_10_monthly", "100 Portfolios Formed on Size and Investment (10 x 10)", "ftp/100_Portfolios_ME_INV_10x10_CSV.zip",
    "factors_ff_100_size_and_inv_10_x_10_exdividends_monthly", "100 Portfolios Formed on Size and Investment (10 x 10) [ex. Dividends]", "ftp/100_Portfolios_10x10_ME_INV_Wout_Div_CSV.zip",
    "factors_ff_100_size_and_inv_10_x_10_daily", "100 Portfolios Formed on Size and Investment (10 x 10) [Daily]", "ftp/100_Portfolios_ME_INV_10x10_daily_CSV.zip",
    "factors_ff_25_bm_and_op_5_x_5_monthly", "25 Portfolios Formed on Book-to-Market and Operating Profitability (5 x 5)", "ftp/25_Portfolios_BEME_OP_5x5_CSV.zip",
    "factors_ff_25_bm_and_op_5_x_5_exdividends_monthly", "25 Portfolios Formed on Book-to-Market and Operating Profitability (5 x 5) [ex. Dividends]", "ftp/25_Portfolios_BEME_OP_5x5_Wout_Div_CSV.zip",
    "factors_ff_25_bm_and_op_5_x_5_daily", "25 Portfolios Formed on Book-to-Market and Operating Profitability (5 x 5) [Daily]", "ftp/25_Portfolios_BEME_OP_5x5_daily_CSV.zip",
    "factors_ff_25_bm_and_inv_5_x_5_monthly", "25 Portfolios Formed on Book-to-Market and Investment (5 x 5)", "ftp/25_Portfolios_BEME_INV_5x5_CSV.zip",
    "factors_ff_25_bm_and_inv_5_x_5_exdividends_monthly", "25 Portfolios Formed on Book-to-Market and Investment (5 x 5) [ex. Dividends]", "ftp/25_Portfolios_BEME_INV_5x5_Wout_Div_CSV.zip",
    "factors_ff_25_bm_and_inv_5_x_5_daily", "25 Portfolios Formed on Book-to-Market and Investment (5 x 5) [Daily]", "ftp/25_Portfolios_BEME_INV_5x5_daily_CSV.zip",
    "factors_ff_25_op_and_inv_5_x_5_monthly", "25 Portfolios Formed on Operating Profitability and Investment (5 x 5)", "ftp/25_Portfolios_OP_INV_5x5_CSV.zip",
    "factors_ff_25_op_and_inv_5_x_5_exdividends_monthly", "25 Portfolios Formed on Operating Profitability and Investment (5 x 5) [ex. Dividends]", "ftp/25_Portfolios_OP_INV_5x5_Wout_Div_CSV.zip",
    "factors_ff_25_op_and_inv_5_x_5_daily", "25 Portfolios Formed on Operating Profitability and Investment (5 x 5) [Daily]", "ftp/25_Portfolios_OP_INV_5x5_daily_CSV.zip",
    "factors_ff_32_size_bm_and_op_2_x_4_x_4_monthly", "32 Portfolios Formed on Size, Book-to-Market, and Operating Profitability (2 x 4 x 4)", "ftp/32_Portfolios_ME_BEME_OP_2x4x4_CSV.zip",
    "factors_ff_32_size_bm_and_op_2_x_4_x_4_exdividends_monthly", "32 Portfolios Formed on Size, Book-to-Market, and Operating Profitability (2 x 4 x 4) [ex. Dividends]", "ftp/32_Portfolios_ME_BEME_OP_2x4x4_Wout_Div_CSV.zip",
    "factors_ff_32_size_bm_and_inv_2_x_4_x_4_monthly", "32 Portfolios Formed on Size, Book-to-Market, and Investment (2 x 4 x 4)", "ftp/32_Portfolios_ME_BEME_INV_2x4x4_CSV.zip",
    "factors_ff_32_size_bm_and_inv_2_x_4_x_4_exdividends_monthly", "32 Portfolios Formed on Size, Book-to-Market, and Investment (2 x 4 x 4) [ex. Dividends]", "ftp/32_Portfolios_ME_BEME_INV_2x4x4_Wout_Div_CSV.zip",
    "factors_ff_32_size_op_and_inv_2_x_4_x_4_monthly", "32 Portfolios Formed on Size, Operating Profitability, and Investment (2 x 4 x 4)", "ftp/32_Portfolios_ME_OP_INV_2x4x4_CSV.zip",
    "factors_ff_32_size_op_and_inv_2_x_4_x_4_exdividends_monthly", "32 Portfolios Formed on Size, Operating Profitability, and Investment (2 x 4 x 4) [ex. Dividends]", "ftp/32_Portfolios_ME_OP_INV_2x4x4_Wout_Div_CSV.zip",
    "factors_ff_earningsprice_monthly", "Portfolios Formed on Earnings/Price", "ftp/Portfolios_Formed_on_E-P_CSV.zip",
    "factors_ff_earningsprice_exdividends_monthly", "Portfolios Formed on Earnings/Price [ex. Dividends]", "ftp/Portfolios_Formed_on_E-P_Wout_Div_CSV.zip",
    "factors_ff_cashflowprice_monthly", "Portfolios Formed on Cashflow/Price", "ftp/Portfolios_Formed_on_CF-P_CSV.zip",
    "factors_ff_cashflowprice_exdividends_monthly", "Portfolios Formed on Cashflow/Price [ex. Dividends]", "ftp/Portfolios_Formed_on_CF-P_Wout_Div_CSV.zip",
    "factors_ff_dividend_yield_monthly", "Portfolios Formed on Dividend Yield", "ftp/Portfolios_Formed_on_D-P_CSV.zip",
    "factors_ff_dividend_yield_exdividends_monthly", "Portfolios Formed on Dividend Yield [ex. Dividends]", "ftp/Portfolios_Formed_on_D-P_Wout_Div_CSV.zip",
    "factors_ff_6_size_and_earningsprice_monthly", "6 Portfolios Formed on Size and Earnings/Price", "ftp/6_Portfolios_ME_EP_2x3_CSV.zip",
    "factors_ff_6_size_and_earningsprice_exdividends_monthly", "6 Portfolios Formed on Size and Earnings/Price [ex. Dividends]", "ftp/6_Portfolios_ME_EP_2x3_Wout_Div_CSV.zip",
    "factors_ff_6_size_and_cashflowprice_monthly", "6 Portfolios Formed on Size and Cashflow/Price", "ftp/6_Portfolios_ME_CFP_2x3_CSV.zip",
    "factors_ff_6_size_and_cashflowprice_exdividends_monthly", "6 Portfolios Formed on Size and Cashflow/Price [ex. Dividends]", "ftp/6_Portfolios_ME_CFP_2x3_Wout_Div_CSV.zip",
    "factors_ff_6_size_and_dividend_yield_monthly", "6 Portfolios Formed on Size and Dividend Yield", "ftp/6_Portfolios_ME_DP_2x3_CSV.zip",
    "factors_ff_6_size_and_dividend_yield_exdividends_monthly", "6 Portfolios Formed on Size and Dividend Yield [ex. Dividends]", "ftp/6_Portfolios_ME_DP_2x3_Wout_Div_CSV.zip",
    "factors_ff_momentum_factor_monthly", "Momentum Factor (Mom)", "ftp/F-F_Momentum_Factor_CSV.zip",
    "factors_ff_momentum_factor_daily", "Momentum Factor (Mom) [Daily]", "ftp/F-F_Momentum_Factor_daily_CSV.zip",
    "factors_ff_6_size_and_momentum_2_x_3_monthly", "6 Portfolios Formed on Size and Momentum (2 x 3)", "ftp/6_Portfolios_ME_Prior_12_2_CSV.zip",
    "factors_ff_6_size_and_momentum_2_x_3_daily", "6 Portfolios Formed on Size and Momentum (2 x 3) [Daily]", "ftp/6_Portfolios_ME_Prior_12_2_Daily_CSV.zip",
    "factors_ff_25_size_and_momentum_5_x_5_monthly", "25 Portfolios Formed on Size and Momentum (5 x 5)", "ftp/25_Portfolios_ME_Prior_12_2_CSV.zip",
    "factors_ff_25_size_and_momentum_5_x_5_daily", "25 Portfolios Formed on Size and Momentum (5 x 5) [Daily]", "ftp/25_Portfolios_ME_Prior_12_2_Daily_CSV.zip",
    "factors_ff_10_momentum_monthly", "10 Portfolios Formed on Momentum", "ftp/10_Portfolios_Prior_12_2_CSV.zip",
    "factors_ff_10_momentum_daily", "10 Portfolios Formed on Momentum [Daily]", "ftp/10_Portfolios_Prior_12_2_Daily_CSV.zip",
    "factors_ff_shortterm_reversal_factor_st_rev_monthly", "Short-Term Reversal Factor (ST Rev)", "ftp/F-F_ST_Reversal_Factor_CSV.zip",
    "factors_ff_shortterm_reversal_factor_st_rev_daily", "Short-Term Reversal Factor (ST Rev) [Daily]", "ftp/F-F_ST_Reversal_Factor_daily_CSV.zip",
    "factors_ff_6_size_and_shortterm_reversal_2_x_3_monthly", "6 Portfolios Formed on Size and Short-Term Reversal (2 x 3)", "ftp/6_Portfolios_ME_Prior_1_0_CSV.zip",
    "factors_ff_6_size_and_shortterm_reversal_2_x_3_daily", "6 Portfolios Formed on Size and Short-Term Reversal (2 x 3) [Daily]", "ftp/6_Portfolios_ME_Prior_1_0_Daily_CSV.zip",
    "factors_ff_25_size_and_shortterm_reversal_5_x_5_monthly", "25 Portfolios Formed on Size and Short-Term Reversal (5 x 5)", "ftp/25_Portfolios_ME_Prior_1_0_CSV.zip",
    "factors_ff_25_size_and_shortterm_reversal_5_x_5_daily", "25 Portfolios Formed on Size and Short-Term Reversal (5 x 5) [Daily]", "ftp/25_Portfolios_ME_Prior_1_0_Daily_CSV.zip",
    "factors_ff_10_shortterm_reversal_monthly", "10 Portfolios Formed on Short-Term Reversal", "ftp/10_Portfolios_Prior_1_0_CSV.zip",
    "factors_ff_10_shortterm_reversal_daily", "10 Portfolios Formed on Short-Term Reversal [Daily]", "ftp/10_Portfolios_Prior_1_0_Daily_CSV.zip",
    "factors_ff_longterm_reversal_factor_lt_rev_monthly", "Long-Term Reversal Factor (LT Rev)", "ftp/F-F_LT_Reversal_Factor_CSV.zip",
    "factors_ff_longterm_reversal_factor_lt_rev_daily", "Long-Term Reversal Factor (LT Rev) [Daily]", "ftp/F-F_LT_Reversal_Factor_daily_CSV.zip",
    "factors_ff_6_size_and_longterm_reversal_2_x_3_monthly", "6 Portfolios Formed on Size and Long-Term Reversal (2 x 3)", "ftp/6_Portfolios_ME_Prior_60_13_CSV.zip",
    "factors_ff_6_size_and_longterm_reversal_2_x_3_daily", "6 Portfolios Formed on Size and Long-Term Reversal (2 x 3) [Daily]", "ftp/6_Portfolios_ME_Prior_60_13_Daily_CSV.zip",
    "factors_ff_25_size_and_longterm_reversal_5_x_5_monthly", "25 Portfolios Formed on Size and Long-Term Reversal (5 x 5)", "ftp/25_Portfolios_ME_Prior_60_13_CSV.zip",
    "factors_ff_25_size_and_longterm_reversal_5_x_5_daily", "25 Portfolios Formed on Size and Long-Term Reversal (5 x 5) [Daily]", "ftp/25_Portfolios_ME_Prior_60_13_Daily_CSV.zip",
    "factors_ff_10_longterm_reversal_monthly", "10 Portfolios Formed on Long-Term Reversal", "ftp/10_Portfolios_Prior_60_13_CSV.zip",
    "factors_ff_10_longterm_reversal_daily", "10 Portfolios Formed on Long-Term Reversal [Daily]", "ftp/10_Portfolios_Prior_60_13_Daily_CSV.zip",
    "factors_ff_accruals_monthly", "Portfolios Formed on Accruals", "ftp/Portfolios_Formed_on_AC_CSV.zip",
    "factors_ff_25_size_and_accruals_monthly", "25 Portfolios Formed on Size and Accruals", "ftp/25_Portfolios_ME_AC_5x5_CSV.zip",
    "factors_ff_market_beta_monthly", "Portfolios Formed on Market Beta", "ftp/Portfolios_Formed_on_BETA_CSV.zip",
    "factors_ff_25_size_and_market_beta_monthly", "25 Portfolios Formed on Size and Market Beta", "ftp/25_Portfolios_ME_BETA_5x5_CSV.zip",
    "factors_ff_net_share_issues_monthly", "Portfolios Formed on Net Share Issues", "ftp/Portfolios_Formed_on_NI_CSV.zip",
    "factors_ff_25_size_and_net_share_issues_monthly", "25 Portfolios Formed on Size and Net Share Issues", "ftp/25_Portfolios_ME_NI_5x5_CSV.zip",
    "factors_ff_variance_monthly", "Portfolios Formed on Variance", "ftp/Portfolios_Formed_on_VAR_CSV.zip",
    "factors_ff_25_size_and_variance_monthly", "25 Portfolios Formed on Size and Variance", "ftp/25_Portfolios_ME_VAR_5x5_CSV.zip",
    "factors_ff_residual_variance_monthly", "Portfolios Formed on Residual Variance", "ftp/Portfolios_Formed_on_RESVAR_CSV.zip",
    "factors_ff_25_size_and_residual_variance_monthly", "25 Portfolios Formed on Size and Residual Variance", "ftp/25_Portfolios_ME_RESVAR_5x5_CSV.zip",
    "factors_ff_5_industry_portfolios_monthly", "5 Industry Portfolios", "ftp/5_Industry_Portfolios_CSV.zip",
    "factors_ff_5_industry_portfolios_exdividends_monthly", "5 Industry Portfolios [ex. Dividends]", "ftp/5_Industry_Portfolios_Wout_Div_CSV.zip",
    "factors_ff_5_industry_portfolios_daily", "5 Industry Portfolios [Daily]", "ftp/5_Industry_Portfolios_daily_CSV.zip",
    "factors_ff_10_industry_portfolios_monthly", "10 Industry Portfolios", "ftp/10_Industry_Portfolios_CSV.zip",
    "factors_ff_10_industry_portfolios_exdividends_monthly", "10 Industry Portfolios [ex. Dividends]", "ftp/10_Industry_Portfolios_Wout_Div_CSV.zip",
    "factors_ff_10_industry_portfolios_daily", "10 Industry Portfolios [Daily]", "ftp/10_Industry_Portfolios_daily_CSV.zip",
    "factors_ff_12_industry_portfolios_monthly", "12 Industry Portfolios", "ftp/12_Industry_Portfolios_CSV.zip",
    "factors_ff_12_industry_portfolios_exdividends_monthly", "12 Industry Portfolios [ex. Dividends]", "ftp/12_Industry_Portfolios_Wout_Div_CSV.zip",
    "factors_ff_12_industry_portfolios_daily", "12 Industry Portfolios [Daily]", "ftp/12_Industry_Portfolios_daily_CSV.zip",
    "factors_ff_17_industry_portfolios_monthly", "17 Industry Portfolios", "ftp/17_Industry_Portfolios_CSV.zip",
    "factors_ff_17_industry_portfolios_exdividends_monthly", "17 Industry Portfolios [ex. Dividends]", "ftp/17_Industry_Portfolios_Wout_Div_CSV.zip",
    "factors_ff_17_industry_portfolios_daily", "17 Industry Portfolios [Daily]", "ftp/17_Industry_Portfolios_daily_CSV.zip",
    "factors_ff_30_industry_portfolios_monthly", "30 Industry Portfolios", "ftp/30_Industry_Portfolios_CSV.zip",
    "factors_ff_30_industry_portfolios_exdividends_monthly", "30 Industry Portfolios [ex. Dividends]", "ftp/30_Industry_Portfolios_Wout_Div_CSV.zip",
    "factors_ff_30_industry_portfolios_daily", "30 Industry Portfolios [Daily]", "ftp/30_Industry_Portfolios_daily_CSV.zip",
    "factors_ff_38_industry_portfolios_monthly", "38 Industry Portfolios", "ftp/38_Industry_Portfolios_CSV.zip",
    "factors_ff_38_industry_portfolios_exdividends_monthly", "38 Industry Portfolios [ex. Dividends]", "ftp/38_Industry_Portfolios_Wout_Div_CSV.zip",
    "factors_ff_38_industry_portfolios_daily", "38 Industry Portfolios [Daily]", "ftp/38_Industry_Portfolios_daily_CSV.zip",
    "factors_ff_48_industry_portfolios_monthly", "48 Industry Portfolios", "ftp/48_Industry_Portfolios_CSV.zip",
    "factors_ff_48_industry_portfolios_exdividends_monthly", "48 Industry Portfolios [ex. Dividends]", "ftp/48_Industry_Portfolios_Wout_Div_CSV.zip",
    "factors_ff_48_industry_portfolios_daily", "48 Industry Portfolios [Daily]", "ftp/48_Industry_Portfolios_daily_CSV.zip",
    "factors_ff_49_industry_portfolios_monthly", "49 Industry Portfolios", "ftp/49_Industry_Portfolios_CSV.zip",
    "factors_ff_49_industry_portfolios_exdividends_monthly", "49 Industry Portfolios [ex. Dividends]", "ftp/49_Industry_Portfolios_Wout_Div_CSV.zip",
    "factors_ff_49_industry_portfolios_daily", "49 Industry Portfolios [Daily]", "ftp/49_Industry_Portfolios_daily_CSV.zip",
    "factors_ff_me_breakpoints_monthly", "ME Breakpoints", "ftp/ME_Breakpoints_CSV.zip",
    "factors_ff_beme_breakpoints_monthly", "BE/ME Breakpoints", "ftp/BE-ME_Breakpoints_CSV.zip",
    "factors_ff_op_breakpoints_monthly", "Operating Profitability Breakpoints", "ftp/OP_Breakpoints_CSV.zip",
    "factors_ff_inv_breakpoints_monthly", "Investment Breakpoints", "ftp/INV_Breakpoints_CSV.zip",
    "factors_ff_ep_breakpoints_monthly", "E/P Breakpoints", "ftp/E-P_Breakpoints_CSV.zip",
    "factors_ff_cfp_breakpoints_monthly", "CF/P Breakpoints", "ftp/CF-P_Breakpoints_CSV.zip",
    "factors_ff_dp_breakpoints_monthly", "D/P Breakpoints", "ftp/D-P_Breakpoints_CSV.zip",
    "factors_ff_prior_212_return_breakpoints_monthly", "Prior (2-12) Return Breakpoints", "ftp/Prior_2-12_Breakpoints_CSV.zip",
    "factors_ff_developed_3_monthly", "Fama/French Developed 3 Factors", "ftp/Developed_3_Factors_CSV.zip",
    "factors_ff_developed_3_daily", "Fama/French Developed 3 Factors [Daily]", "ftp/Developed_3_Factors_Daily_CSV.zip",
    "factors_ff_developed_ex_us_3_monthly", "Fama/French Developed ex US 3 Factors", "ftp/Developed_ex_US_3_Factors_CSV.zip",
    "factors_ff_developed_ex_us_3_daily", "Fama/French Developed ex US 3 Factors [Daily]", "ftp/Developed_ex_US_3_Factors_Daily_CSV.zip",
    "factors_ff_european_3_monthly", "Fama/French European 3 Factors", "ftp/Europe_3_Factors_CSV.zip",
    "factors_ff_european_3_daily", "Fama/French European 3 Factors [Daily]", "ftp/Europe_3_Factors_Daily_CSV.zip",
    "factors_ff_japanese_3_monthly", "Fama/French Japanese 3 Factors", "ftp/Japan_3_Factors_CSV.zip",
    "factors_ff_japanese_3_daily", "Fama/French Japanese 3 Factors [Daily]", "ftp/Japan_3_Factors_Daily_CSV.zip",
    "factors_ff_asia_pacific_ex_japan_3_monthly", "Fama/French Asia Pacific ex Japan 3 Factors", "ftp/Asia_Pacific_ex_Japan_3_Factors_CSV.zip",
    "factors_ff_asia_pacific_ex_japan_3_daily", "Fama/French Asia Pacific ex Japan 3 Factors [Daily]", "ftp/Asia_Pacific_ex_Japan_3_Factors_Daily_CSV.zip",
    "factors_ff_north_american_3_monthly", "Fama/French North American 3 Factors", "ftp/North_America_3_Factors_CSV.zip",
    "factors_ff_north_american_3_daily", "Fama/French North American 3 Factors [Daily]", "ftp/North_America_3_Factors_Daily_CSV.zip",
    "factors_ff_developed_5_monthly", "Fama/French Developed 5 Factors", "ftp/Developed_5_Factors_CSV.zip",
    "factors_ff_developed_5_daily", "Fama/French Developed 5 Factors [Daily]", "ftp/Developed_5_Factors_Daily_CSV.zip",
    "factors_ff_developed_ex_us_5_monthly", "Fama/French Developed ex US 5 Factors", "ftp/Developed_ex_US_5_Factors_CSV.zip",
    "factors_ff_developed_ex_us_5_daily", "Fama/French Developed ex US 5 Factors [Daily]", "ftp/Developed_ex_US_5_Factors_Daily_CSV.zip",
    "factors_ff_european_5_monthly", "Fama/French European 5 Factors", "ftp/Europe_5_Factors_CSV.zip",
    "factors_ff_european_5_daily", "Fama/French European 5 Factors [Daily]", "ftp/Europe_5_Factors_Daily_CSV.zip",
    "factors_ff_japanese_5_monthly", "Fama/French Japanese 5 Factors", "ftp/Japan_5_Factors_CSV.zip",
    "factors_ff_japanese_5_daily", "Fama/French Japanese 5 Factors [Daily]", "ftp/Japan_5_Factors_Daily_CSV.zip",
    "factors_ff_asia_pacific_ex_japan_5_monthly", "Fama/French Asia Pacific ex Japan 5 Factors", "ftp/Asia_Pacific_ex_Japan_5_Factors_CSV.zip",
    "factors_ff_asia_pacific_ex_japan_5_daily", "Fama/French Asia Pacific ex Japan 5 Factors [Daily]", "ftp/Asia_Pacific_ex_Japan_5_Factors_Daily_CSV.zip",
    "factors_ff_north_american_5_monthly", "Fama/French North American 5 Factors", "ftp/North_America_5_Factors_CSV.zip",
    "factors_ff_north_american_5_daily", "Fama/French North American 5 Factors [Daily]", "ftp/North_America_5_Factors_Daily_CSV.zip",
    "factors_ff_developed_momentum_factor_monthly", "Developed Momentum Factor (Mom)", "ftp/Developed_Mom_Factor_CSV.zip",
    "factors_ff_developed_momentum_factor_daily", "Developed Momentum Factor (Mom) [Daily]", "ftp/Developed_Mom_Factor_Daily_CSV.zip",
    "factors_ff_developed_ex_us_momentum_factor_monthly", "Developed ex US Momentum Factor (Mom)", "ftp/Developed_ex_US_Mom_Factor_CSV.zip",
    "factors_ff_developed_ex_us_momentum_factor_daily", "Developed ex US Momentum Factor (Mom) [Daily]", "ftp/Developed_ex_US_Mom_Factor_Daily_CSV.zip",
    "factors_ff_european_momentum_factor_monthly", "European Momentum Factor (Mom)", "ftp/Europe_Mom_Factor_CSV.zip",
    "factors_ff_european_momentum_factor_daily", "European Momentum Factor (Mom) [Daily]", "ftp/Europe_Mom_Factor_Daily_CSV.zip",
    "factors_ff_japanese_momentum_factor_monthly", "Japanese Momentum Factor (Mom)", "ftp/Japan_Mom_Factor_CSV.zip",
    "factors_ff_japanese_momentum_factor_daily", "Japanese Momentum Factor (Mom) [Daily]", "ftp/Japan_Mom_Factor_Daily_CSV.zip",
    "factors_ff_asia_pacific_ex_japan_momentum_factor_monthly", "Asia Pacific ex Japan Momentum Factor (Mom)", "ftp/Asia_Pacific_ex_Japan_MOM_Factor_CSV.zip",
    "factors_ff_asia_pacific_ex_japan_momentum_factor_daily", "Asia Pacific ex Japan Momentum Factor (Mom) [Daily]", "ftp/Asia_Pacific_ex_Japan_MOM_Factor_Daily_CSV.zip",
    "factors_ff_north_american_momentum_factor_monthly", "North American Momentum Factor (Mom)", "ftp/North_America_Mom_Factor_CSV.zip",
    "factors_ff_north_american_momentum_factor_daily", "North American Momentum Factor (Mom) [Daily]", "ftp/North_America_Mom_Factor_Daily_CSV.zip",
    "factors_ff_6_developed_size_and_bm_2_x_3_monthly", "6 Developed Portfolios Formed on Size and Book-to-Market (2 x 3)", "ftp/Developed_6_Portfolios_ME_BE-ME_CSV.zip",
    "factors_ff_6_developed_size_and_bm_2_x_3_daily", "6 Developed Portfolios Formed on Size and Book-to-Market (2 x 3) [Daily]", "ftp/Developed_6_Portfolios_ME_BE-ME_daily_CSV.zip",
    "factors_ff_6_developed_ex_us_size_and_bm_2_x_3_monthly", "6 Developed ex US Portfolios Formed on Size and Book-to-Market (2 x 3)", "ftp/Developed_ex_US_6_Portfolios_ME_BE-ME_CSV.zip",
    "factors_ff_6_developed_ex_us_size_and_bm_2_x_3_daily", "6 Developed ex US Portfolios Formed on Size and Book-to-Market (2 x 3) [Daily]", "ftp/Developed_ex_US_6_Portfolios_ME_BE-ME_daily_CSV.zip",
    "factors_ff_6_european_size_and_bm_2_x_3_monthly", "6 European Portfolios Formed on Size and Book-to-Market (2 x 3)", "ftp/Europe_6_Portfolios_ME_BE-ME_CSV.zip",
    "factors_ff_6_european_size_and_bm_2_x_3_daily", "6 European Portfolios Formed on Size and Book-to-Market (2 x 3) [Daily]", "ftp/Europe_6_Portfolios_ME_BE-ME_daily_CSV.zip",
    "factors_ff_6_japanese_size_and_bm_2_x_3_monthly", "6 Japanese Portfolios Formed on Size and Book-to-Market (2 x 3)", "ftp/Japan_6_Portfolios_ME_BE-ME_CSV.zip",
    "factors_ff_6_japanese_size_and_bm_2_x_3_daily", "6 Japanese Portfolios Formed on Size and Book-to-Market (2 x 3) [Daily]", "ftp/Japan_6_Portfolios_ME_BE-ME_daily_CSV.zip",
    "factors_ff_6_asia_pacific_ex_japan_size_and_bm_2_x_3_monthly", "6 Asia Pacific ex Japan Portfolios Formed on Size and Book-to-Market (2 x 3)", "ftp/Asia_Pacific_ex_Japan_6_Portfolios_ME_BE-ME_CSV.zip",
    "factors_ff_6_asia_pacific_ex_japan_size_and_bm_2_x_3_daily", "6 Asia Pacific ex Japan Portfolios Formed on Size and Book-to-Market (2 x 3) [Daily]", "ftp/Asia_Pacific_ex_Japan_6_Portfolios_ME_BE-ME_daily_CSV.zip",
    "factors_ff_6_north_american_size_and_bm_2_x_3_monthly", "6 North American Portfolios Formed on Size and Book-to-Market (2 x 3)", "ftp/North_America_6_Portfolios_ME_BE-ME_CSV.zip",
    "factors_ff_6_north_american_size_and_bm_2_x_3_daily", "6 North American Portfolios Formed on Size and Book-to-Market (2 x 3) [Daily]", "ftp/North_America_6_Portfolios_ME_BE-ME_daily_CSV.zip",
    "factors_ff_25_developed_size_and_bm_5_x_5_monthly", "25 Developed Portfolios Formed on Size and Book-to-Market (5 x 5)", "ftp/Developed_25_Portfolios_ME_BE-ME_CSV.zip",
    "factors_ff_25_developed_size_and_bm_5_x_5_daily", "25 Developed Portfolios Formed on Size and Book-to-Market (5 x 5) [Daily]", "ftp/Developed_25_Portfolios_ME_BE-ME_daily_CSV.zip",
    "factors_ff_25_developed_ex_us_size_and_bm_5_x_5_monthly", "25 Developed ex US Portfolios Formed on Size and Book-to-Market (5 x 5)", "ftp/Developed_ex_US_25_Portfolios_ME_BE-ME_CSV.zip",
    "factors_ff_25_developed_ex_us_size_and_bm_5_x_5_daily", "25 Developed ex US Portfolios Formed on Size and Book-to-Market (5 x 5) [Daily]", "ftp/Developed_ex_US_25_Portfolios_ME_BE-ME_daily_CSV.zip",
    "factors_ff_25_european_size_and_bm_5_x_5_monthly", "25 European Portfolios Formed on Size and Book-to-Market (5 x 5)", "ftp/Europe_25_Portfolios_ME_BE-ME_CSV.zip",
    "factors_ff_25_european_size_and_bm_5_x_5_daily", "25 European Portfolios Formed on Size and Book-to-Market (5 x 5) [Daily]", "ftp/Europe_25_Portfolios_ME_BE-ME_daily_CSV.zip",
    "factors_ff_25_japanese_size_and_bm_5_x_5_monthly", "25 Japanese Portfolios Formed on Size and Book-to-Market (5 x 5)", "ftp/Japan_25_Portfolios_ME_BE-ME_CSV.zip",
    "factors_ff_25_japanese_size_and_bm_5_x_5_daily", "25 Japanese Portfolios Formed on Size and Book-to-Market (5 x 5) [Daily]", "ftp/Japan_25_Portfolios_ME_BE-ME_daily_CSV.zip",
    "factors_ff_25_asia_pacific_ex_japan_size_and_bm_5_x_5_monthly", "25 Asia Pacific ex Japan Portfolios Formed on Size and Book-to-Market (5 x 5)", "ftp/Asia_Pacific_ex_Japan_25_Portfolios_ME_BE-ME_CSV.zip",
    "factors_ff_25_asia_pacific_ex_japan_size_and_bm_5_x_5_daily", "25 Asia Pacific ex Japan Portfolios Formed on Size and Book-to-Market (5 x 5) [Daily]", "ftp/Asia_Pacific_ex_Japan_25_Portfolios_ME_BE-ME_daily_CSV.zip",
    "factors_ff_25_north_american_size_and_bm_5_x_5_monthly", "25 North American Portfolios Formed on Size and Book-to-Market (5 x 5)", "ftp/North_America_25_Portfolios_ME_BE-ME_CSV.zip",
    "factors_ff_25_north_american_size_and_bm_5_x_5_daily", "25 North American Portfolios Formed on Size and Book-to-Market (5 x 5) [Daily]", "ftp/North_America_25_Portfolios_ME_BE-ME_daily_CSV.zip",
    "factors_ff_6_developed_size_and_op_2_x_3_monthly", "6 Developed Portfolios Formed on Size and Operating Profitability (2 x 3)", "ftp/Developed_6_Portfolios_ME_OP_CSV.zip",
    "factors_ff_6_developed_size_and_op_2_x_3_daily", "6 Developed Portfolios Formed on Size and Operating Profitability (2 x 3) [Daily]", "ftp/Developed_6_Portfolios_ME_OP_Daily_CSV.zip",
    "factors_ff_6_developed_ex_us_size_and_op_2_x_3_monthly", "6 Developed ex US Portfolios Formed on Size and Operating Profitability (2 x 3)", "ftp/Developed_ex_US_6_Portfolios_ME_OP_CSV.zip",
    "factors_ff_6_developed_ex_us_size_and_op_2_x_3_daily", "6 Developed ex US Portfolios Formed on Size and Operating Profitability (2 x 3) [Daily]", "ftp/Developed_ex_US_6_Portfolios_ME_OP_Daily_CSV.zip",
    "factors_ff_6_european_size_and_op_2_x_3_monthly", "6 European Portfolios Formed on Size and Operating Profitability (2 x 3)", "ftp/Europe_6_Portfolios_ME_OP_CSV.zip",
    "factors_ff_6_european_size_and_op_2_x_3_daily", "6 European Portfolios Formed on Size and Operating Profitability (2 x 3) [Daily]", "ftp/Europe_6_Portfolios_ME_OP_Daily_CSV.zip",
    "factors_ff_6_japanese_size_and_op_2_x_3_monthly", "6 Japanese Portfolios Formed on Size and Operating Profitability (2 x 3)", "ftp/Japan_6_Portfolios_ME_OP_CSV.zip",
    "factors_ff_6_japanese_size_and_op_2_x_3_daily", "6 Japanese Portfolios Formed on Size and Operating Profitability (2 x 3) [Daily]", "ftp/Japan_6_Portfolios_ME_OP_Daily_CSV.zip",
    "factors_ff_6_asia_pacific_ex_japan_size_and_op_2_x_3_monthly", "6 Asia Pacific ex Japan Portfolios Formed on Size and Operating Profitability (2 x 3)", "ftp/Asia_Pacific_ex_Japan_6_Portfolios_ME_OP_CSV.zip",
    "factors_ff_6_asia_pacific_ex_japan_size_and_op_2_x_3_daily", "6 Asia Pacific ex Japan Portfolios Formed on Size and Operating Profitability (2 x 3) [Daily]", "ftp/Asia_Pacific_ex_Japan_6_Portfolios_ME_OP_Daily_CSV.zip",
    "factors_ff_6_north_american_size_and_op_2_x_3_monthly", "6 North American Portfolios Formed on Size and Operating Profitability (2 x 3)", "ftp/North_America_6_Portfolios_ME_OP_CSV.zip",
    "factors_ff_6_north_american_size_and_op_2_x_3_daily", "6 North American Portfolios Formed on Size and Operating Profitability (2 x 3) [Daily]", "ftp/North_America_6_Portfolios_ME_OP_Daily_CSV.zip",
    "factors_ff_25_developed_size_and_op_5_x_5_monthly", "25 Developed Portfolios Formed on Size and Operating Profitability (5 x 5)", "ftp/Developed_25_Portfolios_ME_OP_CSV.zip",
    "factors_ff_25_developed_size_and_op_5_x_5_daily", "25 Developed Portfolios Formed on Size and Operating Profitability (5 x 5) [Daily]", "ftp/Developed_25_Portfolios_ME_OP_Daily_CSV.zip",
    "factors_ff_25_developed_ex_us_size_and_op_5_x_5_monthly", "25 Developed ex US Portfolios Formed on Size and Operating Profitability (5 x 5)", "ftp/Developed_ex_US_25_Portfolios_ME_OP_CSV.zip",
    "factors_ff_25_developed_ex_us_size_and_op_5_x_5_daily", "25 Developed ex US Portfolios Formed on Size and Operating Profitability (5 x 5) [Daily]", "ftp/Developed_ex_US_25_Portfolios_ME_OP_Daily_CSV.zip",
    "factors_ff_25_european_size_and_op_5_x_5_monthly", "25 European Portfolios Formed on Size and Operating Profitability (5 x 5)", "ftp/Europe_25_Portfolios_ME_OP_CSV.zip",
    "factors_ff_25_european_size_and_op_5_x_5_daily", "25 European Portfolios Formed on Size and Operating Profitability (5 x 5) [Daily]", "ftp/Europe_25_Portfolios_ME_OP_Daily_CSV.zip",
    "factors_ff_25_japanese_size_and_op_5_x_5_monthly", "25 Japanese Portfolios Formed on Size and Operating Profitability (5 x 5)", "ftp/Japan_25_Portfolios_ME_OP_CSV.zip",
    "factors_ff_25_japanese_size_and_op_5_x_5_daily", "25 Japanese Portfolios Formed on Size and Operating Profitability (5 x 5) [Daily]", "ftp/Japan_25_Portfolios_ME_OP_Daily_CSV.zip",
    "factors_ff_25_asia_pacific_ex_japan_size_and_op_5_x_5_monthly", "25 Asia Pacific ex Japan Portfolios Formed on Size and Operating Profitability (5 x 5)", "ftp/Asia_Pacific_ex_Japan_25_Portfolios_ME_OP_CSV.zip",
    "factors_ff_25_asia_pacific_ex_japan_size_and_op_5_x_5_daily", "25 Asia Pacific ex Japan Portfolios Formed on Size and Operating Profitability (5 x 5) [Daily]", "ftp/Asia_Pacific_ex_Japan_25_Portfolios_ME_OP_Daily_CSV.zip",
    "factors_ff_25_north_american_size_and_op_5_x_5_monthly", "25 North American Portfolios Formed on Size and Operating Profitability (5 x 5)", "ftp/North_America_25_Portfolios_ME_OP_CSV.zip",
    "factors_ff_25_north_american_size_and_op_5_x_5_daily", "25 North American Portfolios Formed on Size and Operating Profitability (5 x 5) [Daily]", "ftp/North_America_25_Portfolios_ME_OP_Daily_CSV.zip",
    "factors_ff_6_developed_size_and_inv_2_x_3_monthly", "6 Developed Portfolios Formed on Size and Investment (2 x 3)", "ftp/Developed_6_Portfolios_ME_INV_CSV.zip",
    "factors_ff_6_developed_size_and_inv_2_x_3_daily", "6 Developed Portfolios Formed on Size and Investment (2 x 3) [Daily]", "ftp/Developed_6_Portfolios_ME_INV_Daily_CSV.zip",
    "factors_ff_6_developed_ex_us_size_and_inv_2_x_3_monthly", "6 Developed ex US Portfolios Formed on Size and Investment (2 x 3)", "ftp/Developed_ex_US_6_Portfolios_ME_INV_CSV.zip",
    "factors_ff_6_developed_ex_us_size_and_inv_2_x_3_daily", "6 Developed ex US Portfolios Formed on Size and Investment (2 x 3) [Daily]", "ftp/Developed_ex_US_6_Portfolios_ME_INV_Daily_CSV.zip",
    "factors_ff_6_european_size_and_inv_2_x_3_monthly", "6 European Portfolios Formed on Size and Investment (2 x 3)", "ftp/Europe_6_Portfolios_ME_INV_CSV.zip",
    "factors_ff_6_european_size_and_inv_2_x_3_daily", "6 European Portfolios Formed on Size and Investment (2 x 3) [Daily]", "ftp/Europe_6_Portfolios_ME_INV_Daily_CSV.zip",
    "factors_ff_6_japanese_size_and_inv_2_x_3_monthly", "6 Japanese Portfolios Formed on Size and Investment (2 x 3)", "ftp/Japan_6_Portfolios_ME_INV_CSV.zip",
    "factors_ff_6_japanese_size_and_inv_2_x_3_daily", "6 Japanese Portfolios Formed on Size and Investment (2 x 3) [Daily]", "ftp/Japan_6_Portfolios_ME_INV_Daily_CSV.zip",
    "factors_ff_6_asia_pacific_ex_japan_size_and_inv_2_x_3_monthly", "6 Asia Pacific ex Japan Portfolios Formed on Size and Investment (2 x 3)", "ftp/Asia_Pacific_ex_Japan_6_Portfolios_ME_INV_CSV.zip",
    "factors_ff_6_asia_pacific_ex_japan_size_and_inv_2_x_3_daily", "6 Asia Pacific ex Japan Portfolios Formed on Size and Investment (2 x 3) [Daily]", "ftp/Asia_Pacific_ex_Japan_6_Portfolios_ME_INV_Daily_CSV.zip",
    "factors_ff_6_north_american_size_and_inv_2_x_3_monthly", "6 North American Portfolios Formed on Size and Investment (2 x 3)", "ftp/North_America_6_Portfolios_ME_INV_CSV.zip",
    "factors_ff_6_north_american_size_and_inv_2_x_3_daily", "6 North American Portfolios Formed on Size and Investment (2 x 3) [Daily]", "ftp/North_America_6_Portfolios_ME_INV_Daily_CSV.zip",
    "factors_ff_25_developed_size_and_inv_5_x_5_monthly", "25 Developed Portfolios Formed on Size and Investment (5 x 5)", "ftp/Developed_25_Portfolios_ME_INV_CSV.zip",
    "factors_ff_25_developed_size_and_inv_5_x_5_daily", "25 Developed Portfolios Formed on Size and Investment (5 x 5) [Daily]", "ftp/Developed_25_Portfolios_ME_INV_Daily_CSV.zip",
    "factors_ff_25_developed_ex_us_size_and_inv_5_x_5_monthly", "25 Developed ex US Portfolios Formed on Size and Investment (5 x 5)", "ftp/Developed_ex_US_25_Portfolios_ME_INV_CSV.zip",
    "factors_ff_25_developed_ex_us_size_and_inv_5_x_5_daily", "25 Developed ex US Portfolios Formed on Size and Investment (5 x 5) [Daily]", "ftp/Developed_ex_US_25_Portfolios_ME_INV_Daily_CSV.zip",
    "factors_ff_25_european_size_and_inv_5_x_5_monthly", "25 European Portfolios Formed on Size and Investment (5 x 5)", "ftp/Europe_25_Portfolios_ME_INV_CSV.zip",
    "factors_ff_25_european_size_and_inv_5_x_5_daily", "25 European Portfolios Formed on Size and Investment (5 x 5) [Daily]", "ftp/Europe_25_Portfolios_ME_INV_Daily_CSV.zip",
    "factors_ff_25_japanese_size_and_inv_5_x_5_monthly", "25 Japanese Portfolios Formed on Size and Investment (5 x 5)", "ftp/Japan_25_Portfolios_ME_INV_CSV.zip",
    "factors_ff_25_japanese_size_and_inv_5_x_5_daily", "25 Japanese Portfolios Formed on Size and Investment (5 x 5) [Daily]", "ftp/Japan_25_Portfolios_ME_INV_Daily_CSV.zip",
    "factors_ff_25_asia_pacific_ex_japan_size_and_inv_5_x_5_monthly", "25 Asia Pacific ex Japan Portfolios Formed on Size and Investment (5 x 5)", "ftp/Asia_Pacific_ex_Japan_25_Portfolios_ME_INV_CSV.zip",
    "factors_ff_25_asia_pacific_ex_japan_size_and_inv_5_x_5_daily", "25 Asia Pacific ex Japan Portfolios Formed on Size and Investment (5 x 5) [Daily]", "ftp/Asia_Pacific_ex_Japan_25_Portfolios_ME_INV_Daily_CSV.zip",
    "factors_ff_25_north_american_size_and_inv_5_x_5_monthly", "25 North American Portfolios Formed on Size and Investment (5 x 5)", "ftp/North_America_25_Portfolios_ME_INV_CSV.zip",
    "factors_ff_25_north_american_size_and_inv_5_x_5_daily", "25 North American Portfolios Formed on Size and Investment (5 x 5) [Daily]", "ftp/North_America_25_Portfolios_ME_INV_Daily_CSV.zip",
    "factors_ff_6_developed_size_and_momentum_2_x_3_monthly", "6 Developed Portfolios Formed on Size and Momentum (2 x 3)", "ftp/Developed_6_Portfolios_ME_Prior_12_2_CSV.zip",
    "factors_ff_6_developed_size_and_momentum_2_x_3_daily", "6 Developed Portfolios Formed on Size and Momentum (2 x 3) [Daily]", "ftp/Developed_6_Portfolios_ME_Prior_250_20_daily_CSV.zip",
    "factors_ff_6_developed_ex_us_size_and_momentum_2_x_3_monthly", "6 Developed ex US Portfolios Formed on Size and Momentum (2 x 3)", "ftp/Developed_ex_US_6_Portfolios_ME_Prior_12_2_CSV.zip",
    "factors_ff_6_developed_ex_us_size_and_momentum_2_x_3_daily", "6 Developed ex US Portfolios Formed on Size and Momentum (2 x 3) [Daily]", "ftp/Developed_ex_US_6_Portfolios_ME_Prior_250_20_daily_CSV.zip",
    "factors_ff_6_european_size_and_momentum_2_x_3_monthly", "6 European Portfolios Formed on Size and Momentum (2 x 3)", "ftp/Europe_6_Portfolios_ME_Prior_12_2_CSV.zip",
    "factors_ff_6_european_size_and_momentum_2_x_3_daily", "6 European Portfolios Formed on Size and Momentum (2 x 3) [Daily]", "ftp/Europe_6_Portfolios_ME_Prior_250_20_daily_CSV.zip",
    "factors_ff_6_japanese_size_and_momentum_2_x_3_monthly", "6 Japanese Portfolios Formed on Size and Momentum (2 x 3)", "ftp/Japan_6_Portfolios_ME_Prior_12_2_CSV.zip",
    "factors_ff_6_japanese_size_and_momentum_2_x_3_daily", "6 Japanese Portfolios Formed on Size and Momentum (2 x 3) [Daily]", "ftp/Japan_6_Portfolios_ME_Prior_250_20_daily_CSV.zip",
    "factors_ff_6_asia_pacific_ex_japan_size_and_momentum_2_x_3_monthly", "6 Asia Pacific ex Japan Portfolios Formed on Size and Momentum (2 x 3)", "ftp/Asia_Pacific_ex_Japan_6_Portfolios_ME_Prior_12_2_CSV.zip",
    "factors_ff_6_asia_pacific_ex_japan_size_and_momentum_2_x_3_daily", "6 Asia Pacific ex Japan Portfolios Formed on Size and Momentum (2 x 3) [Daily]", "ftp/Asia_Pacific_ex_Japan_6_Portfolios_ME_Prior_250_20_daily_CSV.zip",
    "factors_ff_6_north_american_size_and_momentum_2_x_3_monthly", "6 North American Portfolios Formed on Size and Momentum (2 x 3)", "ftp/North_America_6_Portfolios_ME_Prior_12_2_CSV.zip",
    "factors_ff_6_north_american_size_and_momentum_2_x_3_daily", "6 North American Portfolios Formed on Size and Momentum (2 x 3) [Daily]", "ftp/North_America_6_Portfolios_ME_Prior_250_20_daily_CSV.zip",
    "factors_ff_25_developed_size_and_momentum_5_x_5_monthly", "25 Developed Portfolios Formed on Size and Momentum (5 x 5)", "ftp/Developed_25_Portfolios_ME_Prior_12_2_CSV.zip",
    "factors_ff_25_developed_size_and_momentum_5_x_5_daily", "25 Developed Portfolios Formed on Size and Momentum (5 x 5) [Daily]", "ftp/Developed_25_Portfolios_ME_Prior_250_20_daily_CSV.zip",
    "factors_ff_25_developed_ex_us_size_and_momentum_5_x_5_monthly", "25 Developed ex US Portfolios Formed on Size and Momentum (5 x 5)", "ftp/Developed_ex_US_25_Portfolios_ME_Prior_12_2_CSV.zip",
    "factors_ff_25_developed_ex_us_size_and_momentum_5_x_5_daily", "25 Developed ex US Portfolios Formed on Size and Momentum (5 x 5) [Daily]", "ftp/Developed_ex_US_25_Portfolios_ME_Prior_250_20_daily_CSV.zip",
    "factors_ff_25_european_size_and_momentum_5_x_5_monthly", "25 European Portfolios Formed on Size and Momentum (5 x 5)", "ftp/Europe_25_Portfolios_ME_Prior_12_2_CSV.zip",
    "factors_ff_25_european_size_and_momentum_5_x_5_daily", "25 European Portfolios Formed on Size and Momentum (5 x 5) [Daily]", "ftp/Europe_25_Portfolios_ME_Prior_250_20_daily_CSV.zip",
    "factors_ff_25_japanese_size_and_momentum_5_x_5_monthly", "25 Japanese Portfolios Formed on Size and Momentum (5 x 5)", "ftp/Japan_25_Portfolios_ME_Prior_12_2_CSV.zip",
    "factors_ff_25_japanese_size_and_momentum_5_x_5_daily", "25 Japanese Portfolios Formed on Size and Momentum (5 x 5) [Daily]", "ftp/Japan_25_Portfolios_ME_Prior_250_20_daily_CSV.zip",
    "factors_ff_25_asia_pacific_ex_japan_size_and_momentum_5_x_5_monthly", "25 Asia Pacific ex Japan Portfolios Formed on Size and Momentum (5 x 5)", "ftp/Asia_Pacific_ex_Japan_25_Portfolios_ME_Prior_12_2_CSV.zip",
    "factors_ff_25_asia_pacific_ex_japan_size_and_momentum_5_x_5_daily", "25 Asia Pacific ex Japan Portfolios Formed on Size and Momentum (5 x 5) [Daily]", "ftp/Asia_Pacific_ex_Japan_25_Portfolios_ME_Prior_250_20_daily_CSV.zip",
    "factors_ff_25_north_american_size_and_momentum_5_x_5_monthly", "25 North American Portfolios Formed on Size and Momentum (5 x 5)", "ftp/North_America_25_Portfolios_ME_Prior_12_2_CSV.zip",
    "factors_ff_25_north_american_size_and_momentum_5_x_5_daily", "25 North American Portfolios Formed on Size and Momentum (5 x 5) [Daily]", "ftp/North_America_25_Portfolios_ME_Prior_250_20_daily_CSV.zip",
    "factors_ff_32_developed_size_bm_and_op_2_x_4_x_4_monthly", "32 Developed Portfolios Formed on Size, Book-to-Market, and Operating Profitability (2 x 4 x 4)", "ftp/Developed_32_Portfolios_ME_BE-ME_OP_2x4x4_CSV.zip",
    "factors_ff_32_developed_ex_us_size_bm_and_op_2_x_4_x_4_monthly", "32 Developed ex US Portfolios Formed on Size, Book-to-Market, and Operating Profitability (2 x 4 x 4)", "ftp/Developed_ex_US_32_Portfolios_ME_BE-ME_OP_2x4x4_CSV.zip",
    "factors_ff_32_european_size_bm_and_op_2_x_4_x_4_monthly", "32 European Portfolios Formed on Size, Book-to-Market, and Operating Profitability (2 x 4 x 4)", "ftp/Europe_32_Portfolios_ME_BE-ME_OP_2x4x4_CSV.zip",
    "factors_ff_32_japanese_size_bm_and_op_2_x_4_x_4_monthly", "32 Japanese Portfolios Formed on Size, Book-to-Market, and Operating Profitability (2 x 4 x 4)", "ftp/Japan_32_Portfolios_ME_BE-ME_OP_2x4x4_CSV.zip",
    "factors_ff_32_asia_pacific_ex_japan_size_bm_and_op_2_x_4_x_4_monthly", "32 Asia Pacific ex Japan Portfolios Formed on Size, Book-to-Market, and Operating Profitability (2 x 4 x 4)", "ftp/Asia_Pacific_ex_Japan_32_Portfolios_ME_BE-ME_OP_2x4x4_CSV.zip",
    "factors_ff_32_north_american_size_bm_and_op_2_x_4_x_4_monthly", "32 North American Portfolios Formed on Size, Book-to-Market, and Operating Profitability (2 x 4 x 4)", "ftp/North_America_32_Portfolios_ME_BE-ME_OP_2x4x4_CSV.zip",
    "factors_ff_32_developed_size_bm_and_inv_2_x_4_x_4_monthly", "32 Developed Portfolios Formed on Size, Book-to-Market, and Investment (2 x 4 x 4)", "ftp/Developed_32_Portfolios_ME_BE-ME_INV(TA)_2x4x4_CSV.zip",
    "factors_ff_32_developed_ex_us_size_bm_and_inv_2_x_4_x_4_monthly", "32 Developed ex US Portfolios Formed on Size, Book-to-Market, and Investment (2 x 4 x 4)", "ftp/Developed_ex_US_32_Portfolios_ME_BE-ME_INV(TA)_2x4x4_CSV.zip",
    "factors_ff_32_european_size_bm_and_inv_2_x_4_x_4_monthly", "32 European Portfolios Formed on Size, Book-to-Market, and Investment (2 x 4 x 4)", "ftp/Europe_32_Portfolios_ME_BE-ME_INV(TA)_2x4x4_CSV.zip",
    "factors_ff_32_japanese_size_bm_and_inv_2_x_4_x_4_monthly", "32 Japanese Portfolios Formed on Size, Book-to-Market, and Investment (2 x 4 x 4)", "ftp/Japan_32_Portfolios_ME_BE-ME_INV(TA)_2x4x4_CSV.zip",
    "factors_ff_32_asia_pacific_ex_japan_size_bm_and_inv_2_x_4_x_4_monthly", "32 Asia Pacific ex Japan Portfolios Formed on Size, Book-to-Market, and Investment (2 x 4 x 4)", "ftp/Asia_Pacific_ex_Japan_32_Portfolios_ME_BE-ME_INV(TA)_2x4x4_CSV.zip",
    "factors_ff_32_north_american_size_bm_and_inv_2_x_4_x_4_monthly", "32 North American Portfolios Formed on Size, Book-to-Market, and Investment (2 x 4 x 4)", "ftp/North_America_32_Portfolios_ME_BE-ME_INV(TA)_2x4x4_CSV.zip",
    "factors_ff_32_developed_size_op_and_inv_2_x_4_x_4_monthly", "32 Developed Portfolios Formed on Size, Operating Profitability, and Investment (2 x 4 x 4)", "ftp/Developed_32_Portfolios_ME_INV(TA)_OP_2x4x4_CSV.zip",
    "factors_ff_32_developed_ex_us_size_op_and_inv_2_x_4_x_4_monthly", "32 Developed ex US Portfolios Formed on Size, Operating Profitability, and Investment (2 x 4 x 4)", "ftp/Developed_ex_US_32_Portfolios_ME_INV(TA)_OP_2x4x4_CSV.zip",
    "factors_ff_32_european_size_op_and_inv_2_x_4_x_4_monthly", "32 European Portfolios Formed on Size, Operating Profitability, and Investment (2 x 4 x 4)", "ftp/Europe_32_Portfolios_ME_INV(TA)_OP_2x4x4_CSV.zip",
    "factors_ff_32_japanese_size_op_and_inv_2_x_4_x_4_monthly", "32 Japanese Portfolios Formed on Size, Operating Profitability, and Investment (2 x 4 x 4)", "ftp/Japan_32_Portfolios_ME_INV(TA)_OP_2x4x4_CSV.zip",
    "factors_ff_32_asia_pacific_ex_japan_size_op_and_inv_2_x_4_x_4_monthly", "32 Asia Pacific ex Japan Portfolios Formed on Size, Operating Profitability, and Investment (2 x 4 x 4)", "ftp/Asia_Pacific_ex_Japan_32_Portfolios_ME_INV(TA)_OP_2x4x4_CSV.zip",
    "factors_ff_32_north_american_size_op_and_inv_2_x_4_x_4_monthly", "32 North American Portfolios Formed on Size, Operating Profitability, and Investment (2 x 4 x 4)", "ftp/North_America_32_Portfolios_ME_INV(TA)_OP_2x4x4_CSV.zip",
    "factors_ff_emerging_5_monthly", "Fama/French Emerging 5 Factors", "ftp/Emerging_5_Factors_CSV.zip",
    "factors_ff_emerging_momentum_factor_monthly", "Emerging Momentum Factor (Mom)", "ftp/Emerging_MOM_Factor_CSV.zip",
    "factors_ff_6_emerging_market_size_and_bm_2_x_3_monthly", "6 Emerging Market Portfolios Formed on Size and Book-to-Market (2 x 3)", "ftp/Emerging_Markets_6_Portfolios_ME_BE-ME_CSV.zip",
    "factors_ff_6_emerging_market_size_and_op_2_x_3_monthly", "6 Emerging Market Portfolios Formed on Size and Operating Profitability (2 x 3)", "ftp/Emerging_Markets_6_Portfolios_ME_OP_CSV.zip",
    "factors_ff_6_emerging_market_size_and_inv_2_x_3_monthly", "6 Emerging Market Portfolios Formed on Size and Investment (2 x 3)", "ftp/Emerging_Markets_6_Portfolios_ME_INV_CSV.zip",
    "factors_ff_6_emerging_market_size_and_momentum_2_x_3_monthly", "6 Emerging Market Portfolios Formed on Size and Momentum (2 x 3)", "ftp/Emerging_Markets_6_Portfolios_ME_Prior_12_2_CSV.zip",
    "factors_ff_4_emerging_market_bm__and_op_2_x_2___monthly", "4 Emerging Market Portfolios Formed on Book-to-Market  and Operating Profitability (2 x 2)  ", "ftp/Emerging_Markets_4_Portfolios_BE-ME_OP_CSV.zip",
    "factors_ff_4_emerging_market_op_and_inv_2_x_2_monthly", "4 Emerging Market Portfolios Formed on Operating Profitability and Investment (2 x 2)", "ftp/Emerging_Markets_4_Portfolios_OP_INV_CSV.zip",
    "factors_ff_4_emerging_market_bm_and_inv_2_x_2_monthly", "4 Emerging Market Portfolios Formed on Book-to-Market and Investment (2 x 2)", "ftp/Emerging_Markets_4_Portfolios_BE-ME_INV_CSV.zip"
  ) |>
    dplyr::mutate(domain = "Fama-French")
  # nolint end
}

#' List Supported Legacy Fama-French Datasets
#'
#' Returns a tibble with the legacy names of initially supported
#' Fama-French datasets, including their names and frequencies (daily,
#' weekly, monthly). Each dataset type is associated with a specific Fama-French
#' model (e.g., 3 factors, 5 factors). Additionally, it annotates each dataset
#' with the domain "Fama-French". Not included in the exported
#' `list_supported_datasets()` function.
#'
#' @returns A tibble with columns: `type` (the type of dataset), `dataset_name`
#'   (a descriptive name of the dataset), `file_url` (the path of the source
#'   ZIP relative to Kenneth French's data library), and `domain` (the domain
#'   to which the dataset belongs, always "Fama-French").
#'
#' @family utility functions
list_supported_datasets_ff_legacy <- function() {
  # nolint start
  tribble(
    ~type, ~dataset_name, ~file_url,
    "factors_ff3_daily", "Fama/French 3 Factors [Daily]", "ftp/F-F_Research_Data_Factors_daily_CSV.zip",
    "factors_ff3_weekly", "Fama/French 3 Factors [Weekly]", "ftp/F-F_Research_Data_Factors_weekly_CSV.zip",
    "factors_ff3_monthly", "Fama/French 3 Factors", "ftp/F-F_Research_Data_Factors_CSV.zip",
    "factors_ff5_daily", "Fama/French 5 Factors (2x3) [Daily]", "ftp/F-F_Research_Data_5_Factors_2x3_daily_CSV.zip",
    "factors_ff5_monthly", "Fama/French 5 Factors (2x3)", "ftp/F-F_Research_Data_5_Factors_2x3_CSV.zip",
    "factors_ff_industry_5_monthly", "5 Industry Portfolios", "ftp/5_Industry_Portfolios_CSV.zip",
    "factors_ff_industry_5_daily", "5 Industry Portfolios [Daily]", "ftp/5_Industry_Portfolios_daily_CSV.zip",
    "factors_ff_industry_10_monthly", "10 Industry Portfolios", "ftp/10_Industry_Portfolios_CSV.zip",
    "factors_ff_industry_10_daily", "10 Industry Portfolios [Daily]", "ftp/10_Industry_Portfolios_daily_CSV.zip",
    "factors_ff_industry_30_monthly", "30 Industry Portfolios", "ftp/30_Industry_Portfolios_CSV.zip",
    "factors_ff_industry_30_daily", "30 Industry Portfolios [Daily]", "ftp/30_Industry_Portfolios_daily_CSV.zip",
    "factors_ff_industry_38_monthly", "38 Industry Portfolios", "ftp/38_Industry_Portfolios_CSV.zip",
    "factors_ff_industry_38_daily", "38 Industry Portfolios [Daily]", "ftp/38_Industry_Portfolios_daily_CSV.zip",
    "factors_ff_industry_48_monthly", "48 Industry Portfolios", "ftp/48_Industry_Portfolios_CSV.zip",
    "factors_ff_industry_48_daily", "48 Industry Portfolios [Daily]", "ftp/48_Industry_Portfolios_daily_CSV.zip",
    "factors_ff_industry_49_monthly", "49 Industry Portfolios", "ftp/49_Industry_Portfolios_CSV.zip",
    "factors_ff_industry_49_daily", "49 Industry Portfolios [Daily]", "ftp/49_Industry_Portfolios_daily_CSV.zip"
  ) |>
    dplyr::mutate(domain = "Fama-French")
  # nolint end
}

#' List Supported Global Q Datasets
#'
#' Returns a tibble with the supported Global Q datasets, including
#' their names and frequencies (daily, weekly, weekly week-to-week, monthly,
#' quarterly, annual). Each dataset type is associated with the Global Q
#' model, specifically the q5 factors model for the year 2023. Additionally,
#' it annotates each dataset with the domain "Global Q".
#'
#' @returns A tibble with columns: `type` (the type of dataset),
#'   `dataset_name` (the file name of the dataset), and `domain` (the
#'   domain to which the dataset belongs, always "Global Q").
#'
list_supported_datasets_q <- function() {
  # nolint start
  tribble(
    ~type                        ,
    ~dataset_name                ,
    "factors_q5_daily"           ,
    "q5_factors_daily_2024"      ,
    "factors_q5_weekly"          ,
    "q5_factors_weekly_2024"     ,
    "factors_q5_weekly_w2w"      ,
    "q5_factors_weekly_w2w_2024" ,
    "factors_q5_monthly"         ,
    "q5_factors_monthly_2024"    ,
    "factors_q5_quarterly"       ,
    "q5_factors_quarterly_2024"  ,
    "factors_q5_annual"          ,
    "q5_factors_annual_2024"
  ) |>
    dplyr::mutate(domain = "Global Q")
  # nolint end
}

#' List Supported Macro Predictor Datasets
#'
#' Returns a tibble with the supported macro predictor datasets
#' provided by Goyal-Welch, including their frequencies (monthly, quarterly,
#' annual). All datasets reference the same source file,
#' "PredictorData2022.xlsx" for the year 2022. Additionally, it annotates
#' each dataset with the domain "Goyal-Welch".
#'
#' @returns A tibble with columns: `type` (the type of dataset),
#'   `dataset_name` (the file name of the dataset, which is the same for
#'   all types), and `domain` (the domain to which the dataset belongs,
#'   always "Goyal-Welch").
#'
#' @family utility functions
list_supported_datasets_macro_predictors <- function() {
  # nolint start
  tribble(
    ~type                        ,
    ~dataset_name                ,
    "macro_predictors_monthly"   ,
    "PredictorData2022.xlsx"     ,
    "macro_predictors_quarterly" ,
    "PredictorData2022.xlsx"     ,
    "macro_predictors_annual"    ,
    "PredictorData2022.xlsx"     ,
  ) |>
    dplyr::mutate(domain = "Goyal-Welch")
  # nolint end
}

#' List Supported WRDS Datasets
#'
#' Returns a tibble with the supported datasets provided via WRDS.
#' Additionally, it annotates each dataset with the domain "WRDS".
#'
#' @returns A tibble with columns: `type` (the type of dataset),
#'   `dataset_name` (the file name of the dataset), and `domain` (the
#'   domain to which the dataset belongs, always "WRDS").
#'
#' @family utility functions
list_supported_datasets_wrds <- function() {
  # nolint start
  tribble(
    ~type                                           ,
    ~dataset_name                                   ,
    "wrds_crsp_monthly"                             ,
    "crsp.msf, crsp.msenames, crsp.msedelist"       ,
    "wrds_crsp_daily"                               ,
    "crsp.dsf, crsp.msenames, crsp.msedelist"       ,
    "wrds_compustat_annual"                         ,
    "comp.funda"                                    ,
    "wrds_compustat_quarterly"                      ,
    "comp.fundq"                                    ,
    "wrds_ccm_links"                                ,
    "crsp.ccmxpf_linktable"                         ,
    "wrds_fisd"                                     ,
    "fisd.fisd_mergedissue, fisd.fisd_mergedissuer" ,
    "wrds_trace_enhanced"                           ,
    "trace.trace_enhanced"
  ) |>
    dplyr::mutate(domain = "WRDS")
  # nolint end
}

#' List Supported Pseudo WRDS Datasets
#'
#' Returns a tibble with the supported pseudo WRDS datasets generated by
#' `download_data(domain = "Pseudo Data", ...)`. Annotated with the domain
#' "Pseudo Data".
#'
#' @returns A tibble with columns: `type`, `dataset_name`, and `domain`.
#'
#' @family utility functions
list_supported_datasets_pseudo <- function() {
  tribble(
    ~type,
    ~dataset_name,
    "crsp_monthly",
    "pseudo crsp_monthly",
    "crsp_daily",
    "pseudo crsp_daily",
    "compustat_annual",
    "pseudo compustat_annual",
    "compustat_quarterly",
    "pseudo compustat_quarterly",
    "ccm_links",
    "pseudo ccm_links"
  ) |>
    dplyr::mutate(domain = "Pseudo Data")
}

#' List Supported Other Datasets
#'
#' Returns a tibble listing the supported other datasets and their
#' corresponding dataset names.
#'
#' @returns A tibble with columns: `type` (the type of dataset), `dataset_name`
#'   (the name of the data source), and `domain` (the domain to which the
#'   dataset belongs).
#'
#' @family utility functions
list_supported_datasets_other <- function() {
  tibble(
    "type" = c(
      "stock_prices",
      "constituents",
      "fred",
      "osap",
      "jkp",
      "risk_free",
      "high_frequency_sp500",
      "factor_library",
      "factor_library_grid"
    ),
    "dataset_name" = c(
      "YahooFinance",
      "various",
      "various",
      "Open Source Asset Pricing",
      "Global Factor Data",
      "Risk-Free Rate",
      "High Frequency S&P 500",
      "Factor Library",
      "Factor Library Grid"
    ),
    "domain" = c(
      "Stock Prices",
      "Index Constituents",
      "FRED",
      "Open Source Asset Pricing",
      "Global Factor Data",
      "Tidy Finance",
      "Tidy Finance",
      "Tidy Finance",
      "Tidy Finance"
    )
  )
}

#' List All Supported Datasets
#'
#' Aggregates and returns a comprehensive tibble of all supported datasets
#' from different domains. It includes various datasets across different
#' frequencies (daily, weekly, monthly, quarterly, annual) and models
#' (e.g., q5 factors, Fama-French 3 and 5 factors, macro predictors).
#'
#' @param domain A character vector to filter for domain-specific datasets
#'   (e.g., c("WRDS", "Fama-French")).
#' @param as_vector Logical indicating whether datasets should be returned
#'   as a character vector instead of a data frame.
#'
#' @returns A tibble aggregating all supported datasets with columns:
#'   `type` (the type of dataset), `dataset_name` (a descriptive name or
#'   file name of the dataset), and `domain` (the domain to which the
#'   dataset belongs, e.g., "Global Q", "Fama-French", "Goyal-Welch").
#'
#' @family utility functions
#' @export
#'
#' @examples
#' # List all supported datasets as a data frame
#' list_supported_datasets()
#'
#' # Filter by domain
#' list_supported_datasets(domain = "WRDS")
#'
#' # List supported datasets as a vector
#' list_supported_datasets(as_vector = TRUE)
list_supported_datasets <- function(domain = NULL, as_vector = FALSE) {
  supported_datasets <- dplyr::bind_rows(
    list_supported_datasets_q(),
    list_supported_datasets_ff(),
    list_supported_datasets_macro_predictors(),
    list_supported_datasets_wrds(),
    list_supported_datasets_pseudo(),
    list_supported_datasets_other()
  ) |>
    dplyr::select("type", "dataset_name", "domain")
  if (!is.null(domain)) {
    filter_domains <- domain
    supported_datasets <- supported_datasets |>
      dplyr::filter(.data$domain %in% filter_domains)
  }
  if (as_vector) {
    supported_datasets$type
  } else {
    supported_datasets
  }
}

#' @rdname list_supported_datasets
#' @description
#' `list_supported_types()` is a soft-deprecated alias for
#' `list_supported_datasets()`.
#' @export
list_supported_types <- function(domain = NULL, as_vector = FALSE) {
  lifecycle::deprecate_soft(
    when = "0.5.1",
    what = "list_supported_types()",
    with = "list_supported_datasets()"
  )
  list_supported_datasets(domain = domain, as_vector = as_vector)
}
