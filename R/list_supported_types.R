# Define types ------------------------------------------------------------
#' List Supported Fama-French Dataset Types
#'
#' This function returns a tibble with the supported Fama-French dataset types,
#' including their names and frequencies (daily, weekly, monthly). Each dataset
#' type is associated with a specific Fama-French model (e.g., 3 factors, 5
#' factors). Additionally, it annotates each dataset with the domain
#' "Fama-French".
#'
#' @return A tibble with columns: `type` (the type of dataset), `dataset_name`
#'   (a descriptive name of the dataset), and `domain` (the domain to which the
#'   dataset belongs, always "Fama-French").
#'
#' @importFrom tibble tribble
#'
list_supported_types_ff <- function() {

  # curated_list <- tribble(
  #   ~type, ~dataset_name,
  #   "factors_ff3_daily", "Fama/French 3 Factors [Daily]",
  #   "factors_ff3_weekly", "Fama/French 3 Factors [Weekly]",
  #   "factors_ff3_monthly", "Fama/French 3 Factors",
  #   "factors_ff5_daily", "Fama/French 5 Factors (2x3) [Daily]",
  #   "factors_ff5_monthly", "Fama/French 5 Factors (2x3)",
  #   "factors_ff_industry_5_monthly", "5 Industry Portfolios",
  #   "factors_ff_industry_5_daily", "5 Industry Portfolios [Daily]",
  #   "factors_ff_industry_10_monthly", "10 Industry Portfolios",
  #   "factors_ff_industry_10_daily", "10 Industry Portfolios [Daily]",
  #   "factors_ff_industry_30_monthly", "30 Industry Portfolios",
  #   "factors_ff_industry_30_daily", "30 Industry Portfolios [Daily]",
  #   "factors_ff_industry_38_monthly", "38 Industry Portfolios",
  #   "factors_ff_industry_38_daily", "38 Industry Portfolios [Daily]",
  #   "factors_ff_industry_48_monthly", "48 Industry Portfolios",
  #   "factors_ff_industry_48_daily", "48 Industry Portfolios [Daily]",
  #   "factors_ff_industry_49_monthly", "49 Industry Portfolios",
  #   "factors_ff_industry_49_daily", "49 Industry Portfolios [Daily]"
  # ) |>
  #   mutate(domain = "Fama-French")
  #
  # data_sets_raw <- frenchdata::get_french_data_list()
  # data_sets <- data_sets_raw$files_list |>
  #   transmute(type = name,
  #             dataset_name = name,
  #             domain="Fama-French") |>
  #   mutate(type = stringr::str_remove(type, "Fama/French "),
  #          type = stringr::str_remove(type, " Factors"),
  #          type = stringr::str_remove(type, "Portfolios Formed on "),
  #          type = stringr::str_remove_all(type, "[[:punct:]]"),
  #          type = stringr::str_replace_all(type, " ","_"),
  #          type = stringr::str_to_lower(type),
  #          type = paste0("factors_ff_raw_", type)) |>
  #   anti_join(curated_list, join_by(dataset_name))
  #
  # list <- bind_rows(curated_list, data_sets)
  # datapasta::dpasta(list)
  #

  tibble::tribble(
    ~type,                                                                                                 ~dataset_name,       ~domain,
    "factors_ff3_daily",                                                                               "Fama/French 3 Factors [Daily]", "Fama-French",
    "factors_ff3_weekly",                                                                              "Fama/French 3 Factors [Weekly]", "Fama-French",
    "factors_ff3_monthly",                                                                                       "Fama/French 3 Factors", "Fama-French",
    "factors_ff5_daily",                                                                         "Fama/French 5 Factors (2x3) [Daily]", "Fama-French",
    "factors_ff5_monthly",                                                                                 "Fama/French 5 Factors (2x3)", "Fama-French",
    "factors_ff_industry_5_monthly",                                                                                       "5 Industry Portfolios", "Fama-French",
    "factors_ff_industry_5_daily",                                                                               "5 Industry Portfolios [Daily]", "Fama-French",
    "factors_ff_industry_10_monthly",                                                                                      "10 Industry Portfolios", "Fama-French",
    "factors_ff_industry_10_daily",                                                                              "10 Industry Portfolios [Daily]", "Fama-French",
    "factors_ff_industry_30_monthly",                                                                                      "30 Industry Portfolios", "Fama-French",
    "factors_ff_industry_30_daily",                                                                              "30 Industry Portfolios [Daily]", "Fama-French",
    "factors_ff_industry_38_monthly",                                                                                      "38 Industry Portfolios", "Fama-French",
    "factors_ff_industry_38_daily",                                                                              "38 Industry Portfolios [Daily]", "Fama-French",
    "factors_ff_industry_48_monthly",                                                                                      "48 Industry Portfolios", "Fama-French",
    "factors_ff_industry_48_daily",                                                                              "48 Industry Portfolios [Daily]", "Fama-French",
    "factors_ff_industry_49_monthly",                                                                                      "49 Industry Portfolios", "Fama-French",
    "factors_ff_industry_49_daily",                                                                              "49 Industry Portfolios [Daily]", "Fama-French",
    "factors_ff_raw_size",                                                                                   "Portfolios Formed on Size", "Fama-French",
    "factors_ff_raw_size_exdividends",                                                                    "Portfolios Formed on Size [ex.Dividends]", "Fama-French",
    "factors_ff_raw_size_daily",                                                                           "Portfolios Formed on Size [Daily]", "Fama-French",
    "factors_ff_raw_booktomarket",                                                                         "Portfolios Formed on Book-to-Market", "Fama-French",
    "factors_ff_raw_booktomarket_ex_dividends",                                                         "Portfolios Formed on Book-to-Market [ex. Dividends]", "Fama-French",
    "factors_ff_raw_booktomarket_daily",                                                                 "Portfolios Formed on Book-to-Market [Daily]", "Fama-French",
    "factors_ff_raw_operating_profitability",                                                                "Portfolios Formed on Operating Profitability", "Fama-French",
    "factors_ff_raw_operating_profitability_ex_dividends",                                                "Portfolios Formed on Operating Profitability [ex. Dividends]", "Fama-French",
    "factors_ff_raw_operating_profitability_daily",                                                        "Portfolios Formed on Operating Profitability [Daily]", "Fama-French",
    "factors_ff_raw_investment",                                                                             "Portfolios Formed on Investment", "Fama-French",
    "factors_ff_raw_investment_ex_dividends",                                                             "Portfolios Formed on Investment [ex. Dividends]", "Fama-French",
    "factors_ff_raw_investment_daily",                                                                     "Portfolios Formed on Investment [Daily]", "Fama-French",
    "factors_ff_raw_6_size_and_booktomarket_2_x_3",                                                      "6 Portfolios Formed on Size and Book-to-Market (2 x 3)", "Fama-French",
    "factors_ff_raw_6_size_and_booktomarket_2_x_3_ex_dividends",                                      "6 Portfolios Formed on Size and Book-to-Market (2 x 3) [ex. Dividends]", "Fama-French",
    "factors_ff_raw_6_size_and_booktomarket_2_x_3_weekly",                                             "6 Portfolios Formed on Size and Book-to-Market (2 x 3) [Weekly]", "Fama-French",
    "factors_ff_raw_6_size_and_booktomarket_2_x_3_daily",                                              "6 Portfolios Formed on Size and Book-to-Market (2 x 3) [Daily]", "Fama-French",
    "factors_ff_raw_25_size_and_booktomarket_5_x_5",                                                     "25 Portfolios Formed on Size and Book-to-Market (5 x 5)", "Fama-French",
    "factors_ff_raw_25_size_and_booktomarket_5_x_5_ex_dividends",                                     "25 Portfolios Formed on Size and Book-to-Market (5 x 5) [ex. Dividends]", "Fama-French",
    "factors_ff_raw_25_size_and_booktomarket_5_x_5_daily",                                             "25 Portfolios Formed on Size and Book-to-Market (5 x 5) [Daily]", "Fama-French",
    "factors_ff_raw_100_size_and_booktomarket_10_x_10",                                                  "100 Portfolios Formed on Size and Book-to-Market (10 x 10)", "Fama-French",
    "factors_ff_raw_100_size_and_booktomarket_10_x_10_ex_dividends",                                  "100 Portfolios Formed on Size and Book-to-Market (10 x 10) [ex. Dividends]", "Fama-French",
    "factors_ff_raw_100_size_and_booktomarket_10_x_10_daily",                                          "100 Portfolios Formed on Size and Book-to-Market (10 x 10) [Daily]", "Fama-French",
    "factors_ff_raw_6_size_and_operating_profitability_2_x_3",                                             "6 Portfolios Formed on Size and Operating Profitability (2 x 3)", "Fama-French",
    "factors_ff_raw_6_size_and_operating_profitability_2_x_3_ex_dividends",                             "6 Portfolios Formed on Size and Operating Profitability (2 x 3) [ex. Dividends]", "Fama-French",
    "factors_ff_raw_6_size_and_operating_profitability_2_x_3_daily",                                     "6 Portfolios Formed on Size and Operating Profitability (2 x 3) [Daily]", "Fama-French",
    "factors_ff_raw_25_size_and_operating_profitability_5_x_5",                                            "25 Portfolios Formed on Size and Operating Profitability (5 x 5)", "Fama-French",
    "factors_ff_raw_25_size_and_operating_profitability_5_x_5_ex_dividends",                            "25 Portfolios Formed on Size and Operating Profitability (5 x 5) [ex. Dividends]", "Fama-French",
    "factors_ff_raw_25_size_and_operating_profitability\t5_x_5_daily",                                   "25 Portfolios Formed on Size and Operating Profitability\t(5 x 5) [Daily]", "Fama-French",
    "factors_ff_raw_100_size_and_operating_profitability_10_x_10",                                         "100 Portfolios Formed on Size and Operating Profitability (10 x 10)", "Fama-French",
    "factors_ff_raw_100_size_and_operating_profitability_10_x_10_ex_dividends",                         "100 Portfolios Formed on Size and Operating Profitability (10 x 10) [ex. Dividends]", "Fama-French",
    "factors_ff_raw_100_size_and_operating_profitability_10_x_10_daily",                                 "100 Portfolios Formed on Size and Operating Profitability (10 x 10) [Daily]", "Fama-French",
    "factors_ff_raw_6_size_and_investment_2_x_3",                                                          "6 Portfolios Formed on Size and Investment (2 x 3)", "Fama-French",
    "factors_ff_raw_6_size_and_investment_2_x_3_ex_dividends",                                          "6 Portfolios Formed on Size and Investment (2 x 3) [ex. Dividends]", "Fama-French",
    "factors_ff_raw_6_size_and_investment_2_x_3_daily",                                                  "6 Portfolios Formed on Size and Investment (2 x 3) [Daily]", "Fama-French",
    "factors_ff_raw_25_size_and_investment_5_x_5",                                                         "25 Portfolios Formed on Size and Investment (5 x 5)", "Fama-French",
    "factors_ff_raw_25_size_and_investment_5_x_5_ex_dividends",                                         "25 Portfolios Formed on Size and Investment (5 x 5) [ex. Dividends]", "Fama-French",
    "factors_ff_raw_25_size_and_investment_5_x_5_daily",                                                 "25 Portfolios Formed on Size and Investment (5 x 5) [Daily]", "Fama-French",
    "factors_ff_raw_100_size_and_investment_10_x_10",                                                      "100 Portfolios Formed on Size and Investment (10 x 10)", "Fama-French",
    "factors_ff_raw_100_size_and_investment_10_x_10_ex_dividends",                                      "100 Portfolios Formed on Size and Investment (10 x 10) [ex. Dividends]", "Fama-French",
    "factors_ff_raw_100_size_and_investment_10_x_10_daily",                                              "100 Portfolios Formed on Size and Investment (10 x 10) [Daily]", "Fama-French",
    "factors_ff_raw_25_booktomarket_and_operating_profitability_5_x_5",                                  "25 Portfolios Formed on Book-to-Market and Operating Profitability (5 x 5)", "Fama-French",
    "factors_ff_raw_25_booktomarket_and_operating_profitability_5_x_5_ex_dividends",                  "25 Portfolios Formed on Book-to-Market and Operating Profitability (5 x 5) [ex. Dividends]", "Fama-French",
    "factors_ff_raw_25_booktomarket_and_operating_profitability_5_x_5_daily",                          "25 Portfolios Formed on Book-to-Market and Operating Profitability (5 x 5) [Daily]", "Fama-French",
    "factors_ff_raw_25_booktomarket_and_investment_5_x_5",                                               "25 Portfolios Formed on Book-to-Market and Investment (5 x 5)", "Fama-French",
    "factors_ff_raw_25_booktomarket_and_investment_5_x_5_ex_dividends",                               "25 Portfolios Formed on Book-to-Market and Investment (5 x 5) [ex. Dividends]", "Fama-French",
    "factors_ff_raw_25_booktomarket_and_investment_5_x_5_daily",                                       "25 Portfolios Formed on Book-to-Market and Investment (5 x 5) [Daily]", "Fama-French",
    "factors_ff_raw_25_operating_profitability_and_investment_5_x_5",                                      "25 Portfolios Formed on Operating Profitability and Investment (5 x 5)", "Fama-French",
    "factors_ff_raw_25_operating_profitability_and_investment_5_x_5_ex_dividends",                      "25 Portfolios Formed on Operating Profitability and Investment (5 x 5) [ex. Dividends]", "Fama-French",
    "factors_ff_raw_25_operating_profitability_and_investment_5_x_5_daily",                              "25 Portfolios Formed on Operating Profitability and Investment (5 x 5) [Daily]", "Fama-French",
    "factors_ff_raw_32_size_booktomarket_and_operating_profitability_2_x_4_x_4",                       "32 Portfolios Formed on Size, Book-to-Market, and Operating Profitability (2 x 4 x 4)", "Fama-French",
    "factors_ff_raw_32_size_booktomarket_and_operating_profitability_2_x_4_x_4_ex_dividends",       "32 Portfolios Formed on Size, Book-to-Market, and Operating Profitability (2 x 4 x 4) [ex. Dividends]", "Fama-French",
    "factors_ff_raw_32_size_booktomarket_and_investment_2_x_4_x_4",                                    "32 Portfolios Formed on Size, Book-to-Market, and Investment (2 x 4 x 4)", "Fama-French",
    "factors_ff_raw_32_size_booktomarket_and_investment_2_x_4_x_4_ex_dividends",                    "32 Portfolios Formed on Size, Book-to-Market, and Investment (2 x 4 x 4) [ex. Dividends]", "Fama-French",
    "factors_ff_raw_32_size_operating_profitability_and_investment_2_x_4_x_4",                           "32 Portfolios Formed on Size, Operating Profitability, and Investment (2 x 4 x 4)", "Fama-French",
    "factors_ff_raw_32_size_operating_profitability_and_investment_2_x_4_x_4_ex_dividends",           "32 Portfolios Formed on Size, Operating Profitability, and Investment (2 x 4 x 4) [ex. Dividends]", "Fama-French",
    "factors_ff_raw_earningsprice",                                                                         "Portfolios Formed on Earnings/Price", "Fama-French",
    "factors_ff_raw_earningsprice_ex_dividends",                                                         "Portfolios Formed on Earnings/Price [ex. Dividends]", "Fama-French",
    "factors_ff_raw_cashflowprice",                                                                         "Portfolios Formed on Cashflow/Price", "Fama-French",
    "factors_ff_raw_cashflowprice_ex_dividends",                                                         "Portfolios Formed on Cashflow/Price [ex. Dividends]", "Fama-French",
    "factors_ff_raw_dividend_yield",                                                                         "Portfolios Formed on Dividend Yield", "Fama-French",
    "factors_ff_raw_dividend_yield_ex_dividends",                                                         "Portfolios Formed on Dividend Yield [ex. Dividends]", "Fama-French",
    "factors_ff_raw_6_size_and_earningsprice",                                                              "6 Portfolios Formed on Size and Earnings/Price", "Fama-French",
    "factors_ff_raw_6_size_and_earningsprice_ex_dividends",                                              "6 Portfolios Formed on Size and Earnings/Price [ex. Dividends]", "Fama-French",
    "factors_ff_raw_6_size_and_cashflowprice",                                                              "6 Portfolios Formed on Size and Cashflow/Price", "Fama-French",
    "factors_ff_raw_6_size_and_cashflowprice_ex_dividends",                                              "6 Portfolios Formed on Size and Cashflow/Price [ex. Dividends]", "Fama-French",
    "factors_ff_raw_6_size_and_dividend_yield",                                                              "6 Portfolios Formed on Size and Dividend Yield", "Fama-French",
    "factors_ff_raw_6_size_and_dividend_yield_ex_dividends",                                              "6 Portfolios Formed on Size and Dividend Yield [ex. Dividends]", "Fama-French",
    "factors_ff_raw_momentum_factor_mom",                                                                                       "Momentum Factor (Mom)", "Fama-French",
    "factors_ff_raw_momentum_factor_mom_daily",                                                                               "Momentum Factor (Mom) [Daily]", "Fama-French",
    "factors_ff_raw_6_size_and_momentum_2_x_3",                                                            "6 Portfolios Formed on Size and Momentum (2 x 3)", "Fama-French",
    "factors_ff_raw_6_size_and_momentum_2_x_3_daily",                                                    "6 Portfolios Formed on Size and Momentum (2 x 3) [Daily]", "Fama-French",
    "factors_ff_raw_25_size_and_momentum_5_x_5",                                                           "25 Portfolios Formed on Size and Momentum (5 x 5)", "Fama-French",
    "factors_ff_raw_25_size_and_momentum_5_x_5_daily",                                                   "25 Portfolios Formed on Size and Momentum (5 x 5) [Daily]", "Fama-French",
    "factors_ff_raw_10_momentum",                                                                            "10 Portfolios Formed on Momentum", "Fama-French",
    "factors_ff_raw_10_momentum_daily",                                                                    "10 Portfolios Formed on Momentum [Daily]", "Fama-French",
    "factors_ff_raw_shortterm_reversal_factor_st_rev",                                                                         "Short-Term Reversal Factor (ST Rev)", "Fama-French",
    "factors_ff_raw_shortterm_reversal_factor_st_rev_daily",                                                                 "Short-Term Reversal Factor (ST Rev) [Daily]", "Fama-French",
    "factors_ff_raw_6_size_and_shortterm_reversal_2_x_3",                                                 "6 Portfolios Formed on Size and Short-Term Reversal (2 x 3)", "Fama-French",
    "factors_ff_raw_6_size_and_shortterm_reversal_2_x_3_daily",                                         "6 Portfolios Formed on Size and Short-Term Reversal (2 x 3) [Daily]", "Fama-French",
    "factors_ff_raw_25_size_and_shortterm_reversal_5_x_5",                                                "25 Portfolios Formed on Size and Short-Term Reversal (5 x 5)", "Fama-French",
    "factors_ff_raw_25_size_and_shortterm_reversal_5_x_5_daily",                                        "25 Portfolios Formed on Size and Short-Term Reversal (5 x 5) [Daily]", "Fama-French",
    "factors_ff_raw_10_shortterm_reversal",                                                                 "10 Portfolios Formed on Short-Term Reversal", "Fama-French",
    "factors_ff_raw_10_shortterm_reversal_daily",                                                         "10 Portfolios Formed on Short-Term Reversal [Daily]", "Fama-French",
    "factors_ff_raw_longterm_reversal_factor_lt_rev",                                                                          "Long-Term Reversal Factor (LT Rev)", "Fama-French",
    "factors_ff_raw_longterm_reversal_factor_lt_rev_daily",                                                                  "Long-Term Reversal Factor (LT Rev) [Daily]", "Fama-French",
    "factors_ff_raw_6_size_and_longterm_reversal_2_x_3",                                                  "6 Portfolios Formed on Size and Long-Term Reversal (2 x 3)", "Fama-French",
    "factors_ff_raw_6_size_and_longterm_reversal_2_x_3_daily",                                          "6 Portfolios Formed on Size and Long-Term Reversal (2 x 3) [Daily]", "Fama-French",
    "factors_ff_raw_25_size_and_longterm_reversal_5_x_5",                                                 "25 Portfolios Formed on Size and Long-Term Reversal (5 x 5)", "Fama-French",
    "factors_ff_raw_25_size_and_longterm_reversal_5_x_5_daily",                                         "25 Portfolios Formed on Size and Long-Term Reversal (5 x 5) [Daily]", "Fama-French",
    "factors_ff_raw_10_longterm_reversal",                                                                  "10 Portfolios Formed on Long-Term Reversal", "Fama-French",
    "factors_ff_raw_10_longterm_reversal_daily",                                                          "10 Portfolios Formed on Long-Term Reversal [Daily]", "Fama-French",
    "factors_ff_raw_accruals",                                                                               "Portfolios Formed on Accruals", "Fama-French",
    "factors_ff_raw_25_size_and_accruals",                                                                   "25 Portfolios Formed on Size and Accruals", "Fama-French",
    "factors_ff_raw_market_beta",                                                                            "Portfolios Formed on Market Beta", "Fama-French",
    "factors_ff_raw_25_size_and_market_beta",                                                                "25 Portfolios Formed on Size and Market Beta", "Fama-French",
    "factors_ff_raw_net_share_issues",                                                                       "Portfolios Formed on Net Share Issues", "Fama-French",
    "factors_ff_raw_25_size_and_net_share_issues",                                                           "25 Portfolios Formed on Size and Net Share Issues", "Fama-French",
    "factors_ff_raw_variance",                                                                               "Portfolios Formed on Variance", "Fama-French",
    "factors_ff_raw_25_size_and_variance",                                                                   "25 Portfolios Formed on Size and Variance", "Fama-French",
    "factors_ff_raw_residual_variance",                                                                      "Portfolios Formed on Residual Variance", "Fama-French",
    "factors_ff_raw_25_size_and_residual_variance",                                                          "25 Portfolios Formed on Size and Residual Variance", "Fama-French",
    "factors_ff_raw_5_industry_portfolios_ex_dividends",                                                                       "5 Industry Portfolios [ex. Dividends]", "Fama-French",
    "factors_ff_raw_10_industry_portfolios_ex_dividends",                                                                      "10 Industry Portfolios [ex. Dividends]", "Fama-French",
    "factors_ff_raw_12_industry_portfolios",                                                                                      "12 Industry Portfolios", "Fama-French",
    "factors_ff_raw_12_industry_portfolios_ex_dividends",                                                                      "12 Industry Portfolios [ex. Dividends]", "Fama-French",
    "factors_ff_raw_12_industry_portfolios_daily",                                                                              "12 Industry Portfolios [Daily]", "Fama-French",
    "factors_ff_raw_17_industry_portfolios",                                                                                      "17 Industry Portfolios", "Fama-French",
    "factors_ff_raw_17_industry_portfolios_ex_dividends",                                                                      "17 Industry Portfolios [ex. Dividends]", "Fama-French",
    "factors_ff_raw_17_industry_portfolios_daily",                                                                              "17 Industry Portfolios [Daily]", "Fama-French",
    "factors_ff_raw_30_industry_portfolios_ex_dividends",                                                                      "30 Industry Portfolios [ex. Dividends]", "Fama-French",
    "factors_ff_raw_38_industry_portfolios_ex_dividends",                                                                      "38 Industry Portfolios [ex. Dividends]", "Fama-French",
    "factors_ff_raw_48_industry_portfolios_ex_dividends",                                                                      "48 Industry Portfolios [ex. Dividends]", "Fama-French",
    "factors_ff_raw_49_industry_portfolios_ex_dividends",                                                                      "49 Industry Portfolios [ex. Dividends]", "Fama-French",
    "factors_ff_raw_me_breakpoints",                                                                                              "ME Breakpoints", "Fama-French",
    "factors_ff_raw_beme_breakpoints",                                                                                           "BE/ME Breakpoints", "Fama-French",
    "factors_ff_raw_operating_profitability_breakpoints",                                                                         "Operating Profitability Breakpoints", "Fama-French",
    "factors_ff_raw_investment_breakpoints",                                                                                      "Investment Breakpoints", "Fama-French",
    "factors_ff_raw_ep_breakpoints",                                                                                             "E/P Breakpoints", "Fama-French",
    "factors_ff_raw_cfp_breakpoints",                                                                                            "CF/P Breakpoints", "Fama-French",
    "factors_ff_raw_dp_breakpoints",                                                                                             "D/P Breakpoints", "Fama-French",
    "factors_ff_raw_prior_212_return_breakpoints",                                                                             "Prior (2-12) Return Breakpoints", "Fama-French",
    "factors_ff_raw_developed_3",                                                                             "Fama/French Developed 3 Factors", "Fama-French",
    "factors_ff_raw_developed_3_daily",                                                                     "Fama/French Developed 3 Factors [Daily]", "Fama-French",
    "factors_ff_raw_developed_ex_us_3",                                                                       "Fama/French Developed ex US 3 Factors", "Fama-French",
    "factors_ff_raw_developed_ex_us_3_daily",                                                               "Fama/French Developed ex US 3 Factors [Daily]", "Fama-French",
    "factors_ff_raw_european_3",                                                                              "Fama/French European 3 Factors", "Fama-French",
    "factors_ff_raw_european_3_daily",                                                                      "Fama/French European 3 Factors [Daily]", "Fama-French",
    "factors_ff_raw_japanese_3",                                                                              "Fama/French Japanese 3 Factors", "Fama-French",
    "factors_ff_raw_japanese_3_daily",                                                                      "Fama/French Japanese 3 Factors [Daily]", "Fama-French",
    "factors_ff_raw_asia_pacific_ex_japan_3",                                                                 "Fama/French Asia Pacific ex Japan 3 Factors", "Fama-French",
    "factors_ff_raw_asia_pacific_ex_japan_3_daily",                                                         "Fama/French Asia Pacific ex Japan 3 Factors [Daily]", "Fama-French",
    "factors_ff_raw_north_american_3",                                                                        "Fama/French North American 3 Factors", "Fama-French",
    "factors_ff_raw_north_american_3_daily",                                                                "Fama/French North American 3 Factors [Daily]", "Fama-French",
    "factors_ff_raw_developed_5",                                                                             "Fama/French Developed 5 Factors", "Fama-French",
    "factors_ff_raw_developed_5_daily",                                                                     "Fama/French Developed 5 Factors [Daily]", "Fama-French",
    "factors_ff_raw_developed_ex_us_5",                                                                       "Fama/French Developed ex US 5 Factors", "Fama-French",
    "factors_ff_raw_developed_ex_us_5_daily",                                                               "Fama/French Developed ex US 5 Factors [Daily]", "Fama-French",
    "factors_ff_raw_european_5",                                                                              "Fama/French European 5 Factors", "Fama-French",
    "factors_ff_raw_european_5_daily",                                                                      "Fama/French European 5 Factors [Daily]", "Fama-French",
    "factors_ff_raw_japanese_5",                                                                              "Fama/French Japanese 5 Factors", "Fama-French",
    "factors_ff_raw_japanese_5_daily",                                                                      "Fama/French Japanese 5 Factors [Daily]", "Fama-French",
    "factors_ff_raw_asia_pacific_ex_japan_5",                                                                 "Fama/French Asia Pacific ex Japan 5 Factors", "Fama-French",
    "factors_ff_raw_asia_pacific_ex_japan_5_daily",                                                         "Fama/French Asia Pacific ex Japan 5 Factors [Daily]", "Fama-French",
    "factors_ff_raw_north_american_5",                                                                        "Fama/French North American 5 Factors", "Fama-French",
    "factors_ff_raw_north_american_5_daily",                                                                "Fama/French North American 5 Factors [Daily]", "Fama-French",
    "factors_ff_raw_developed_momentum_factor_mom",                                                                             "Developed Momentum Factor (Mom)", "Fama-French",
    "factors_ff_raw_developed_momentum_factor_mom_daily",                                                                     "Developed Momentum Factor (Mom) [Daily]", "Fama-French",
    "factors_ff_raw_developed_ex_us_momentum_factor_mom",                                                                       "Developed ex US Momentum Factor (Mom)", "Fama-French",
    "factors_ff_raw_developed_ex_us_momentum_factor_mom_daily",                                                               "Developed ex US Momentum Factor (Mom) [Daily]", "Fama-French",
    "factors_ff_raw_european_momentum_factor_mom",                                                                              "European Momentum Factor (Mom)", "Fama-French",
    "factors_ff_raw_european_momentum_factor_mom_daily",                                                                      "European Momentum Factor (Mom) [Daily]", "Fama-French",
    "factors_ff_raw_japanese_momentum_factor_mom",                                                                              "Japanese Momentum Factor (Mom)", "Fama-French",
    "factors_ff_raw_japanese_momentum_factor_mom_daily",                                                                      "Japanese Momentum Factor (Mom) [Daily]", "Fama-French",
    "factors_ff_raw_asia_pacific_ex_japan_momentum_factor_mom",                                                                 "Asia Pacific ex Japan Momentum Factor (Mom)", "Fama-French",
    "factors_ff_raw_asia_pacific_ex_japan_momentum_factor_mom_daily",                                                         "Asia Pacific ex Japan Momentum Factor (Mom) [Daily]", "Fama-French",
    "factors_ff_raw_north_american_momentum_factor_mom",                                                                        "North American Momentum Factor (Mom)", "Fama-French",
    "factors_ff_raw_north_american_momentum_factor_mom_daily",                                                                "North American Momentum Factor (Mom) [Daily]", "Fama-French",
    "factors_ff_raw_6_developed_size_and_booktomarket_2_x_3",                                            "6 Developed Portfolios Formed on Size and Book-to-Market (2 x 3)", "Fama-French",
    "factors_ff_raw_6_developed_size_and_booktomarket_2_x_3_daily",                                    "6 Developed Portfolios Formed on Size and Book-to-Market (2 x 3) [Daily]", "Fama-French",
    "factors_ff_raw_6_developed_ex_us_size_and_booktomarket_2_x_3",                                      "6 Developed ex US Portfolios Formed on Size and Book-to-Market (2 x 3)", "Fama-French",
    "factors_ff_raw_6_developed_ex_us_size_and_booktomarket_2_x_3_daily",                              "6 Developed ex US Portfolios Formed on Size and Book-to-Market (2 x 3) [Daily]", "Fama-French",
    "factors_ff_raw_6_european_size_and_booktomarket_2_x_3",                                             "6 European Portfolios Formed on Size and Book-to-Market (2 x 3)", "Fama-French",
    "factors_ff_raw_6_european_size_and_booktomarket_2_x_3_daily",                                     "6 European Portfolios Formed on Size and Book-to-Market (2 x 3) [Daily]", "Fama-French",
    "factors_ff_raw_6_japanese_size_and_booktomarket_2_x_3",                                             "6 Japanese Portfolios Formed on Size and Book-to-Market (2 x 3)", "Fama-French",
    "factors_ff_raw_6_japanese_size_and_booktomarket_2_x_3_daily",                                     "6 Japanese Portfolios Formed on Size and Book-to-Market (2 x 3) [Daily]", "Fama-French",
    "factors_ff_raw_6_asia_pacific_ex_japan_size_and_booktomarket_2_x_3",                                "6 Asia Pacific ex Japan Portfolios Formed on Size and Book-to-Market (2 x 3)", "Fama-French",
    "factors_ff_raw_6_asia_pacific_ex_japan_size_and_booktomarket_2_x_3_daily",                        "6 Asia Pacific ex Japan Portfolios Formed on Size and Book-to-Market (2 x 3) [Daily]", "Fama-French",
    "factors_ff_raw_6_north_american_size_and_booktomarket_2_x_3",                                       "6 North American Portfolios Formed on Size and Book-to-Market (2 x 3)", "Fama-French",
    "factors_ff_raw_6_north_american_size_and_booktomarket_2_x_3_daily",                               "6 North American Portfolios Formed on Size and Book-to-Market (2 x 3) [Daily]", "Fama-French",
    "factors_ff_raw_25_developed_size_and_booktomarket_5_x_5",                                           "25 Developed Portfolios Formed on Size and Book-to-Market (5 x 5)", "Fama-French",
    "factors_ff_raw_25_developed_size_and_booktomarket_5_x_5_daily",                                   "25 Developed Portfolios Formed on Size and Book-to-Market (5 x 5) [Daily]", "Fama-French",
    "factors_ff_raw_25_developed_ex_us_size_and_booktomarket_5_x_5",                                     "25 Developed ex US Portfolios Formed on Size and Book-to-Market (5 x 5)", "Fama-French",
    "factors_ff_raw_25_developed_ex_us_size_and_booktomarket_5_x_5_daily",                             "25 Developed ex US Portfolios Formed on Size and Book-to-Market (5 x 5) [Daily]", "Fama-French",
    "factors_ff_raw_25_european_size_and_booktomarket_5_x_5",                                            "25 European Portfolios Formed on Size and Book-to-Market (5 x 5)", "Fama-French",
    "factors_ff_raw_25_european_size_and_booktomarket_5_x_5_daily",                                    "25 European Portfolios Formed on Size and Book-to-Market (5 x 5) [Daily]", "Fama-French",
    "factors_ff_raw_25_japanese_size_and_booktomarket_5_x_5",                                            "25 Japanese Portfolios Formed on Size and Book-to-Market (5 x 5)", "Fama-French",
    "factors_ff_raw_25_japanese_size_and_booktomarket_5_x_5_daily",                                    "25 Japanese Portfolios Formed on Size and Book-to-Market (5 x 5) [Daily]", "Fama-French",
    "factors_ff_raw_25_asia_pacific_ex_japan_size_and_booktomarket_5_x_5",                               "25 Asia Pacific ex Japan Portfolios Formed on Size and Book-to-Market (5 x 5)", "Fama-French",
    "factors_ff_raw_25_asia_pacific_ex_japan_size_and_booktomarket_5_x_5_daily",                       "25 Asia Pacific ex Japan Portfolios Formed on Size and Book-to-Market (5 x 5) [Daily]", "Fama-French",
    "factors_ff_raw_25_north_american_size_and_booktomarket_5_x_5",                                      "25 North American Portfolios Formed on Size and Book-to-Market (5 x 5)", "Fama-French",
    "factors_ff_raw_25_north_american_size_and_booktomarket_5_x_5_daily",                              "25 North American Portfolios Formed on Size and Book-to-Market (5 x 5) [Daily]", "Fama-French",
    "factors_ff_raw_6_developed_size_and_operating_profitability_2_x_3",                                   "6 Developed Portfolios Formed on Size and Operating Profitability (2 x 3)", "Fama-French",
    "factors_ff_raw_6_developed_size_and_operating_profitability_2_x_3_daily",                           "6 Developed Portfolios Formed on Size and Operating Profitability (2 x 3) [Daily]", "Fama-French",
    "factors_ff_raw_6_developed_ex_us_size_and_operating_profitability_2_x_3",                             "6 Developed ex US Portfolios Formed on Size and Operating Profitability (2 x 3)", "Fama-French",
    "factors_ff_raw_6_developed_ex_us_size_and_operating_profitability_2_x_3_daily",                     "6 Developed ex US Portfolios Formed on Size and Operating Profitability (2 x 3) [Daily]", "Fama-French",
    "factors_ff_raw_6_european_size_and_operating_profitability_2_x_3",                                    "6 European Portfolios Formed on Size and Operating Profitability (2 x 3)", "Fama-French",
    "factors_ff_raw_6_european_size_and_operating_profitability_2_x_3_daily",                            "6 European Portfolios Formed on Size and Operating Profitability (2 x 3) [Daily]", "Fama-French",
    "factors_ff_raw_6_japanese_size_and_operating_profitability_2_x_3",                                    "6 Japanese Portfolios Formed on Size and Operating Profitability (2 x 3)", "Fama-French",
    "factors_ff_raw_6_japanese_size_and_operating_profitability_2_x_3_daily",                            "6 Japanese Portfolios Formed on Size and Operating Profitability (2 x 3) [Daily]", "Fama-French",
    "factors_ff_raw_6_asia_pacific_ex_japan_size_and_operating_profitability_2_x_3",                       "6 Asia Pacific ex Japan Portfolios Formed on Size and Operating Profitability (2 x 3)", "Fama-French",
    "factors_ff_raw_6_asia_pacific_ex_japan_size_and_operating_profitability_2_x_3_daily",               "6 Asia Pacific ex Japan Portfolios Formed on Size and Operating Profitability (2 x 3) [Daily]", "Fama-French",
    "factors_ff_raw_6_north_american_size_and_operating_profitability_2_x_3",                              "6 North American Portfolios Formed on Size and Operating Profitability (2 x 3)", "Fama-French",
    "factors_ff_raw_6_north_american_size_and_operating_profitability_2_x_3_daily",                      "6 North American Portfolios Formed on Size and Operating Profitability (2 x 3) [Daily]", "Fama-French",
    "factors_ff_raw_25_developed_size_and_operating_profitability_5_x_5",                                  "25 Developed Portfolios Formed on Size and Operating Profitability (5 x 5)", "Fama-French",
    "factors_ff_raw_25_developed_size_and_operating_profitability_5_x_5_daily",                          "25 Developed Portfolios Formed on Size and Operating Profitability (5 x 5) [Daily]", "Fama-French",
    "factors_ff_raw_25_developed_ex_us_size_and_operating_profitability_5_x_5",                            "25 Developed ex US Portfolios Formed on Size and Operating Profitability (5 x 5)", "Fama-French",
    "factors_ff_raw_25_developed_ex_us_size_and_operating_profitability_5_x_5_daily",                    "25 Developed ex US Portfolios Formed on Size and Operating Profitability (5 x 5) [Daily]", "Fama-French",
    "factors_ff_raw_25_european_size_and_operating_profitability_5_x_5",                                   "25 European Portfolios Formed on Size and Operating Profitability (5 x 5)", "Fama-French",
    "factors_ff_raw_25_european_size_and_operating_profitability_5_x_5_daily",                           "25 European Portfolios Formed on Size and Operating Profitability (5 x 5) [Daily]", "Fama-French",
    "factors_ff_raw_25_japanese_size_and_operating_profitability_5_x_5",                                   "25 Japanese Portfolios Formed on Size and Operating Profitability (5 x 5)", "Fama-French",
    "factors_ff_raw_25_japanese_size_and_operating_profitability_5_x_5_daily",                           "25 Japanese Portfolios Formed on Size and Operating Profitability (5 x 5) [Daily]", "Fama-French",
    "factors_ff_raw_25_asia_pacific_ex_japan_size_and_operating_profitability_5_x_5",                      "25 Asia Pacific ex Japan Portfolios Formed on Size and Operating Profitability (5 x 5)", "Fama-French",
    "factors_ff_raw_25_asia_pacific_ex_japan_size_and_operating_profitability_5_x_5_daily",              "25 Asia Pacific ex Japan Portfolios Formed on Size and Operating Profitability (5 x 5) [Daily]", "Fama-French",
    "factors_ff_raw_25_north_american_size_and_operating_profitability_5_x_5",                             "25 North American Portfolios Formed on Size and Operating Profitability (5 x 5)", "Fama-French",
    "factors_ff_raw_25_north_american_size_and_operating_profitability_5_x_5_daily",                     "25 North American Portfolios Formed on Size and Operating Profitability (5 x 5) [Daily]", "Fama-French",
    "factors_ff_raw_6_developed_size_and_investment_2_x_3",                                                "6 Developed Portfolios Formed on Size and Investment (2 x 3)", "Fama-French",
    "factors_ff_raw_6_developed_size_and_investment_2_x_3_daily",                                        "6 Developed Portfolios Formed on Size and Investment (2 x 3) [Daily]", "Fama-French",
    "factors_ff_raw_6_developed_ex_us_size_and_investment_2_x_3",                                          "6 Developed ex US Portfolios Formed on Size and Investment (2 x 3)", "Fama-French",
    "factors_ff_raw_6_developed_ex_us_size_and_investment_2_x_3_daily",                                  "6 Developed ex US Portfolios Formed on Size and Investment (2 x 3) [Daily]", "Fama-French",
    "factors_ff_raw_6_european_size_and_investment_2_x_3",                                                 "6 European Portfolios Formed on Size and Investment (2 x 3)", "Fama-French",
    "factors_ff_raw_6_european_size_and_investment_2_x_3_daily",                                         "6 European Portfolios Formed on Size and Investment (2 x 3) [Daily]", "Fama-French",
    "factors_ff_raw_6_japanese_size_and_investment_2_x_3",                                                 "6 Japanese Portfolios Formed on Size and Investment (2 x 3)", "Fama-French",
    "factors_ff_raw_6_japanese_size_and_investment_2_x_3_daily",                                         "6 Japanese Portfolios Formed on Size and Investment (2 x 3) [Daily]", "Fama-French",
    "factors_ff_raw_6_asia_pacific_ex_japan_size_and_investment_2_x_3",                                    "6 Asia Pacific ex Japan Portfolios Formed on Size and Investment (2 x 3)", "Fama-French",
    "factors_ff_raw_6_asia_pacific_ex_japan_size_and_investment_2_x_3_daily",                            "6 Asia Pacific ex Japan Portfolios Formed on Size and Investment (2 x 3) [Daily]", "Fama-French",
    "factors_ff_raw_6_north_american_size_and_investment_2_x_3",                                           "6 North American Portfolios Formed on Size and Investment (2 x 3)", "Fama-French",
    "factors_ff_raw_6_north_american_size_and_investment_2_x_3_daily",                                   "6 North American Portfolios Formed on Size and Investment (2 x 3) [Daily]", "Fama-French",
    "factors_ff_raw_25_developed_size_and_investment_5_x_5",                                               "25 Developed Portfolios Formed on Size and Investment (5 x 5)", "Fama-French",
    "factors_ff_raw_25_developed_size_and_investment_5_x_5_daily",                                       "25 Developed Portfolios Formed on Size and Investment (5 x 5) [Daily]", "Fama-French",
    "factors_ff_raw_25_developed_ex_us_size_and_investment_5_x_5",                                         "25 Developed ex US Portfolios Formed on Size and Investment (5 x 5)", "Fama-French",
    "factors_ff_raw_25_developed_ex_us_size_and_investment_5_x_5_daily",                                 "25 Developed ex US Portfolios Formed on Size and Investment (5 x 5) [Daily]", "Fama-French",
    "factors_ff_raw_25_european_size_and_investment_5_x_5",                                                "25 European Portfolios Formed on Size and Investment (5 x 5)", "Fama-French",
    "factors_ff_raw_25_european_size_and_investment_5_x_5_daily",                                        "25 European Portfolios Formed on Size and Investment (5 x 5) [Daily]", "Fama-French",
    "factors_ff_raw_25_japanese_size_and_investment_5_x_5",                                                "25 Japanese Portfolios Formed on Size and Investment (5 x 5)", "Fama-French",
    "factors_ff_raw_25_japanese_size_and_investment_5_x_5_daily",                                        "25 Japanese Portfolios Formed on Size and Investment (5 x 5) [Daily]", "Fama-French",
    "factors_ff_raw_25_asia_pacific_ex_japan_size_and_investment_5_x_5",                                   "25 Asia Pacific ex Japan Portfolios Formed on Size and Investment (5 x 5)", "Fama-French",
    "factors_ff_raw_25_asia_pacific_ex_japan_size_and_investment_5_x_5_daily",                           "25 Asia Pacific ex Japan Portfolios Formed on Size and Investment (5 x 5) [Daily]", "Fama-French",
    "factors_ff_raw_25_north_american_size_and_investment_5_x_5",                                          "25 North American Portfolios Formed on Size and Investment (5 x 5)", "Fama-French",
    "factors_ff_raw_25_north_american_size_and_investment_5_x_5_daily",                                  "25 North American Portfolios Formed on Size and Investment (5 x 5) [Daily]", "Fama-French",
    "factors_ff_raw_6_developed_size_and_momentum_2_x_3",                                                  "6 Developed Portfolios Formed on Size and Momentum (2 x 3)", "Fama-French",
    "factors_ff_raw_6_developed_size_and_momentum_2_x_3_daily",                                          "6 Developed Portfolios Formed on Size and Momentum (2 x 3) [Daily]", "Fama-French",
    "factors_ff_raw_6_developed_ex_us_size_and_momentum_2_x_3",                                            "6 Developed ex US Portfolios Formed on Size and Momentum (2 x 3)", "Fama-French",
    "factors_ff_raw_6_developed_ex_us_size_and_momentum_2_x_3_daily",                                    "6 Developed ex US Portfolios Formed on Size and Momentum (2 x 3) [Daily]", "Fama-French",
    "factors_ff_raw_6_european_size_and_momentum_2_x_3",                                                   "6 European Portfolios Formed on Size and Momentum (2 x 3)", "Fama-French",
    "factors_ff_raw_6_european_size_and_momentum_2_x_3_daily",                                           "6 European Portfolios Formed on Size and Momentum (2 x 3) [Daily]", "Fama-French",
    "factors_ff_raw_6_japanese_size_and_momentum_2_x_3",                                                   "6 Japanese Portfolios Formed on Size and Momentum (2 x 3)", "Fama-French",
    "factors_ff_raw_6_japanese_size_and_momentum_2_x_3_daily",                                           "6 Japanese Portfolios Formed on Size and Momentum (2 x 3) [Daily]", "Fama-French",
    "factors_ff_raw_6_asia_pacific_ex_japan_size_and_momentum_2_x_3",                                      "6 Asia Pacific ex Japan Portfolios Formed on Size and Momentum (2 x 3)", "Fama-French",
    "factors_ff_raw_6_asia_pacific_ex_japan_size_and_momentum_2_x_3_daily",                              "6 Asia Pacific ex Japan Portfolios Formed on Size and Momentum (2 x 3) [Daily]", "Fama-French",
    "factors_ff_raw_6_north_american_size_and_momentum_2_x_3",                                             "6 North American Portfolios Formed on Size and Momentum (2 x 3)", "Fama-French",
    "factors_ff_raw_6_north_american_size_and_momentum_2_x_3_daily",                                     "6 North American Portfolios Formed on Size and Momentum (2 x 3) [Daily]", "Fama-French",
    "factors_ff_raw_25_developed_size_and_momentum_5_x_5",                                                 "25 Developed Portfolios Formed on Size and Momentum (5 x 5)", "Fama-French",
    "factors_ff_raw_25_developed_size_and_momentum_5_x_5_daily",                                         "25 Developed Portfolios Formed on Size and Momentum (5 x 5) [Daily]", "Fama-French",
    "factors_ff_raw_25_developed_ex_us_size_and_momentum_5_x_5",                                           "25 Developed ex US Portfolios Formed on Size and Momentum (5 x 5)", "Fama-French",
    "factors_ff_raw_25_developed_ex_us_size_and_momentum_5_x_5_daily",                                   "25 Developed ex US Portfolios Formed on Size and Momentum (5 x 5) [Daily]", "Fama-French",
    "factors_ff_raw_25_european_size_and_momentum_5_x_5",                                                  "25 European Portfolios Formed on Size and Momentum (5 x 5)", "Fama-French",
    "factors_ff_raw_25_european_size_and_momentum_5_x_5_daily",                                          "25 European Portfolios Formed on Size and Momentum (5 x 5) [Daily]", "Fama-French",
    "factors_ff_raw_25_japanese_size_and_momentum_5_x_5",                                                  "25 Japanese Portfolios Formed on Size and Momentum (5 x 5)", "Fama-French",
    "factors_ff_raw_25_japanese_size_and_momentum_5_x_5_daily",                                          "25 Japanese Portfolios Formed on Size and Momentum (5 x 5) [Daily]", "Fama-French",
    "factors_ff_raw_25_asia_pacific_ex_japan_size_and_momentum_5_x_5",                                     "25 Asia Pacific ex Japan Portfolios Formed on Size and Momentum (5 x 5)", "Fama-French",
    "factors_ff_raw_25_asia_pacific_ex_japan_size_and_momentum_5_x_5_daily",                             "25 Asia Pacific ex Japan Portfolios Formed on Size and Momentum (5 x 5) [Daily]", "Fama-French",
    "factors_ff_raw_25_north_american_size_and_momentum_5_x_5",                                            "25 North American Portfolios Formed on Size and Momentum (5 x 5)", "Fama-French",
    "factors_ff_raw_25_north_american_size_and_momentum_5_x_5_daily",                                    "25 North American Portfolios Formed on Size and Momentum (5 x 5) [Daily]", "Fama-French",
    "factors_ff_raw_32_developed_size_booktomarket_and_operating_profitability_2_x_4_x_4",             "32 Developed Portfolios Formed on Size, Book-to-Market, and Operating Profitability (2 x 4 x 4)", "Fama-French",
    "factors_ff_raw_32_developed_ex_us_size_booktomarket_and_operating_profitability_2_x_4_x_4",       "32 Developed ex US Portfolios Formed on Size, Book-to-Market, and Operating Profitability (2 x 4 x 4)", "Fama-French",
    "factors_ff_raw_32_european_size_booktomarket_and_operating_profitability_2_x_4_x_4",              "32 European Portfolios Formed on Size, Book-to-Market, and Operating Profitability (2 x 4 x 4)", "Fama-French",
    "factors_ff_raw_32_japanese_size_booktomarket_and_operating_profitability_2_x_4_x_4",              "32 Japanese Portfolios Formed on Size, Book-to-Market, and Operating Profitability (2 x 4 x 4)", "Fama-French",
    "factors_ff_raw_32_asia_pacific_ex_japan_size_booktomarket_and_operating_profitability_2_x_4_x_4", "32 Asia Pacific ex Japan Portfolios Formed on Size, Book-to-Market, and Operating Profitability (2 x 4 x 4)", "Fama-French",
    "factors_ff_raw_32_north_american_size_booktomarket_and_operating_profitability_2_x_4_x_4",        "32 North American Portfolios Formed on Size, Book-to-Market, and Operating Profitability (2 x 4 x 4)", "Fama-French",
    "factors_ff_raw_32_developed_size_booktomarket_and_investment_2_x_4_x_4",                          "32 Developed Portfolios Formed on Size, Book-to-Market, and Investment (2 x 4 x 4)", "Fama-French",
    "factors_ff_raw_32_developed_ex_us_size_booktomarket_and_investment_2_x_4_x_4",                    "32 Developed ex US Portfolios Formed on Size, Book-to-Market, and Investment (2 x 4 x 4)", "Fama-French",
    "factors_ff_raw_32_european_size_booktomarket_and_investment_2_x_4_x_4",                           "32 European Portfolios Formed on Size, Book-to-Market, and Investment (2 x 4 x 4)", "Fama-French",
    "factors_ff_raw_32_japanese_size_booktomarket_and_investment_2_x_4_x_4",                           "32 Japanese Portfolios Formed on Size, Book-to-Market, and Investment (2 x 4 x 4)", "Fama-French",
    "factors_ff_raw_32_asia_pacific_ex_japan_size_booktomarket_and_investment_2_x_4_x_4",              "32 Asia Pacific ex Japan Portfolios Formed on Size, Book-to-Market, and Investment (2 x 4 x 4)", "Fama-French",
    "factors_ff_raw_32_north_american_size_booktomarket_and_investment_2_x_4_x_4",                     "32 North American Portfolios Formed on Size, Book-to-Market, and Investment (2 x 4 x 4)", "Fama-French",
    "factors_ff_raw_32_developed_size_operating_profitability_and_investment_2_x_4_x_4",                 "32 Developed Portfolios Formed on Size, Operating Profitability, and Investment (2 x 4 x 4)", "Fama-French",
    "factors_ff_raw_32_developed_ex_us_size_operating_profitability_and_investment_2_x_4_x_4",           "32 Developed ex US Portfolios Formed on Size, Operating Profitability, and Investment (2 x 4 x 4)", "Fama-French",
    "factors_ff_raw_32_european_size_operating_profitability_and_investment_2_x_4_x_4",                  "32 European Portfolios Formed on Size, Operating Profitability, and Investment (2 x 4 x 4)", "Fama-French",
    "factors_ff_raw_32_japanese_size_operating_profitability_and_investment_2_x_4_x_4",                  "32 Japanese Portfolios Formed on Size, Operating Profitability, and Investment (2 x 4 x 4)", "Fama-French",
    "factors_ff_raw_32_asia_pacific_ex_japan_size_operating_profitability_and_investment_2_x_4_x_4",     "32 Asia Pacific ex Japan Portfolios Formed on Size, Operating Profitability, and Investment (2 x 4 x 4)", "Fama-French",
    "factors_ff_raw_32_north_american_size_operating_profitability_and_investment_2_x_4_x_4",            "32 North American Portfolios Formed on Size, Operating Profitability, and Investment (2 x 4 x 4)", "Fama-French",
    "factors_ff_raw_emerging_5",                                                                              "Fama/French Emerging 5 Factors", "Fama-French",
    "factors_ff_raw_emerging_momentum_factor_mom",                                                                              "Emerging Momentum Factor (Mom)", "Fama-French",
    "factors_ff_raw_6_emerging_market_size_and_booktomarket_2_x_3",                                      "6 Emerging Market Portfolios Formed on Size and Book-to-Market (2 x 3)", "Fama-French",
    "factors_ff_raw_6_emerging_market_size_and_operating_profitability_2_x_3",                             "6 Emerging Market Portfolios Formed on Size and Operating Profitability (2 x 3)", "Fama-French",
    "factors_ff_raw_6_emerging_market_size_and_investment_2_x_3",                                          "6 Emerging Market Portfolios Formed on Size and Investment (2 x 3)", "Fama-French",
    "factors_ff_raw_6_emerging_market_size_and_momentum_2_x_3",                                            "6 Emerging Market Portfolios Formed on Size and Momentum (2 x 3)", "Fama-French",
    "factors_ff_raw_4_emerging_market_booktomarket__and_operating_profitability_2_x_2__",                "4 Emerging Market Portfolios Formed on Book-to-Market  and Operating Profitability (2 x 2)  ", "Fama-French",
    "factors_ff_raw_4_emerging_market_operating_profitability_and_investment_2_x_2",                       "4 Emerging Market Portfolios Formed on Operating Profitability and Investment (2 x 2)", "Fama-French",
    "factors_ff_raw_4_emerging_market_booktomarket_and_investment_2_x_2",                                "4 Emerging Market Portfolios Formed on Book-to-Market and Investment (2 x 2)", "Fama-French"
  )  }

#' List Supported Global Q Dataset Types
#'
#' This function returns a tibble with the supported Global Q dataset types,
#' including their names and frequencies (daily, weekly, weekly week-to-week,
#' monthly, quarterly, annual). Each dataset type is associated with the Global
#' Q model, specifically the q5 factors model for the year 2022. Additionally,
#' it annotates each dataset with the domain "Global Q".
#'
#' @return A tibble with columns: `type` (the type of dataset), `dataset_name`
#'   (the file name of the dataset), and `domain` (the domain to which the
#'   dataset belongs, always "Global Q").
#'
#' @importFrom tibble tribble
#'
list_supported_types_q <- function() {
  tribble(
    ~type, ~dataset_name,
    "factors_q5_daily", "q5_factors_daily_2022.csv",
    "factors_q5_weekly", "q5_factors_weekly_2022.csv",
    "factors_q5_weekly_w2w", "q5_factors_weekly_w2w_2022.csv",
    "factors_q5_monthly", "q5_factors_monthly_2022.csv",
    "factors_q5_quarterly", "q5_factors_quarterly_2022.csv",
    "factors_q5_annual", "q5_factors_annual_2022.csv"
  ) |>
    mutate(domain = "Global Q")
}

#' List Supported Macro Predictor Dataset Types
#'
#' This function returns a tibble with the supported macro predictor dataset
#' types provided by Goyal-Welch, including their frequencies (monthly,
#' quarterly, annual). All dataset types reference the same source file
#' "PredictorData2022.xlsx" for the year 2022. Additionally, it annotates each
#' dataset with the domain "Goyal-Welch".
#'
#' @return A tibble with columns: `type` (the type of dataset), `dataset_name`
#'   (the file name of the dataset, which is the same for all types), and
#'   `domain` (the domain to which the dataset belongs, always "Goyal-Welch").
#'
#' @importFrom tibble tribble
#'
list_supported_types_macro_predictors <- function() {
  tribble(
    ~type, ~dataset_name,
    "macro_predictors_monthly", "PredictorData2022.xlsx",
    "macro_predictors_quarterly", "PredictorData2022.xlsx",
    "macro_predictors_annual", "PredictorData2022.xlsx",
  ) |>
    mutate(domain = "Goyal-Welch")
}

#' List Supported WRDS Dataset Types
#'
#' This function returns a tibble with the supported dataset types provided via
#' WRDS. Additionally, it annotates each dataset with the domain "WRDS".
#'
#' @return A tibble with columns: `type` (the type of dataset), `dataset_name`
#'   (the file name of the dataset), and `domain` (the domain to which the
#'   dataset belongs, always "WRDS").
#'
#' @importFrom tibble tribble
#'
list_supported_types_wrds <- function() {
  tribble(
    ~type, ~dataset_name,
    "wrds_crsp_monthly", "crsp.msf, crsp.msenames, crsp.msedelist",
    "wrds_crsp_daily", "crsp.dsf, crsp.msenames, crsp.msedelist",
    "wrds_compustat_annual", "comp.funda",
    "wrds_ccm_links", "crsp.ccmxpf_linktable",
    "wrds_fisd", "fisd.fisd_mergedissue, fisd.fisd_mergedissuer",
    "wrds_trace", "trace.trace_enhanced"
  ) |>
    mutate(domain = "WRDS")
}

#' List All Supported Dataset Types
#'
#' This function aggregates and returns a comprehensive tibble of all supported
#' dataset types from different domains. It includes various datasets across
#' different frequencies (daily, weekly, monthly, quarterly, annual) and models
#' (e.g., q5 factors, Fama-French 3 and 5 factors, macro predictors).
#'
#' @param domain A character vector to filter for domain specific types
#'   (e.g. c("WRDS", "Fama-French"))
#' @param as_vector Logical indicating whether types should be returned as a
#'   character vector instead of data frame.
#'
#' @return A tibble aggregating all supported dataset types with columns: `type`
#'   (the type of dataset), `dataset_name` (a descriptive name or file name of
#'   the dataset), and `domain` (the domain to which the dataset belongs, e.g.,
#'   "Global Q", "Fama-French", "Goyal-Welch").
#'
#' @examples
#' # List all supported types as a data frame
#' list_supported_types()
#'
#' # Filter by domain
#' list_supported_types(domain = "WRDS")
#'
#' # List supported types as a vector
#' list_supported_types(as_vector = TRUE)
#'
#' @importFrom dplyr bind_rows filter
#'
#' @export
list_supported_types <- function(domain = NULL, as_vector = FALSE) {
  supported_types <- dplyr::bind_rows(
    list_supported_types_q(),
    list_supported_types_ff(),
    list_supported_types_macro_predictors(),
    list_supported_types_wrds()
  )
  if (!is.null(domain)) {
    filter_domains <- domain
    supported_types <- supported_types |>
      dplyr::filter(.data$domain %in% filter_domains)
  }
  if (as_vector) {
    supported_types$type
  } else {
    supported_types
  }
}
