# Define types ------------------------------------------------------------
#' List Supported Fama-French Dataset Types
#'
#' This function returns a tibble with the supported Fama-French dataset types,
#' including their names and frequencies (daily, weekly, monthly). Each dataset
#' type is associated with a specific Fama-French model (e.g., 3 factors, 5
#' factors). Additionally, it annotates each dataset with the domain
#' "Fama-French".
#'
#' @returns A tibble with columns: `type` (the type of dataset), `dataset_name`
#'   (a descriptive name of the dataset), and `domain` (the domain to which the
#'   dataset belongs, always "Fama-French").
list_supported_types_ff <- function() {

  # data_sets_raw <- frenchdata::get_french_data_list()$files_list
  #
  # data_sets_types <- data_sets_raw |>
  #   mutate(type = gsub("Fama/French ", "", name),
  #          type = gsub(" Factors", "", type),
  #          type = gsub("Portfolios Formed on ", "", type),
  #          type = gsub(" \\(Mom\\)", "", type),
  #          type = gsub("[[:punct:]]", "", type),
  #          type = gsub(" ", "_", type),
  #          type = tolower(type),
  #          type = if_else(!grepl("weekly|daily", type), paste0(type, "_monthly"), type),
  #          type = gsub("booktomarket", "bm", type),
  #          type = gsub("operating_profitability", "op", type),
  #          type = gsub("ex_dividend", "exdividend", type),
  #          type = gsub("investment", "inv", type),
  #          type = paste0("factors_ff_", type)) |>
  #   select(type, dataset_name = name)
  #
  # datapasta::dp_set_max_rows(300)
  # datapasta::tribble_paste(data_sets_types)

  tribble(
                                                                       ~type,                                                                                                 ~dataset_name,
                                                      "factors_ff_3_monthly",                                                                                       "Fama/French 3 Factors",
                                                       "factors_ff_3_weekly",                                                                              "Fama/French 3 Factors [Weekly]",
                                                        "factors_ff_3_daily",                                                                               "Fama/French 3 Factors [Daily]",
                                                  "factors_ff_5_2x3_monthly",                                                                                 "Fama/French 5 Factors (2x3)",
                                                    "factors_ff_5_2x3_daily",                                                                         "Fama/French 5 Factors (2x3) [Daily]",
                                                   "factors_ff_size_monthly",                                                                                   "Portfolios Formed on Size",
                                       "factors_ff_size_exdividends_monthly",                                                                    "Portfolios Formed on Size [ex.Dividends]",
                                                     "factors_ff_size_daily",                                                                           "Portfolios Formed on Size [Daily]",
                                                     "factors_ff_bm_monthly",                                                                         "Portfolios Formed on Book-to-Market",
                                         "factors_ff_bm_exdividends_monthly",                                                         "Portfolios Formed on Book-to-Market [ex. Dividends]",
                                                       "factors_ff_bm_daily",                                                                 "Portfolios Formed on Book-to-Market [Daily]",
                                                     "factors_ff_op_monthly",                                                                "Portfolios Formed on Operating Profitability",
                                         "factors_ff_op_exdividends_monthly",                                                "Portfolios Formed on Operating Profitability [ex. Dividends]",
                                                       "factors_ff_op_daily",                                                        "Portfolios Formed on Operating Profitability [Daily]",
                                                    "factors_ff_inv_monthly",                                                                             "Portfolios Formed on Investment",
                                        "factors_ff_inv_exdividends_monthly",                                                             "Portfolios Formed on Investment [ex. Dividends]",
                                                      "factors_ff_inv_daily",                                                                     "Portfolios Formed on Investment [Daily]",
                                    "factors_ff_6_size_and_bm_2_x_3_monthly",                                                      "6 Portfolios Formed on Size and Book-to-Market (2 x 3)",
                        "factors_ff_6_size_and_bm_2_x_3_exdividends_monthly",                                      "6 Portfolios Formed on Size and Book-to-Market (2 x 3) [ex. Dividends]",
                                     "factors_ff_6_size_and_bm_2_x_3_weekly",                                             "6 Portfolios Formed on Size and Book-to-Market (2 x 3) [Weekly]",
                                      "factors_ff_6_size_and_bm_2_x_3_daily",                                              "6 Portfolios Formed on Size and Book-to-Market (2 x 3) [Daily]",
                                   "factors_ff_25_size_and_bm_5_x_5_monthly",                                                     "25 Portfolios Formed on Size and Book-to-Market (5 x 5)",
                       "factors_ff_25_size_and_bm_5_x_5_exdividends_monthly",                                     "25 Portfolios Formed on Size and Book-to-Market (5 x 5) [ex. Dividends]",
                                     "factors_ff_25_size_and_bm_5_x_5_daily",                                             "25 Portfolios Formed on Size and Book-to-Market (5 x 5) [Daily]",
                                "factors_ff_100_size_and_bm_10_x_10_monthly",                                                  "100 Portfolios Formed on Size and Book-to-Market (10 x 10)",
                    "factors_ff_100_size_and_bm_10_x_10_exdividends_monthly",                                  "100 Portfolios Formed on Size and Book-to-Market (10 x 10) [ex. Dividends]",
                                  "factors_ff_100_size_and_bm_10_x_10_daily",                                          "100 Portfolios Formed on Size and Book-to-Market (10 x 10) [Daily]",
                                    "factors_ff_6_size_and_op_2_x_3_monthly",                                             "6 Portfolios Formed on Size and Operating Profitability (2 x 3)",
                        "factors_ff_6_size_and_op_2_x_3_exdividends_monthly",                             "6 Portfolios Formed on Size and Operating Profitability (2 x 3) [ex. Dividends]",
                                      "factors_ff_6_size_and_op_2_x_3_daily",                                     "6 Portfolios Formed on Size and Operating Profitability (2 x 3) [Daily]",
                                   "factors_ff_25_size_and_op_5_x_5_monthly",                                            "25 Portfolios Formed on Size and Operating Profitability (5 x 5)",
                       "factors_ff_25_size_and_op_5_x_5_exdividends_monthly",                            "25 Portfolios Formed on Size and Operating Profitability (5 x 5) [ex. Dividends]",
                                    "factors_ff_25_size_and_op\t5_x_5_daily",                                   "25 Portfolios Formed on Size and Operating Profitability\t(5 x 5) [Daily]",
                                "factors_ff_100_size_and_op_10_x_10_monthly",                                         "100 Portfolios Formed on Size and Operating Profitability (10 x 10)",
                    "factors_ff_100_size_and_op_10_x_10_exdividends_monthly",                         "100 Portfolios Formed on Size and Operating Profitability (10 x 10) [ex. Dividends]",
                                  "factors_ff_100_size_and_op_10_x_10_daily",                                 "100 Portfolios Formed on Size and Operating Profitability (10 x 10) [Daily]",
                                   "factors_ff_6_size_and_inv_2_x_3_monthly",                                                          "6 Portfolios Formed on Size and Investment (2 x 3)",
                       "factors_ff_6_size_and_inv_2_x_3_exdividends_monthly",                                          "6 Portfolios Formed on Size and Investment (2 x 3) [ex. Dividends]",
                                     "factors_ff_6_size_and_inv_2_x_3_daily",                                                  "6 Portfolios Formed on Size and Investment (2 x 3) [Daily]",
                                  "factors_ff_25_size_and_inv_5_x_5_monthly",                                                         "25 Portfolios Formed on Size and Investment (5 x 5)",
                      "factors_ff_25_size_and_inv_5_x_5_exdividends_monthly",                                         "25 Portfolios Formed on Size and Investment (5 x 5) [ex. Dividends]",
                                    "factors_ff_25_size_and_inv_5_x_5_daily",                                                 "25 Portfolios Formed on Size and Investment (5 x 5) [Daily]",
                               "factors_ff_100_size_and_inv_10_x_10_monthly",                                                      "100 Portfolios Formed on Size and Investment (10 x 10)",
                   "factors_ff_100_size_and_inv_10_x_10_exdividends_monthly",                                      "100 Portfolios Formed on Size and Investment (10 x 10) [ex. Dividends]",
                                 "factors_ff_100_size_and_inv_10_x_10_daily",                                              "100 Portfolios Formed on Size and Investment (10 x 10) [Daily]",
                                     "factors_ff_25_bm_and_op_5_x_5_monthly",                                  "25 Portfolios Formed on Book-to-Market and Operating Profitability (5 x 5)",
                         "factors_ff_25_bm_and_op_5_x_5_exdividends_monthly",                  "25 Portfolios Formed on Book-to-Market and Operating Profitability (5 x 5) [ex. Dividends]",
                                       "factors_ff_25_bm_and_op_5_x_5_daily",                          "25 Portfolios Formed on Book-to-Market and Operating Profitability (5 x 5) [Daily]",
                                    "factors_ff_25_bm_and_inv_5_x_5_monthly",                                               "25 Portfolios Formed on Book-to-Market and Investment (5 x 5)",
                        "factors_ff_25_bm_and_inv_5_x_5_exdividends_monthly",                               "25 Portfolios Formed on Book-to-Market and Investment (5 x 5) [ex. Dividends]",
                                      "factors_ff_25_bm_and_inv_5_x_5_daily",                                       "25 Portfolios Formed on Book-to-Market and Investment (5 x 5) [Daily]",
                                    "factors_ff_25_op_and_inv_5_x_5_monthly",                                      "25 Portfolios Formed on Operating Profitability and Investment (5 x 5)",
                        "factors_ff_25_op_and_inv_5_x_5_exdividends_monthly",                      "25 Portfolios Formed on Operating Profitability and Investment (5 x 5) [ex. Dividends]",
                                      "factors_ff_25_op_and_inv_5_x_5_daily",                              "25 Portfolios Formed on Operating Profitability and Investment (5 x 5) [Daily]",
                            "factors_ff_32_size_bm_and_op_2_x_4_x_4_monthly",                       "32 Portfolios Formed on Size, Book-to-Market, and Operating Profitability (2 x 4 x 4)",
                "factors_ff_32_size_bm_and_op_2_x_4_x_4_exdividends_monthly",       "32 Portfolios Formed on Size, Book-to-Market, and Operating Profitability (2 x 4 x 4) [ex. Dividends]",
                           "factors_ff_32_size_bm_and_inv_2_x_4_x_4_monthly",                                    "32 Portfolios Formed on Size, Book-to-Market, and Investment (2 x 4 x 4)",
               "factors_ff_32_size_bm_and_inv_2_x_4_x_4_exdividends_monthly",                    "32 Portfolios Formed on Size, Book-to-Market, and Investment (2 x 4 x 4) [ex. Dividends]",
                           "factors_ff_32_size_op_and_inv_2_x_4_x_4_monthly",                           "32 Portfolios Formed on Size, Operating Profitability, and Investment (2 x 4 x 4)",
               "factors_ff_32_size_op_and_inv_2_x_4_x_4_exdividends_monthly",           "32 Portfolios Formed on Size, Operating Profitability, and Investment (2 x 4 x 4) [ex. Dividends]",
                                          "factors_ff_earningsprice_monthly",                                                                         "Portfolios Formed on Earnings/Price",
                              "factors_ff_earningsprice_exdividends_monthly",                                                         "Portfolios Formed on Earnings/Price [ex. Dividends]",
                                          "factors_ff_cashflowprice_monthly",                                                                         "Portfolios Formed on Cashflow/Price",
                              "factors_ff_cashflowprice_exdividends_monthly",                                                         "Portfolios Formed on Cashflow/Price [ex. Dividends]",
                                         "factors_ff_dividend_yield_monthly",                                                                         "Portfolios Formed on Dividend Yield",
                             "factors_ff_dividend_yield_exdividends_monthly",                                                         "Portfolios Formed on Dividend Yield [ex. Dividends]",
                               "factors_ff_6_size_and_earningsprice_monthly",                                                              "6 Portfolios Formed on Size and Earnings/Price",
                   "factors_ff_6_size_and_earningsprice_exdividends_monthly",                                              "6 Portfolios Formed on Size and Earnings/Price [ex. Dividends]",
                               "factors_ff_6_size_and_cashflowprice_monthly",                                                              "6 Portfolios Formed on Size and Cashflow/Price",
                   "factors_ff_6_size_and_cashflowprice_exdividends_monthly",                                              "6 Portfolios Formed on Size and Cashflow/Price [ex. Dividends]",
                              "factors_ff_6_size_and_dividend_yield_monthly",                                                              "6 Portfolios Formed on Size and Dividend Yield",
                  "factors_ff_6_size_and_dividend_yield_exdividends_monthly",                                              "6 Portfolios Formed on Size and Dividend Yield [ex. Dividends]",
                                        "factors_ff_momentum_factor_monthly",                                                                                       "Momentum Factor (Mom)",
                                          "factors_ff_momentum_factor_daily",                                                                               "Momentum Factor (Mom) [Daily]",
                              "factors_ff_6_size_and_momentum_2_x_3_monthly",                                                            "6 Portfolios Formed on Size and Momentum (2 x 3)",
                                "factors_ff_6_size_and_momentum_2_x_3_daily",                                                    "6 Portfolios Formed on Size and Momentum (2 x 3) [Daily]",
                             "factors_ff_25_size_and_momentum_5_x_5_monthly",                                                           "25 Portfolios Formed on Size and Momentum (5 x 5)",
                               "factors_ff_25_size_and_momentum_5_x_5_daily",                                                   "25 Portfolios Formed on Size and Momentum (5 x 5) [Daily]",
                                            "factors_ff_10_momentum_monthly",                                                                            "10 Portfolios Formed on Momentum",
                                              "factors_ff_10_momentum_daily",                                                                    "10 Portfolios Formed on Momentum [Daily]",
                       "factors_ff_shortterm_reversal_factor_st_rev_monthly",                                                                         "Short-Term Reversal Factor (ST Rev)",
                         "factors_ff_shortterm_reversal_factor_st_rev_daily",                                                                 "Short-Term Reversal Factor (ST Rev) [Daily]",
                    "factors_ff_6_size_and_shortterm_reversal_2_x_3_monthly",                                                 "6 Portfolios Formed on Size and Short-Term Reversal (2 x 3)",
                      "factors_ff_6_size_and_shortterm_reversal_2_x_3_daily",                                         "6 Portfolios Formed on Size and Short-Term Reversal (2 x 3) [Daily]",
                   "factors_ff_25_size_and_shortterm_reversal_5_x_5_monthly",                                                "25 Portfolios Formed on Size and Short-Term Reversal (5 x 5)",
                     "factors_ff_25_size_and_shortterm_reversal_5_x_5_daily",                                        "25 Portfolios Formed on Size and Short-Term Reversal (5 x 5) [Daily]",
                                  "factors_ff_10_shortterm_reversal_monthly",                                                                 "10 Portfolios Formed on Short-Term Reversal",
                                    "factors_ff_10_shortterm_reversal_daily",                                                         "10 Portfolios Formed on Short-Term Reversal [Daily]",
                        "factors_ff_longterm_reversal_factor_lt_rev_monthly",                                                                          "Long-Term Reversal Factor (LT Rev)",
                          "factors_ff_longterm_reversal_factor_lt_rev_daily",                                                                  "Long-Term Reversal Factor (LT Rev) [Daily]",
                     "factors_ff_6_size_and_longterm_reversal_2_x_3_monthly",                                                  "6 Portfolios Formed on Size and Long-Term Reversal (2 x 3)",
                       "factors_ff_6_size_and_longterm_reversal_2_x_3_daily",                                          "6 Portfolios Formed on Size and Long-Term Reversal (2 x 3) [Daily]",
                    "factors_ff_25_size_and_longterm_reversal_5_x_5_monthly",                                                 "25 Portfolios Formed on Size and Long-Term Reversal (5 x 5)",
                      "factors_ff_25_size_and_longterm_reversal_5_x_5_daily",                                         "25 Portfolios Formed on Size and Long-Term Reversal (5 x 5) [Daily]",
                                   "factors_ff_10_longterm_reversal_monthly",                                                                  "10 Portfolios Formed on Long-Term Reversal",
                                     "factors_ff_10_longterm_reversal_daily",                                                          "10 Portfolios Formed on Long-Term Reversal [Daily]",
                                               "factors_ff_accruals_monthly",                                                                               "Portfolios Formed on Accruals",
                                   "factors_ff_25_size_and_accruals_monthly",                                                                   "25 Portfolios Formed on Size and Accruals",
                                            "factors_ff_market_beta_monthly",                                                                            "Portfolios Formed on Market Beta",
                                "factors_ff_25_size_and_market_beta_monthly",                                                                "25 Portfolios Formed on Size and Market Beta",
                                       "factors_ff_net_share_issues_monthly",                                                                       "Portfolios Formed on Net Share Issues",
                           "factors_ff_25_size_and_net_share_issues_monthly",                                                           "25 Portfolios Formed on Size and Net Share Issues",
                                               "factors_ff_variance_monthly",                                                                               "Portfolios Formed on Variance",
                                   "factors_ff_25_size_and_variance_monthly",                                                                   "25 Portfolios Formed on Size and Variance",
                                      "factors_ff_residual_variance_monthly",                                                                      "Portfolios Formed on Residual Variance",
                          "factors_ff_25_size_and_residual_variance_monthly",                                                          "25 Portfolios Formed on Size and Residual Variance",
                                  "factors_ff_5_industry_portfolios_monthly",                                                                                       "5 Industry Portfolios",
                      "factors_ff_5_industry_portfolios_exdividends_monthly",                                                                       "5 Industry Portfolios [ex. Dividends]",
                                    "factors_ff_5_industry_portfolios_daily",                                                                               "5 Industry Portfolios [Daily]",
                                 "factors_ff_10_industry_portfolios_monthly",                                                                                      "10 Industry Portfolios",
                     "factors_ff_10_industry_portfolios_exdividends_monthly",                                                                      "10 Industry Portfolios [ex. Dividends]",
                                   "factors_ff_10_industry_portfolios_daily",                                                                              "10 Industry Portfolios [Daily]",
                                 "factors_ff_12_industry_portfolios_monthly",                                                                                      "12 Industry Portfolios",
                     "factors_ff_12_industry_portfolios_exdividends_monthly",                                                                      "12 Industry Portfolios [ex. Dividends]",
                                   "factors_ff_12_industry_portfolios_daily",                                                                              "12 Industry Portfolios [Daily]",
                                 "factors_ff_17_industry_portfolios_monthly",                                                                                      "17 Industry Portfolios",
                     "factors_ff_17_industry_portfolios_exdividends_monthly",                                                                      "17 Industry Portfolios [ex. Dividends]",
                                   "factors_ff_17_industry_portfolios_daily",                                                                              "17 Industry Portfolios [Daily]",
                                 "factors_ff_30_industry_portfolios_monthly",                                                                                      "30 Industry Portfolios",
                     "factors_ff_30_industry_portfolios_exdividends_monthly",                                                                      "30 Industry Portfolios [ex. Dividends]",
                                   "factors_ff_30_industry_portfolios_daily",                                                                              "30 Industry Portfolios [Daily]",
                                 "factors_ff_38_industry_portfolios_monthly",                                                                                      "38 Industry Portfolios",
                     "factors_ff_38_industry_portfolios_exdividends_monthly",                                                                      "38 Industry Portfolios [ex. Dividends]",
                                   "factors_ff_38_industry_portfolios_daily",                                                                              "38 Industry Portfolios [Daily]",
                                 "factors_ff_48_industry_portfolios_monthly",                                                                                      "48 Industry Portfolios",
                     "factors_ff_48_industry_portfolios_exdividends_monthly",                                                                      "48 Industry Portfolios [ex. Dividends]",
                                   "factors_ff_48_industry_portfolios_daily",                                                                              "48 Industry Portfolios [Daily]",
                                 "factors_ff_49_industry_portfolios_monthly",                                                                                      "49 Industry Portfolios",
                     "factors_ff_49_industry_portfolios_exdividends_monthly",                                                                      "49 Industry Portfolios [ex. Dividends]",
                                   "factors_ff_49_industry_portfolios_daily",                                                                              "49 Industry Portfolios [Daily]",
                                         "factors_ff_me_breakpoints_monthly",                                                                                              "ME Breakpoints",
                                       "factors_ff_beme_breakpoints_monthly",                                                                                           "BE/ME Breakpoints",
                                         "factors_ff_op_breakpoints_monthly",                                                                         "Operating Profitability Breakpoints",
                                        "factors_ff_inv_breakpoints_monthly",                                                                                      "Investment Breakpoints",
                                         "factors_ff_ep_breakpoints_monthly",                                                                                             "E/P Breakpoints",
                                        "factors_ff_cfp_breakpoints_monthly",                                                                                            "CF/P Breakpoints",
                                         "factors_ff_dp_breakpoints_monthly",                                                                                             "D/P Breakpoints",
                           "factors_ff_prior_212_return_breakpoints_monthly",                                                                             "Prior (2-12) Return Breakpoints",
                                            "factors_ff_developed_3_monthly",                                                                             "Fama/French Developed 3 Factors",
                                              "factors_ff_developed_3_daily",                                                                     "Fama/French Developed 3 Factors [Daily]",
                                      "factors_ff_developed_ex_us_3_monthly",                                                                       "Fama/French Developed ex US 3 Factors",
                                        "factors_ff_developed_ex_us_3_daily",                                                               "Fama/French Developed ex US 3 Factors [Daily]",
                                             "factors_ff_european_3_monthly",                                                                              "Fama/French European 3 Factors",
                                               "factors_ff_european_3_daily",                                                                      "Fama/French European 3 Factors [Daily]",
                                             "factors_ff_japanese_3_monthly",                                                                              "Fama/French Japanese 3 Factors",
                                               "factors_ff_japanese_3_daily",                                                                      "Fama/French Japanese 3 Factors [Daily]",
                                "factors_ff_asia_pacific_ex_japan_3_monthly",                                                                 "Fama/French Asia Pacific ex Japan 3 Factors",
                                  "factors_ff_asia_pacific_ex_japan_3_daily",                                                         "Fama/French Asia Pacific ex Japan 3 Factors [Daily]",
                                       "factors_ff_north_american_3_monthly",                                                                        "Fama/French North American 3 Factors",
                                         "factors_ff_north_american_3_daily",                                                                "Fama/French North American 3 Factors [Daily]",
                                            "factors_ff_developed_5_monthly",                                                                             "Fama/French Developed 5 Factors",
                                              "factors_ff_developed_5_daily",                                                                     "Fama/French Developed 5 Factors [Daily]",
                                      "factors_ff_developed_ex_us_5_monthly",                                                                       "Fama/French Developed ex US 5 Factors",
                                        "factors_ff_developed_ex_us_5_daily",                                                               "Fama/French Developed ex US 5 Factors [Daily]",
                                             "factors_ff_european_5_monthly",                                                                              "Fama/French European 5 Factors",
                                               "factors_ff_european_5_daily",                                                                      "Fama/French European 5 Factors [Daily]",
                                             "factors_ff_japanese_5_monthly",                                                                              "Fama/French Japanese 5 Factors",
                                               "factors_ff_japanese_5_daily",                                                                      "Fama/French Japanese 5 Factors [Daily]",
                                "factors_ff_asia_pacific_ex_japan_5_monthly",                                                                 "Fama/French Asia Pacific ex Japan 5 Factors",
                                  "factors_ff_asia_pacific_ex_japan_5_daily",                                                         "Fama/French Asia Pacific ex Japan 5 Factors [Daily]",
                                       "factors_ff_north_american_5_monthly",                                                                        "Fama/French North American 5 Factors",
                                         "factors_ff_north_american_5_daily",                                                                "Fama/French North American 5 Factors [Daily]",
                              "factors_ff_developed_momentum_factor_monthly",                                                                             "Developed Momentum Factor (Mom)",
                                "factors_ff_developed_momentum_factor_daily",                                                                     "Developed Momentum Factor (Mom) [Daily]",
                        "factors_ff_developed_ex_us_momentum_factor_monthly",                                                                       "Developed ex US Momentum Factor (Mom)",
                          "factors_ff_developed_ex_us_momentum_factor_daily",                                                               "Developed ex US Momentum Factor (Mom) [Daily]",
                               "factors_ff_european_momentum_factor_monthly",                                                                              "European Momentum Factor (Mom)",
                                 "factors_ff_european_momentum_factor_daily",                                                                      "European Momentum Factor (Mom) [Daily]",
                               "factors_ff_japanese_momentum_factor_monthly",                                                                              "Japanese Momentum Factor (Mom)",
                                 "factors_ff_japanese_momentum_factor_daily",                                                                      "Japanese Momentum Factor (Mom) [Daily]",
                  "factors_ff_asia_pacific_ex_japan_momentum_factor_monthly",                                                                 "Asia Pacific ex Japan Momentum Factor (Mom)",
                    "factors_ff_asia_pacific_ex_japan_momentum_factor_daily",                                                         "Asia Pacific ex Japan Momentum Factor (Mom) [Daily]",
                         "factors_ff_north_american_momentum_factor_monthly",                                                                        "North American Momentum Factor (Mom)",
                           "factors_ff_north_american_momentum_factor_daily",                                                                "North American Momentum Factor (Mom) [Daily]",
                          "factors_ff_6_developed_size_and_bm_2_x_3_monthly",                                            "6 Developed Portfolios Formed on Size and Book-to-Market (2 x 3)",
                            "factors_ff_6_developed_size_and_bm_2_x_3_daily",                                    "6 Developed Portfolios Formed on Size and Book-to-Market (2 x 3) [Daily]",
                    "factors_ff_6_developed_ex_us_size_and_bm_2_x_3_monthly",                                      "6 Developed ex US Portfolios Formed on Size and Book-to-Market (2 x 3)",
                      "factors_ff_6_developed_ex_us_size_and_bm_2_x_3_daily",                              "6 Developed ex US Portfolios Formed on Size and Book-to-Market (2 x 3) [Daily]",
                           "factors_ff_6_european_size_and_bm_2_x_3_monthly",                                             "6 European Portfolios Formed on Size and Book-to-Market (2 x 3)",
                             "factors_ff_6_european_size_and_bm_2_x_3_daily",                                     "6 European Portfolios Formed on Size and Book-to-Market (2 x 3) [Daily]",
                           "factors_ff_6_japanese_size_and_bm_2_x_3_monthly",                                             "6 Japanese Portfolios Formed on Size and Book-to-Market (2 x 3)",
                             "factors_ff_6_japanese_size_and_bm_2_x_3_daily",                                     "6 Japanese Portfolios Formed on Size and Book-to-Market (2 x 3) [Daily]",
              "factors_ff_6_asia_pacific_ex_japan_size_and_bm_2_x_3_monthly",                                "6 Asia Pacific ex Japan Portfolios Formed on Size and Book-to-Market (2 x 3)",
                "factors_ff_6_asia_pacific_ex_japan_size_and_bm_2_x_3_daily",                        "6 Asia Pacific ex Japan Portfolios Formed on Size and Book-to-Market (2 x 3) [Daily]",
                     "factors_ff_6_north_american_size_and_bm_2_x_3_monthly",                                       "6 North American Portfolios Formed on Size and Book-to-Market (2 x 3)",
                       "factors_ff_6_north_american_size_and_bm_2_x_3_daily",                               "6 North American Portfolios Formed on Size and Book-to-Market (2 x 3) [Daily]",
                         "factors_ff_25_developed_size_and_bm_5_x_5_monthly",                                           "25 Developed Portfolios Formed on Size and Book-to-Market (5 x 5)",
                           "factors_ff_25_developed_size_and_bm_5_x_5_daily",                                   "25 Developed Portfolios Formed on Size and Book-to-Market (5 x 5) [Daily]",
                   "factors_ff_25_developed_ex_us_size_and_bm_5_x_5_monthly",                                     "25 Developed ex US Portfolios Formed on Size and Book-to-Market (5 x 5)",
                     "factors_ff_25_developed_ex_us_size_and_bm_5_x_5_daily",                             "25 Developed ex US Portfolios Formed on Size and Book-to-Market (5 x 5) [Daily]",
                          "factors_ff_25_european_size_and_bm_5_x_5_monthly",                                            "25 European Portfolios Formed on Size and Book-to-Market (5 x 5)",
                            "factors_ff_25_european_size_and_bm_5_x_5_daily",                                    "25 European Portfolios Formed on Size and Book-to-Market (5 x 5) [Daily]",
                          "factors_ff_25_japanese_size_and_bm_5_x_5_monthly",                                            "25 Japanese Portfolios Formed on Size and Book-to-Market (5 x 5)",
                            "factors_ff_25_japanese_size_and_bm_5_x_5_daily",                                    "25 Japanese Portfolios Formed on Size and Book-to-Market (5 x 5) [Daily]",
             "factors_ff_25_asia_pacific_ex_japan_size_and_bm_5_x_5_monthly",                               "25 Asia Pacific ex Japan Portfolios Formed on Size and Book-to-Market (5 x 5)",
               "factors_ff_25_asia_pacific_ex_japan_size_and_bm_5_x_5_daily",                       "25 Asia Pacific ex Japan Portfolios Formed on Size and Book-to-Market (5 x 5) [Daily]",
                    "factors_ff_25_north_american_size_and_bm_5_x_5_monthly",                                      "25 North American Portfolios Formed on Size and Book-to-Market (5 x 5)",
                      "factors_ff_25_north_american_size_and_bm_5_x_5_daily",                              "25 North American Portfolios Formed on Size and Book-to-Market (5 x 5) [Daily]",
                          "factors_ff_6_developed_size_and_op_2_x_3_monthly",                                   "6 Developed Portfolios Formed on Size and Operating Profitability (2 x 3)",
                            "factors_ff_6_developed_size_and_op_2_x_3_daily",                           "6 Developed Portfolios Formed on Size and Operating Profitability (2 x 3) [Daily]",
                    "factors_ff_6_developed_ex_us_size_and_op_2_x_3_monthly",                             "6 Developed ex US Portfolios Formed on Size and Operating Profitability (2 x 3)",
                      "factors_ff_6_developed_ex_us_size_and_op_2_x_3_daily",                     "6 Developed ex US Portfolios Formed on Size and Operating Profitability (2 x 3) [Daily]",
                           "factors_ff_6_european_size_and_op_2_x_3_monthly",                                    "6 European Portfolios Formed on Size and Operating Profitability (2 x 3)",
                             "factors_ff_6_european_size_and_op_2_x_3_daily",                            "6 European Portfolios Formed on Size and Operating Profitability (2 x 3) [Daily]",
                           "factors_ff_6_japanese_size_and_op_2_x_3_monthly",                                    "6 Japanese Portfolios Formed on Size and Operating Profitability (2 x 3)",
                             "factors_ff_6_japanese_size_and_op_2_x_3_daily",                            "6 Japanese Portfolios Formed on Size and Operating Profitability (2 x 3) [Daily]",
              "factors_ff_6_asia_pacific_ex_japan_size_and_op_2_x_3_monthly",                       "6 Asia Pacific ex Japan Portfolios Formed on Size and Operating Profitability (2 x 3)",
                "factors_ff_6_asia_pacific_ex_japan_size_and_op_2_x_3_daily",               "6 Asia Pacific ex Japan Portfolios Formed on Size and Operating Profitability (2 x 3) [Daily]",
                     "factors_ff_6_north_american_size_and_op_2_x_3_monthly",                              "6 North American Portfolios Formed on Size and Operating Profitability (2 x 3)",
                       "factors_ff_6_north_american_size_and_op_2_x_3_daily",                      "6 North American Portfolios Formed on Size and Operating Profitability (2 x 3) [Daily]",
                         "factors_ff_25_developed_size_and_op_5_x_5_monthly",                                  "25 Developed Portfolios Formed on Size and Operating Profitability (5 x 5)",
                           "factors_ff_25_developed_size_and_op_5_x_5_daily",                          "25 Developed Portfolios Formed on Size and Operating Profitability (5 x 5) [Daily]",
                   "factors_ff_25_developed_ex_us_size_and_op_5_x_5_monthly",                            "25 Developed ex US Portfolios Formed on Size and Operating Profitability (5 x 5)",
                     "factors_ff_25_developed_ex_us_size_and_op_5_x_5_daily",                    "25 Developed ex US Portfolios Formed on Size and Operating Profitability (5 x 5) [Daily]",
                          "factors_ff_25_european_size_and_op_5_x_5_monthly",                                   "25 European Portfolios Formed on Size and Operating Profitability (5 x 5)",
                            "factors_ff_25_european_size_and_op_5_x_5_daily",                           "25 European Portfolios Formed on Size and Operating Profitability (5 x 5) [Daily]",
                          "factors_ff_25_japanese_size_and_op_5_x_5_monthly",                                   "25 Japanese Portfolios Formed on Size and Operating Profitability (5 x 5)",
                            "factors_ff_25_japanese_size_and_op_5_x_5_daily",                           "25 Japanese Portfolios Formed on Size and Operating Profitability (5 x 5) [Daily]",
             "factors_ff_25_asia_pacific_ex_japan_size_and_op_5_x_5_monthly",                      "25 Asia Pacific ex Japan Portfolios Formed on Size and Operating Profitability (5 x 5)",
               "factors_ff_25_asia_pacific_ex_japan_size_and_op_5_x_5_daily",              "25 Asia Pacific ex Japan Portfolios Formed on Size and Operating Profitability (5 x 5) [Daily]",
                    "factors_ff_25_north_american_size_and_op_5_x_5_monthly",                             "25 North American Portfolios Formed on Size and Operating Profitability (5 x 5)",
                      "factors_ff_25_north_american_size_and_op_5_x_5_daily",                     "25 North American Portfolios Formed on Size and Operating Profitability (5 x 5) [Daily]",
                         "factors_ff_6_developed_size_and_inv_2_x_3_monthly",                                                "6 Developed Portfolios Formed on Size and Investment (2 x 3)",
                           "factors_ff_6_developed_size_and_inv_2_x_3_daily",                                        "6 Developed Portfolios Formed on Size and Investment (2 x 3) [Daily]",
                   "factors_ff_6_developed_ex_us_size_and_inv_2_x_3_monthly",                                          "6 Developed ex US Portfolios Formed on Size and Investment (2 x 3)",
                     "factors_ff_6_developed_ex_us_size_and_inv_2_x_3_daily",                                  "6 Developed ex US Portfolios Formed on Size and Investment (2 x 3) [Daily]",
                          "factors_ff_6_european_size_and_inv_2_x_3_monthly",                                                 "6 European Portfolios Formed on Size and Investment (2 x 3)",
                            "factors_ff_6_european_size_and_inv_2_x_3_daily",                                         "6 European Portfolios Formed on Size and Investment (2 x 3) [Daily]",
                          "factors_ff_6_japanese_size_and_inv_2_x_3_monthly",                                                 "6 Japanese Portfolios Formed on Size and Investment (2 x 3)",
                            "factors_ff_6_japanese_size_and_inv_2_x_3_daily",                                         "6 Japanese Portfolios Formed on Size and Investment (2 x 3) [Daily]",
             "factors_ff_6_asia_pacific_ex_japan_size_and_inv_2_x_3_monthly",                                    "6 Asia Pacific ex Japan Portfolios Formed on Size and Investment (2 x 3)",
               "factors_ff_6_asia_pacific_ex_japan_size_and_inv_2_x_3_daily",                            "6 Asia Pacific ex Japan Portfolios Formed on Size and Investment (2 x 3) [Daily]",
                    "factors_ff_6_north_american_size_and_inv_2_x_3_monthly",                                           "6 North American Portfolios Formed on Size and Investment (2 x 3)",
                      "factors_ff_6_north_american_size_and_inv_2_x_3_daily",                                   "6 North American Portfolios Formed on Size and Investment (2 x 3) [Daily]",
                        "factors_ff_25_developed_size_and_inv_5_x_5_monthly",                                               "25 Developed Portfolios Formed on Size and Investment (5 x 5)",
                          "factors_ff_25_developed_size_and_inv_5_x_5_daily",                                       "25 Developed Portfolios Formed on Size and Investment (5 x 5) [Daily]",
                  "factors_ff_25_developed_ex_us_size_and_inv_5_x_5_monthly",                                         "25 Developed ex US Portfolios Formed on Size and Investment (5 x 5)",
                    "factors_ff_25_developed_ex_us_size_and_inv_5_x_5_daily",                                 "25 Developed ex US Portfolios Formed on Size and Investment (5 x 5) [Daily]",
                         "factors_ff_25_european_size_and_inv_5_x_5_monthly",                                                "25 European Portfolios Formed on Size and Investment (5 x 5)",
                           "factors_ff_25_european_size_and_inv_5_x_5_daily",                                        "25 European Portfolios Formed on Size and Investment (5 x 5) [Daily]",
                         "factors_ff_25_japanese_size_and_inv_5_x_5_monthly",                                                "25 Japanese Portfolios Formed on Size and Investment (5 x 5)",
                           "factors_ff_25_japanese_size_and_inv_5_x_5_daily",                                        "25 Japanese Portfolios Formed on Size and Investment (5 x 5) [Daily]",
            "factors_ff_25_asia_pacific_ex_japan_size_and_inv_5_x_5_monthly",                                   "25 Asia Pacific ex Japan Portfolios Formed on Size and Investment (5 x 5)",
              "factors_ff_25_asia_pacific_ex_japan_size_and_inv_5_x_5_daily",                           "25 Asia Pacific ex Japan Portfolios Formed on Size and Investment (5 x 5) [Daily]",
                   "factors_ff_25_north_american_size_and_inv_5_x_5_monthly",                                          "25 North American Portfolios Formed on Size and Investment (5 x 5)",
                     "factors_ff_25_north_american_size_and_inv_5_x_5_daily",                                  "25 North American Portfolios Formed on Size and Investment (5 x 5) [Daily]",
                    "factors_ff_6_developed_size_and_momentum_2_x_3_monthly",                                                  "6 Developed Portfolios Formed on Size and Momentum (2 x 3)",
                      "factors_ff_6_developed_size_and_momentum_2_x_3_daily",                                          "6 Developed Portfolios Formed on Size and Momentum (2 x 3) [Daily]",
              "factors_ff_6_developed_ex_us_size_and_momentum_2_x_3_monthly",                                            "6 Developed ex US Portfolios Formed on Size and Momentum (2 x 3)",
                "factors_ff_6_developed_ex_us_size_and_momentum_2_x_3_daily",                                    "6 Developed ex US Portfolios Formed on Size and Momentum (2 x 3) [Daily]",
                     "factors_ff_6_european_size_and_momentum_2_x_3_monthly",                                                   "6 European Portfolios Formed on Size and Momentum (2 x 3)",
                       "factors_ff_6_european_size_and_momentum_2_x_3_daily",                                           "6 European Portfolios Formed on Size and Momentum (2 x 3) [Daily]",
                     "factors_ff_6_japanese_size_and_momentum_2_x_3_monthly",                                                   "6 Japanese Portfolios Formed on Size and Momentum (2 x 3)",
                       "factors_ff_6_japanese_size_and_momentum_2_x_3_daily",                                           "6 Japanese Portfolios Formed on Size and Momentum (2 x 3) [Daily]",
        "factors_ff_6_asia_pacific_ex_japan_size_and_momentum_2_x_3_monthly",                                      "6 Asia Pacific ex Japan Portfolios Formed on Size and Momentum (2 x 3)",
          "factors_ff_6_asia_pacific_ex_japan_size_and_momentum_2_x_3_daily",                              "6 Asia Pacific ex Japan Portfolios Formed on Size and Momentum (2 x 3) [Daily]",
               "factors_ff_6_north_american_size_and_momentum_2_x_3_monthly",                                             "6 North American Portfolios Formed on Size and Momentum (2 x 3)",
                 "factors_ff_6_north_american_size_and_momentum_2_x_3_daily",                                     "6 North American Portfolios Formed on Size and Momentum (2 x 3) [Daily]",
                   "factors_ff_25_developed_size_and_momentum_5_x_5_monthly",                                                 "25 Developed Portfolios Formed on Size and Momentum (5 x 5)",
                     "factors_ff_25_developed_size_and_momentum_5_x_5_daily",                                         "25 Developed Portfolios Formed on Size and Momentum (5 x 5) [Daily]",
             "factors_ff_25_developed_ex_us_size_and_momentum_5_x_5_monthly",                                           "25 Developed ex US Portfolios Formed on Size and Momentum (5 x 5)",
               "factors_ff_25_developed_ex_us_size_and_momentum_5_x_5_daily",                                   "25 Developed ex US Portfolios Formed on Size and Momentum (5 x 5) [Daily]",
                    "factors_ff_25_european_size_and_momentum_5_x_5_monthly",                                                  "25 European Portfolios Formed on Size and Momentum (5 x 5)",
                      "factors_ff_25_european_size_and_momentum_5_x_5_daily",                                          "25 European Portfolios Formed on Size and Momentum (5 x 5) [Daily]",
                    "factors_ff_25_japanese_size_and_momentum_5_x_5_monthly",                                                  "25 Japanese Portfolios Formed on Size and Momentum (5 x 5)",
                      "factors_ff_25_japanese_size_and_momentum_5_x_5_daily",                                          "25 Japanese Portfolios Formed on Size and Momentum (5 x 5) [Daily]",
       "factors_ff_25_asia_pacific_ex_japan_size_and_momentum_5_x_5_monthly",                                     "25 Asia Pacific ex Japan Portfolios Formed on Size and Momentum (5 x 5)",
         "factors_ff_25_asia_pacific_ex_japan_size_and_momentum_5_x_5_daily",                             "25 Asia Pacific ex Japan Portfolios Formed on Size and Momentum (5 x 5) [Daily]",
              "factors_ff_25_north_american_size_and_momentum_5_x_5_monthly",                                            "25 North American Portfolios Formed on Size and Momentum (5 x 5)",
                "factors_ff_25_north_american_size_and_momentum_5_x_5_daily",                                    "25 North American Portfolios Formed on Size and Momentum (5 x 5) [Daily]",
                  "factors_ff_32_developed_size_bm_and_op_2_x_4_x_4_monthly",             "32 Developed Portfolios Formed on Size, Book-to-Market, and Operating Profitability (2 x 4 x 4)",
            "factors_ff_32_developed_ex_us_size_bm_and_op_2_x_4_x_4_monthly",       "32 Developed ex US Portfolios Formed on Size, Book-to-Market, and Operating Profitability (2 x 4 x 4)",
                   "factors_ff_32_european_size_bm_and_op_2_x_4_x_4_monthly",              "32 European Portfolios Formed on Size, Book-to-Market, and Operating Profitability (2 x 4 x 4)",
                   "factors_ff_32_japanese_size_bm_and_op_2_x_4_x_4_monthly",              "32 Japanese Portfolios Formed on Size, Book-to-Market, and Operating Profitability (2 x 4 x 4)",
      "factors_ff_32_asia_pacific_ex_japan_size_bm_and_op_2_x_4_x_4_monthly", "32 Asia Pacific ex Japan Portfolios Formed on Size, Book-to-Market, and Operating Profitability (2 x 4 x 4)",
             "factors_ff_32_north_american_size_bm_and_op_2_x_4_x_4_monthly",        "32 North American Portfolios Formed on Size, Book-to-Market, and Operating Profitability (2 x 4 x 4)",
                 "factors_ff_32_developed_size_bm_and_inv_2_x_4_x_4_monthly",                          "32 Developed Portfolios Formed on Size, Book-to-Market, and Investment (2 x 4 x 4)",
           "factors_ff_32_developed_ex_us_size_bm_and_inv_2_x_4_x_4_monthly",                    "32 Developed ex US Portfolios Formed on Size, Book-to-Market, and Investment (2 x 4 x 4)",
                  "factors_ff_32_european_size_bm_and_inv_2_x_4_x_4_monthly",                           "32 European Portfolios Formed on Size, Book-to-Market, and Investment (2 x 4 x 4)",
                  "factors_ff_32_japanese_size_bm_and_inv_2_x_4_x_4_monthly",                           "32 Japanese Portfolios Formed on Size, Book-to-Market, and Investment (2 x 4 x 4)",
     "factors_ff_32_asia_pacific_ex_japan_size_bm_and_inv_2_x_4_x_4_monthly",              "32 Asia Pacific ex Japan Portfolios Formed on Size, Book-to-Market, and Investment (2 x 4 x 4)",
            "factors_ff_32_north_american_size_bm_and_inv_2_x_4_x_4_monthly",                     "32 North American Portfolios Formed on Size, Book-to-Market, and Investment (2 x 4 x 4)",
                 "factors_ff_32_developed_size_op_and_inv_2_x_4_x_4_monthly",                 "32 Developed Portfolios Formed on Size, Operating Profitability, and Investment (2 x 4 x 4)",
           "factors_ff_32_developed_ex_us_size_op_and_inv_2_x_4_x_4_monthly",           "32 Developed ex US Portfolios Formed on Size, Operating Profitability, and Investment (2 x 4 x 4)",
                  "factors_ff_32_european_size_op_and_inv_2_x_4_x_4_monthly",                  "32 European Portfolios Formed on Size, Operating Profitability, and Investment (2 x 4 x 4)",
                  "factors_ff_32_japanese_size_op_and_inv_2_x_4_x_4_monthly",                  "32 Japanese Portfolios Formed on Size, Operating Profitability, and Investment (2 x 4 x 4)",
     "factors_ff_32_asia_pacific_ex_japan_size_op_and_inv_2_x_4_x_4_monthly",     "32 Asia Pacific ex Japan Portfolios Formed on Size, Operating Profitability, and Investment (2 x 4 x 4)",
            "factors_ff_32_north_american_size_op_and_inv_2_x_4_x_4_monthly",            "32 North American Portfolios Formed on Size, Operating Profitability, and Investment (2 x 4 x 4)",
                                             "factors_ff_emerging_5_monthly",                                                                              "Fama/French Emerging 5 Factors",
                               "factors_ff_emerging_momentum_factor_monthly",                                                                              "Emerging Momentum Factor (Mom)",
                    "factors_ff_6_emerging_market_size_and_bm_2_x_3_monthly",                                      "6 Emerging Market Portfolios Formed on Size and Book-to-Market (2 x 3)",
                    "factors_ff_6_emerging_market_size_and_op_2_x_3_monthly",                             "6 Emerging Market Portfolios Formed on Size and Operating Profitability (2 x 3)",
                   "factors_ff_6_emerging_market_size_and_inv_2_x_3_monthly",                                          "6 Emerging Market Portfolios Formed on Size and Investment (2 x 3)",
              "factors_ff_6_emerging_market_size_and_momentum_2_x_3_monthly",                                            "6 Emerging Market Portfolios Formed on Size and Momentum (2 x 3)",
                   "factors_ff_4_emerging_market_bm__and_op_2_x_2___monthly",                "4 Emerging Market Portfolios Formed on Book-to-Market  and Operating Profitability (2 x 2)  ",
                     "factors_ff_4_emerging_market_op_and_inv_2_x_2_monthly",                       "4 Emerging Market Portfolios Formed on Operating Profitability and Investment (2 x 2)",
                     "factors_ff_4_emerging_market_bm_and_inv_2_x_2_monthly",                                "4 Emerging Market Portfolios Formed on Book-to-Market and Investment (2 x 2)"
     ) |>
    mutate(domain = "Fama-French")
}

#' List Supported Legacy Fama-French Dataset Types
#'
#' This function returns a tibble with the legacy names of initially supported
#' Fama-French dataset types, including their names and frequencies (daily, weekly, monthly).
#' Each dataset type is associated with a specific Fama-French model (e.g., 3 factors, 5
#' factors). Additionally, it annotates each dataset with the domain "Fama-French".
#' Not included in the exported `list_supported_types()` function.
#'
#' @returns A tibble with columns: `type` (the type of dataset), `dataset_name`
#'   (a descriptive name of the dataset), and `domain` (the domain to which the
#'   dataset belongs, always "Fama-French").
list_supported_types_ff_legacy <- function() {
  tribble(
    ~type, ~dataset_name,
    "factors_ff3_daily", "Fama/French 3 Factors [Daily]",
    "factors_ff3_weekly", "Fama/French 3 Factors [Weekly]",
    "factors_ff3_monthly", "Fama/French 3 Factors",
    "factors_ff5_daily", "Fama/French 5 Factors (2x3) [Daily]",
    "factors_ff5_monthly", "Fama/French 5 Factors (2x3)",
    "factors_ff_industry_5_monthly", "5 Industry Portfolios",
    "factors_ff_industry_5_daily", "5 Industry Portfolios [Daily]",
    "factors_ff_industry_10_monthly", "10 Industry Portfolios",
    "factors_ff_industry_10_daily", "10 Industry Portfolios [Daily]",
    "factors_ff_industry_30_monthly", "30 Industry Portfolios",
    "factors_ff_industry_30_daily", "30 Industry Portfolios [Daily]",
    "factors_ff_industry_38_monthly", "38 Industry Portfolios",
    "factors_ff_industry_38_daily", "38 Industry Portfolios [Daily]",
    "factors_ff_industry_48_monthly", "48 Industry Portfolios",
    "factors_ff_industry_48_daily", "48 Industry Portfolios [Daily]",
    "factors_ff_industry_49_monthly", "49 Industry Portfolios",
    "factors_ff_industry_49_daily", "49 Industry Portfolios [Daily]"
  ) |>
    mutate(domain = "Fama-French")
}

#' List Supported Global Q Dataset Types
#'
#' This function returns a tibble with the supported Global Q dataset types,
#' including their names and frequencies (daily, weekly, weekly week-to-week,
#' monthly, quarterly, annual). Each dataset type is associated with the Global
#' Q model, specifically the q5 factors model for the year 2023. Additionally,
#' it annotates each dataset with the domain "Global Q".
#'
#' @returns A tibble with columns: `type` (the type of dataset), `dataset_name`
#'   (the file name of the dataset), and `domain` (the domain to which the
#'   dataset belongs, always "Global Q").
list_supported_types_q <- function() {
  tribble(
    ~type, ~dataset_name,
    "factors_q5_daily", "q5_factors_daily_2023.csv",
    "factors_q5_weekly", "q5_factors_weekly_2023.csv",
    "factors_q5_weekly_w2w", "q5_factors_weekly_w2w_2023.csv",
    "factors_q5_monthly", "q5_factors_monthly_2023.csv",
    "factors_q5_quarterly", "q5_factors_quarterly_2023.csv",
    "factors_q5_annual", "q5_factors_annual_2023.csv"
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
#' @returns A tibble with columns: `type` (the type of dataset), `dataset_name`
#'   (the file name of the dataset, which is the same for all types), and
#'   `domain` (the domain to which the dataset belongs, always "Goyal-Welch").
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
#' @returns A tibble with columns: `type` (the type of dataset), `dataset_name`
#'   (the file name of the dataset), and `domain` (the domain to which the
#'   dataset belongs, always "WRDS").
list_supported_types_wrds <- function() {
  tribble(
    ~type, ~dataset_name,
    "wrds_crsp_monthly", "crsp.msf, crsp.msenames, crsp.msedelist",
    "wrds_crsp_daily", "crsp.dsf, crsp.msenames, crsp.msedelist",
    "wrds_compustat_annual", "comp.funda",
    "wrds_compustat_quarterly", "comp.fundq",
    "wrds_ccm_links", "crsp.ccmxpf_linktable",
    "wrds_fisd", "fisd.fisd_mergedissue, fisd.fisd_mergedissuer",
    "wrds_trace_enhanced", "trace.trace_enhanced"
  ) |>
    mutate(domain = "WRDS")
}

#' List Supported Other Data Types
#'
#' Returns a tibble listing the supported other data types and their corresponding dataset names.
#'
#' @returns A tibble with columns \code{type} and \code{dataset_name}, where \code{type} indicates the code used to specify the data source and \code{dataset_name} provides the name of the data source.
list_supported_types_other <- function() {
  tibble(
    "type" = c("stock_prices", "constituents", "fred", "osap"),
    "dataset_name" = c("YahooFinance", "various", "various", "Open Source Asset Pricing"),
    "domain" = c("Stock Prices", "Index Constituents", "FRED", "Open Source Asset Pricing")
  )
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
#' @returns A tibble aggregating all supported dataset types with columns: `type`
#'   (the type of dataset), `dataset_name` (a descriptive name or file name of
#'   the dataset), and `domain` (the domain to which the dataset belongs, e.g.,
#'   "Global Q", "Fama-French", "Goyal-Welch").
#'
#' @export
#' @examples
#' # List all supported types as a data frame
#' list_supported_types()
#'
#' # Filter by domain
#' list_supported_types(domain = "WRDS")
#'
#' # List supported types as a vector
#' list_supported_types(as_vector = TRUE)
list_supported_types <- function(domain = NULL, as_vector = FALSE) {
  supported_types <- dplyr::bind_rows(
    list_supported_types_q(),
    list_supported_types_ff(),
    list_supported_types_macro_predictors(),
    list_supported_types_wrds(),
    list_supported_types_other()
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
