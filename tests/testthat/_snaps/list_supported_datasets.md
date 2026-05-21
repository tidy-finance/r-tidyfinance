# list_supported_datasets_ff returns expected structure

    Code
      result
    Output
      # A tibble: 297 x 3
         type                                dataset_name                       domain
         <chr>                               <chr>                              <chr> 
       1 factors_ff_3_monthly                Fama/French 3 Factors              Fama-~
       2 factors_ff_3_weekly                 Fama/French 3 Factors [Weekly]     Fama-~
       3 factors_ff_3_daily                  Fama/French 3 Factors [Daily]      Fama-~
       4 factors_ff_5_2x3_monthly            Fama/French 5 Factors (2x3)        Fama-~
       5 factors_ff_5_2x3_daily              Fama/French 5 Factors (2x3) [Dail~ Fama-~
       6 factors_ff_size_monthly             Portfolios Formed on Size          Fama-~
       7 factors_ff_size_exdividends_monthly Portfolios Formed on Size [ex.Div~ Fama-~
       8 factors_ff_size_daily               Portfolios Formed on Size [Daily]  Fama-~
       9 factors_ff_bm_monthly               Portfolios Formed on Book-to-Mark~ Fama-~
      10 factors_ff_bm_exdividends_monthly   Portfolios Formed on Book-to-Mark~ Fama-~
      # i 287 more rows

# list_supported_datasets_ff_legacy returns expected structure

    Code
      result
    Output
      # A tibble: 17 x 3
         type                           dataset_name                        domain    
         <chr>                          <chr>                               <chr>     
       1 factors_ff3_daily              Fama/French 3 Factors [Daily]       Fama-Fren~
       2 factors_ff3_weekly             Fama/French 3 Factors [Weekly]      Fama-Fren~
       3 factors_ff3_monthly            Fama/French 3 Factors               Fama-Fren~
       4 factors_ff5_daily              Fama/French 5 Factors (2x3) [Daily] Fama-Fren~
       5 factors_ff5_monthly            Fama/French 5 Factors (2x3)         Fama-Fren~
       6 factors_ff_industry_5_monthly  5 Industry Portfolios               Fama-Fren~
       7 factors_ff_industry_5_daily    5 Industry Portfolios [Daily]       Fama-Fren~
       8 factors_ff_industry_10_monthly 10 Industry Portfolios              Fama-Fren~
       9 factors_ff_industry_10_daily   10 Industry Portfolios [Daily]      Fama-Fren~
      10 factors_ff_industry_30_monthly 30 Industry Portfolios              Fama-Fren~
      11 factors_ff_industry_30_daily   30 Industry Portfolios [Daily]      Fama-Fren~
      12 factors_ff_industry_38_monthly 38 Industry Portfolios              Fama-Fren~
      13 factors_ff_industry_38_daily   38 Industry Portfolios [Daily]      Fama-Fren~
      14 factors_ff_industry_48_monthly 48 Industry Portfolios              Fama-Fren~
      15 factors_ff_industry_48_daily   48 Industry Portfolios [Daily]      Fama-Fren~
      16 factors_ff_industry_49_monthly 49 Industry Portfolios              Fama-Fren~
      17 factors_ff_industry_49_daily   49 Industry Portfolios [Daily]      Fama-Fren~

# list_supported_datasets_q returns expected structure

    Code
      result
    Output
      # A tibble: 6 x 3
        type                  dataset_name               domain  
        <chr>                 <chr>                      <chr>   
      1 factors_q5_daily      q5_factors_daily_2024      Global Q
      2 factors_q5_weekly     q5_factors_weekly_2024     Global Q
      3 factors_q5_weekly_w2w q5_factors_weekly_w2w_2024 Global Q
      4 factors_q5_monthly    q5_factors_monthly_2024    Global Q
      5 factors_q5_quarterly  q5_factors_quarterly_2024  Global Q
      6 factors_q5_annual     q5_factors_annual_2024     Global Q

# list_supported_datasets_macro_predictors returns expected structure

    Code
      result
    Output
      # A tibble: 3 x 3
        type                       dataset_name           domain     
        <chr>                      <chr>                  <chr>      
      1 macro_predictors_monthly   PredictorData2022.xlsx Goyal-Welch
      2 macro_predictors_quarterly PredictorData2022.xlsx Goyal-Welch
      3 macro_predictors_annual    PredictorData2022.xlsx Goyal-Welch

# list_supported_datasets_wrds returns expected structure

    Code
      result
    Output
      # A tibble: 7 x 3
        type                     dataset_name                                  domain
        <chr>                    <chr>                                         <chr> 
      1 wrds_crsp_monthly        crsp.msf, crsp.msenames, crsp.msedelist       WRDS  
      2 wrds_crsp_daily          crsp.dsf, crsp.msenames, crsp.msedelist       WRDS  
      3 wrds_compustat_annual    comp.funda                                    WRDS  
      4 wrds_compustat_quarterly comp.fundq                                    WRDS  
      5 wrds_ccm_links           crsp.ccmxpf_linktable                         WRDS  
      6 wrds_fisd                fisd.fisd_mergedissue, fisd.fisd_mergedissuer WRDS  
      7 wrds_trace_enhanced      trace.trace_enhanced                          WRDS  

# list_supported_datasets_other returns expected structure

    Code
      result
    Output
      # A tibble: 8 x 3
        type                 dataset_name              domain                   
        <chr>                <chr>                     <chr>                    
      1 stock_prices         YahooFinance              Stock Prices             
      2 constituents         various                   Index Constituents       
      3 fred                 various                   FRED                     
      4 osap                 Open Source Asset Pricing Open Source Asset Pricing
      5 risk_free            Risk-Free Rate            tidyfinance              
      6 high_frequency_sp500 High Frequency S&P 500    tidyfinance              
      7 factor_library       Factor Library            tidyfinance              
      8 factor_library_grid  Factor Library Grid       tidyfinance              

