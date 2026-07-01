# List All Supported Datasets

Aggregates and returns a comprehensive tibble of all supported datasets
from different domains. It includes various datasets across different
frequencies (daily, weekly, monthly, quarterly, annual) and models
(e.g., q5 factors, Fama-French 3 and 5 factors, macro predictors).

`list_supported_types()` is a soft-deprecated alias for
`list_supported_datasets()`.

## Usage

``` r
list_supported_datasets(domain = NULL, as_vector = FALSE)

list_supported_types(domain = NULL, as_vector = FALSE)
```

## Arguments

- domain:

  A character vector to filter for domain-specific datasets (e.g.,
  c("WRDS", "Fama-French")).

- as_vector:

  Logical indicating whether datasets should be returned as a character
  vector instead of a data frame.

## Value

A tibble aggregating all supported datasets with columns: `type` (the
type of dataset), `dataset_name` (a descriptive name or file name of the
dataset), and `domain` (the domain to which the dataset belongs, e.g.,
"Global Q", "Fama-French", "Goyal-Welch").

## See also

Other utility functions:
[`create_summary_statistics()`](https://r.tidy-finance.org/reference/create_summary_statistics.md),
[`get_available_huggingface_files()`](https://r.tidy-finance.org/reference/get_available_huggingface_files.md),
[`list_supported_datasets_ff()`](https://r.tidy-finance.org/reference/list_supported_datasets_ff.md),
[`list_supported_datasets_ff_legacy()`](https://r.tidy-finance.org/reference/list_supported_datasets_ff_legacy.md),
[`list_supported_datasets_macro_predictors()`](https://r.tidy-finance.org/reference/list_supported_datasets_macro_predictors.md),
[`list_supported_datasets_other()`](https://r.tidy-finance.org/reference/list_supported_datasets_other.md),
[`list_supported_datasets_pseudo()`](https://r.tidy-finance.org/reference/list_supported_datasets_pseudo.md),
[`list_supported_datasets_wrds()`](https://r.tidy-finance.org/reference/list_supported_datasets_wrds.md),
[`list_supported_indexes()`](https://r.tidy-finance.org/reference/list_supported_indexes.md),
[`list_supported_jkp_factors()`](https://r.tidy-finance.org/reference/list_supported_jkp_factors.md),
[`list_tidy_finance_chapters()`](https://r.tidy-finance.org/reference/list_tidy_finance_chapters.md),
[`open_tidy_finance_website()`](https://r.tidy-finance.org/reference/open_tidy_finance_website.md),
[`trim()`](https://r.tidy-finance.org/reference/trim.md),
[`validate_dates()`](https://r.tidy-finance.org/reference/validate_dates.md),
[`winsorize()`](https://r.tidy-finance.org/reference/winsorize.md)

## Examples

``` r
# List all supported datasets as a data frame
list_supported_datasets()
#> # A tibble: 329 × 3
#>    type                     dataset_name                   domain     
#>    <chr>                    <chr>                          <chr>      
#>  1 factors_q5_daily         q5_factors_daily_2024          Global Q   
#>  2 factors_q5_weekly        q5_factors_weekly_2024         Global Q   
#>  3 factors_q5_weekly_w2w    q5_factors_weekly_w2w_2024     Global Q   
#>  4 factors_q5_monthly       q5_factors_monthly_2024        Global Q   
#>  5 factors_q5_quarterly     q5_factors_quarterly_2024      Global Q   
#>  6 factors_q5_annual        q5_factors_annual_2024         Global Q   
#>  7 factors_ff_3_monthly     Fama/French 3 Factors          Fama-French
#>  8 factors_ff_3_weekly      Fama/French 3 Factors [Weekly] Fama-French
#>  9 factors_ff_3_daily       Fama/French 3 Factors [Daily]  Fama-French
#> 10 factors_ff_5_2x3_monthly Fama/French 5 Factors (2x3)    Fama-French
#> # ℹ 319 more rows

# Filter by domain
list_supported_datasets(domain = "WRDS")
#> # A tibble: 7 × 3
#>   type                     dataset_name                                  domain
#>   <chr>                    <chr>                                         <chr> 
#> 1 wrds_crsp_monthly        crsp.msf, crsp.msenames, crsp.msedelist       WRDS  
#> 2 wrds_crsp_daily          crsp.dsf, crsp.msenames, crsp.msedelist       WRDS  
#> 3 wrds_compustat_annual    comp.funda                                    WRDS  
#> 4 wrds_compustat_quarterly comp.fundq                                    WRDS  
#> 5 wrds_ccm_links           crsp.ccmxpf_linktable                         WRDS  
#> 6 wrds_fisd                fisd.fisd_mergedissue, fisd.fisd_mergedissuer WRDS  
#> 7 wrds_trace_enhanced      trace.trace_enhanced                          WRDS  

# List supported datasets as a vector
list_supported_datasets(as_vector = TRUE)
#>   [1] "factors_q5_daily"                                                     
#>   [2] "factors_q5_weekly"                                                    
#>   [3] "factors_q5_weekly_w2w"                                                
#>   [4] "factors_q5_monthly"                                                   
#>   [5] "factors_q5_quarterly"                                                 
#>   [6] "factors_q5_annual"                                                    
#>   [7] "factors_ff_3_monthly"                                                 
#>   [8] "factors_ff_3_weekly"                                                  
#>   [9] "factors_ff_3_daily"                                                   
#>  [10] "factors_ff_5_2x3_monthly"                                             
#>  [11] "factors_ff_5_2x3_daily"                                               
#>  [12] "factors_ff_size_monthly"                                              
#>  [13] "factors_ff_size_exdividends_monthly"                                  
#>  [14] "factors_ff_size_daily"                                                
#>  [15] "factors_ff_bm_monthly"                                                
#>  [16] "factors_ff_bm_exdividends_monthly"                                    
#>  [17] "factors_ff_bm_daily"                                                  
#>  [18] "factors_ff_op_monthly"                                                
#>  [19] "factors_ff_op_exdividends_monthly"                                    
#>  [20] "factors_ff_op_daily"                                                  
#>  [21] "factors_ff_inv_monthly"                                               
#>  [22] "factors_ff_inv_exdividends_monthly"                                   
#>  [23] "factors_ff_inv_daily"                                                 
#>  [24] "factors_ff_6_size_and_bm_2_x_3_monthly"                               
#>  [25] "factors_ff_6_size_and_bm_2_x_3_exdividends_monthly"                   
#>  [26] "factors_ff_6_size_and_bm_2_x_3_weekly"                                
#>  [27] "factors_ff_6_size_and_bm_2_x_3_daily"                                 
#>  [28] "factors_ff_25_size_and_bm_5_x_5_monthly"                              
#>  [29] "factors_ff_25_size_and_bm_5_x_5_exdividends_monthly"                  
#>  [30] "factors_ff_25_size_and_bm_5_x_5_daily"                                
#>  [31] "factors_ff_100_size_and_bm_10_x_10_monthly"                           
#>  [32] "factors_ff_100_size_and_bm_10_x_10_exdividends_monthly"               
#>  [33] "factors_ff_100_size_and_bm_10_x_10_daily"                             
#>  [34] "factors_ff_6_size_and_op_2_x_3_monthly"                               
#>  [35] "factors_ff_6_size_and_op_2_x_3_exdividends_monthly"                   
#>  [36] "factors_ff_6_size_and_op_2_x_3_daily"                                 
#>  [37] "factors_ff_25_size_and_op_5_x_5_monthly"                              
#>  [38] "factors_ff_25_size_and_op_5_x_5_exdividends_monthly"                  
#>  [39] "factors_ff_25_size_and_op_5_x_5_daily"                                
#>  [40] "factors_ff_100_size_and_op_10_x_10_monthly"                           
#>  [41] "factors_ff_100_size_and_op_10_x_10_exdividends_monthly"               
#>  [42] "factors_ff_100_size_and_op_10_x_10_daily"                             
#>  [43] "factors_ff_6_size_and_inv_2_x_3_monthly"                              
#>  [44] "factors_ff_6_size_and_inv_2_x_3_exdividends_monthly"                  
#>  [45] "factors_ff_6_size_and_inv_2_x_3_daily"                                
#>  [46] "factors_ff_25_size_and_inv_5_x_5_monthly"                             
#>  [47] "factors_ff_25_size_and_inv_5_x_5_exdividends_monthly"                 
#>  [48] "factors_ff_25_size_and_inv_5_x_5_daily"                               
#>  [49] "factors_ff_100_size_and_inv_10_x_10_monthly"                          
#>  [50] "factors_ff_100_size_and_inv_10_x_10_exdividends_monthly"              
#>  [51] "factors_ff_100_size_and_inv_10_x_10_daily"                            
#>  [52] "factors_ff_25_bm_and_op_5_x_5_monthly"                                
#>  [53] "factors_ff_25_bm_and_op_5_x_5_exdividends_monthly"                    
#>  [54] "factors_ff_25_bm_and_op_5_x_5_daily"                                  
#>  [55] "factors_ff_25_bm_and_inv_5_x_5_monthly"                               
#>  [56] "factors_ff_25_bm_and_inv_5_x_5_exdividends_monthly"                   
#>  [57] "factors_ff_25_bm_and_inv_5_x_5_daily"                                 
#>  [58] "factors_ff_25_op_and_inv_5_x_5_monthly"                               
#>  [59] "factors_ff_25_op_and_inv_5_x_5_exdividends_monthly"                   
#>  [60] "factors_ff_25_op_and_inv_5_x_5_daily"                                 
#>  [61] "factors_ff_32_size_bm_and_op_2_x_4_x_4_monthly"                       
#>  [62] "factors_ff_32_size_bm_and_op_2_x_4_x_4_exdividends_monthly"           
#>  [63] "factors_ff_32_size_bm_and_inv_2_x_4_x_4_monthly"                      
#>  [64] "factors_ff_32_size_bm_and_inv_2_x_4_x_4_exdividends_monthly"          
#>  [65] "factors_ff_32_size_op_and_inv_2_x_4_x_4_monthly"                      
#>  [66] "factors_ff_32_size_op_and_inv_2_x_4_x_4_exdividends_monthly"          
#>  [67] "factors_ff_earningsprice_monthly"                                     
#>  [68] "factors_ff_earningsprice_exdividends_monthly"                         
#>  [69] "factors_ff_cashflowprice_monthly"                                     
#>  [70] "factors_ff_cashflowprice_exdividends_monthly"                         
#>  [71] "factors_ff_dividend_yield_monthly"                                    
#>  [72] "factors_ff_dividend_yield_exdividends_monthly"                        
#>  [73] "factors_ff_6_size_and_earningsprice_monthly"                          
#>  [74] "factors_ff_6_size_and_earningsprice_exdividends_monthly"              
#>  [75] "factors_ff_6_size_and_cashflowprice_monthly"                          
#>  [76] "factors_ff_6_size_and_cashflowprice_exdividends_monthly"              
#>  [77] "factors_ff_6_size_and_dividend_yield_monthly"                         
#>  [78] "factors_ff_6_size_and_dividend_yield_exdividends_monthly"             
#>  [79] "factors_ff_momentum_factor_monthly"                                   
#>  [80] "factors_ff_momentum_factor_daily"                                     
#>  [81] "factors_ff_6_size_and_momentum_2_x_3_monthly"                         
#>  [82] "factors_ff_6_size_and_momentum_2_x_3_daily"                           
#>  [83] "factors_ff_25_size_and_momentum_5_x_5_monthly"                        
#>  [84] "factors_ff_25_size_and_momentum_5_x_5_daily"                          
#>  [85] "factors_ff_10_momentum_monthly"                                       
#>  [86] "factors_ff_10_momentum_daily"                                         
#>  [87] "factors_ff_shortterm_reversal_factor_st_rev_monthly"                  
#>  [88] "factors_ff_shortterm_reversal_factor_st_rev_daily"                    
#>  [89] "factors_ff_6_size_and_shortterm_reversal_2_x_3_monthly"               
#>  [90] "factors_ff_6_size_and_shortterm_reversal_2_x_3_daily"                 
#>  [91] "factors_ff_25_size_and_shortterm_reversal_5_x_5_monthly"              
#>  [92] "factors_ff_25_size_and_shortterm_reversal_5_x_5_daily"                
#>  [93] "factors_ff_10_shortterm_reversal_monthly"                             
#>  [94] "factors_ff_10_shortterm_reversal_daily"                               
#>  [95] "factors_ff_longterm_reversal_factor_lt_rev_monthly"                   
#>  [96] "factors_ff_longterm_reversal_factor_lt_rev_daily"                     
#>  [97] "factors_ff_6_size_and_longterm_reversal_2_x_3_monthly"                
#>  [98] "factors_ff_6_size_and_longterm_reversal_2_x_3_daily"                  
#>  [99] "factors_ff_25_size_and_longterm_reversal_5_x_5_monthly"               
#> [100] "factors_ff_25_size_and_longterm_reversal_5_x_5_daily"                 
#> [101] "factors_ff_10_longterm_reversal_monthly"                              
#> [102] "factors_ff_10_longterm_reversal_daily"                                
#> [103] "factors_ff_accruals_monthly"                                          
#> [104] "factors_ff_25_size_and_accruals_monthly"                              
#> [105] "factors_ff_market_beta_monthly"                                       
#> [106] "factors_ff_25_size_and_market_beta_monthly"                           
#> [107] "factors_ff_net_share_issues_monthly"                                  
#> [108] "factors_ff_25_size_and_net_share_issues_monthly"                      
#> [109] "factors_ff_variance_monthly"                                          
#> [110] "factors_ff_25_size_and_variance_monthly"                              
#> [111] "factors_ff_residual_variance_monthly"                                 
#> [112] "factors_ff_25_size_and_residual_variance_monthly"                     
#> [113] "factors_ff_5_industry_portfolios_monthly"                             
#> [114] "factors_ff_5_industry_portfolios_exdividends_monthly"                 
#> [115] "factors_ff_5_industry_portfolios_daily"                               
#> [116] "factors_ff_10_industry_portfolios_monthly"                            
#> [117] "factors_ff_10_industry_portfolios_exdividends_monthly"                
#> [118] "factors_ff_10_industry_portfolios_daily"                              
#> [119] "factors_ff_12_industry_portfolios_monthly"                            
#> [120] "factors_ff_12_industry_portfolios_exdividends_monthly"                
#> [121] "factors_ff_12_industry_portfolios_daily"                              
#> [122] "factors_ff_17_industry_portfolios_monthly"                            
#> [123] "factors_ff_17_industry_portfolios_exdividends_monthly"                
#> [124] "factors_ff_17_industry_portfolios_daily"                              
#> [125] "factors_ff_30_industry_portfolios_monthly"                            
#> [126] "factors_ff_30_industry_portfolios_exdividends_monthly"                
#> [127] "factors_ff_30_industry_portfolios_daily"                              
#> [128] "factors_ff_38_industry_portfolios_monthly"                            
#> [129] "factors_ff_38_industry_portfolios_exdividends_monthly"                
#> [130] "factors_ff_38_industry_portfolios_daily"                              
#> [131] "factors_ff_48_industry_portfolios_monthly"                            
#> [132] "factors_ff_48_industry_portfolios_exdividends_monthly"                
#> [133] "factors_ff_48_industry_portfolios_daily"                              
#> [134] "factors_ff_49_industry_portfolios_monthly"                            
#> [135] "factors_ff_49_industry_portfolios_exdividends_monthly"                
#> [136] "factors_ff_49_industry_portfolios_daily"                              
#> [137] "factors_ff_me_breakpoints_monthly"                                    
#> [138] "factors_ff_beme_breakpoints_monthly"                                  
#> [139] "factors_ff_op_breakpoints_monthly"                                    
#> [140] "factors_ff_inv_breakpoints_monthly"                                   
#> [141] "factors_ff_ep_breakpoints_monthly"                                    
#> [142] "factors_ff_cfp_breakpoints_monthly"                                   
#> [143] "factors_ff_dp_breakpoints_monthly"                                    
#> [144] "factors_ff_prior_212_return_breakpoints_monthly"                      
#> [145] "factors_ff_developed_3_monthly"                                       
#> [146] "factors_ff_developed_3_daily"                                         
#> [147] "factors_ff_developed_ex_us_3_monthly"                                 
#> [148] "factors_ff_developed_ex_us_3_daily"                                   
#> [149] "factors_ff_european_3_monthly"                                        
#> [150] "factors_ff_european_3_daily"                                          
#> [151] "factors_ff_japanese_3_monthly"                                        
#> [152] "factors_ff_japanese_3_daily"                                          
#> [153] "factors_ff_asia_pacific_ex_japan_3_monthly"                           
#> [154] "factors_ff_asia_pacific_ex_japan_3_daily"                             
#> [155] "factors_ff_north_american_3_monthly"                                  
#> [156] "factors_ff_north_american_3_daily"                                    
#> [157] "factors_ff_developed_5_monthly"                                       
#> [158] "factors_ff_developed_5_daily"                                         
#> [159] "factors_ff_developed_ex_us_5_monthly"                                 
#> [160] "factors_ff_developed_ex_us_5_daily"                                   
#> [161] "factors_ff_european_5_monthly"                                        
#> [162] "factors_ff_european_5_daily"                                          
#> [163] "factors_ff_japanese_5_monthly"                                        
#> [164] "factors_ff_japanese_5_daily"                                          
#> [165] "factors_ff_asia_pacific_ex_japan_5_monthly"                           
#> [166] "factors_ff_asia_pacific_ex_japan_5_daily"                             
#> [167] "factors_ff_north_american_5_monthly"                                  
#> [168] "factors_ff_north_american_5_daily"                                    
#> [169] "factors_ff_developed_momentum_factor_monthly"                         
#> [170] "factors_ff_developed_momentum_factor_daily"                           
#> [171] "factors_ff_developed_ex_us_momentum_factor_monthly"                   
#> [172] "factors_ff_developed_ex_us_momentum_factor_daily"                     
#> [173] "factors_ff_european_momentum_factor_monthly"                          
#> [174] "factors_ff_european_momentum_factor_daily"                            
#> [175] "factors_ff_japanese_momentum_factor_monthly"                          
#> [176] "factors_ff_japanese_momentum_factor_daily"                            
#> [177] "factors_ff_asia_pacific_ex_japan_momentum_factor_monthly"             
#> [178] "factors_ff_asia_pacific_ex_japan_momentum_factor_daily"               
#> [179] "factors_ff_north_american_momentum_factor_monthly"                    
#> [180] "factors_ff_north_american_momentum_factor_daily"                      
#> [181] "factors_ff_6_developed_size_and_bm_2_x_3_monthly"                     
#> [182] "factors_ff_6_developed_size_and_bm_2_x_3_daily"                       
#> [183] "factors_ff_6_developed_ex_us_size_and_bm_2_x_3_monthly"               
#> [184] "factors_ff_6_developed_ex_us_size_and_bm_2_x_3_daily"                 
#> [185] "factors_ff_6_european_size_and_bm_2_x_3_monthly"                      
#> [186] "factors_ff_6_european_size_and_bm_2_x_3_daily"                        
#> [187] "factors_ff_6_japanese_size_and_bm_2_x_3_monthly"                      
#> [188] "factors_ff_6_japanese_size_and_bm_2_x_3_daily"                        
#> [189] "factors_ff_6_asia_pacific_ex_japan_size_and_bm_2_x_3_monthly"         
#> [190] "factors_ff_6_asia_pacific_ex_japan_size_and_bm_2_x_3_daily"           
#> [191] "factors_ff_6_north_american_size_and_bm_2_x_3_monthly"                
#> [192] "factors_ff_6_north_american_size_and_bm_2_x_3_daily"                  
#> [193] "factors_ff_25_developed_size_and_bm_5_x_5_monthly"                    
#> [194] "factors_ff_25_developed_size_and_bm_5_x_5_daily"                      
#> [195] "factors_ff_25_developed_ex_us_size_and_bm_5_x_5_monthly"              
#> [196] "factors_ff_25_developed_ex_us_size_and_bm_5_x_5_daily"                
#> [197] "factors_ff_25_european_size_and_bm_5_x_5_monthly"                     
#> [198] "factors_ff_25_european_size_and_bm_5_x_5_daily"                       
#> [199] "factors_ff_25_japanese_size_and_bm_5_x_5_monthly"                     
#> [200] "factors_ff_25_japanese_size_and_bm_5_x_5_daily"                       
#> [201] "factors_ff_25_asia_pacific_ex_japan_size_and_bm_5_x_5_monthly"        
#> [202] "factors_ff_25_asia_pacific_ex_japan_size_and_bm_5_x_5_daily"          
#> [203] "factors_ff_25_north_american_size_and_bm_5_x_5_monthly"               
#> [204] "factors_ff_25_north_american_size_and_bm_5_x_5_daily"                 
#> [205] "factors_ff_6_developed_size_and_op_2_x_3_monthly"                     
#> [206] "factors_ff_6_developed_size_and_op_2_x_3_daily"                       
#> [207] "factors_ff_6_developed_ex_us_size_and_op_2_x_3_monthly"               
#> [208] "factors_ff_6_developed_ex_us_size_and_op_2_x_3_daily"                 
#> [209] "factors_ff_6_european_size_and_op_2_x_3_monthly"                      
#> [210] "factors_ff_6_european_size_and_op_2_x_3_daily"                        
#> [211] "factors_ff_6_japanese_size_and_op_2_x_3_monthly"                      
#> [212] "factors_ff_6_japanese_size_and_op_2_x_3_daily"                        
#> [213] "factors_ff_6_asia_pacific_ex_japan_size_and_op_2_x_3_monthly"         
#> [214] "factors_ff_6_asia_pacific_ex_japan_size_and_op_2_x_3_daily"           
#> [215] "factors_ff_6_north_american_size_and_op_2_x_3_monthly"                
#> [216] "factors_ff_6_north_american_size_and_op_2_x_3_daily"                  
#> [217] "factors_ff_25_developed_size_and_op_5_x_5_monthly"                    
#> [218] "factors_ff_25_developed_size_and_op_5_x_5_daily"                      
#> [219] "factors_ff_25_developed_ex_us_size_and_op_5_x_5_monthly"              
#> [220] "factors_ff_25_developed_ex_us_size_and_op_5_x_5_daily"                
#> [221] "factors_ff_25_european_size_and_op_5_x_5_monthly"                     
#> [222] "factors_ff_25_european_size_and_op_5_x_5_daily"                       
#> [223] "factors_ff_25_japanese_size_and_op_5_x_5_monthly"                     
#> [224] "factors_ff_25_japanese_size_and_op_5_x_5_daily"                       
#> [225] "factors_ff_25_asia_pacific_ex_japan_size_and_op_5_x_5_monthly"        
#> [226] "factors_ff_25_asia_pacific_ex_japan_size_and_op_5_x_5_daily"          
#> [227] "factors_ff_25_north_american_size_and_op_5_x_5_monthly"               
#> [228] "factors_ff_25_north_american_size_and_op_5_x_5_daily"                 
#> [229] "factors_ff_6_developed_size_and_inv_2_x_3_monthly"                    
#> [230] "factors_ff_6_developed_size_and_inv_2_x_3_daily"                      
#> [231] "factors_ff_6_developed_ex_us_size_and_inv_2_x_3_monthly"              
#> [232] "factors_ff_6_developed_ex_us_size_and_inv_2_x_3_daily"                
#> [233] "factors_ff_6_european_size_and_inv_2_x_3_monthly"                     
#> [234] "factors_ff_6_european_size_and_inv_2_x_3_daily"                       
#> [235] "factors_ff_6_japanese_size_and_inv_2_x_3_monthly"                     
#> [236] "factors_ff_6_japanese_size_and_inv_2_x_3_daily"                       
#> [237] "factors_ff_6_asia_pacific_ex_japan_size_and_inv_2_x_3_monthly"        
#> [238] "factors_ff_6_asia_pacific_ex_japan_size_and_inv_2_x_3_daily"          
#> [239] "factors_ff_6_north_american_size_and_inv_2_x_3_monthly"               
#> [240] "factors_ff_6_north_american_size_and_inv_2_x_3_daily"                 
#> [241] "factors_ff_25_developed_size_and_inv_5_x_5_monthly"                   
#> [242] "factors_ff_25_developed_size_and_inv_5_x_5_daily"                     
#> [243] "factors_ff_25_developed_ex_us_size_and_inv_5_x_5_monthly"             
#> [244] "factors_ff_25_developed_ex_us_size_and_inv_5_x_5_daily"               
#> [245] "factors_ff_25_european_size_and_inv_5_x_5_monthly"                    
#> [246] "factors_ff_25_european_size_and_inv_5_x_5_daily"                      
#> [247] "factors_ff_25_japanese_size_and_inv_5_x_5_monthly"                    
#> [248] "factors_ff_25_japanese_size_and_inv_5_x_5_daily"                      
#> [249] "factors_ff_25_asia_pacific_ex_japan_size_and_inv_5_x_5_monthly"       
#> [250] "factors_ff_25_asia_pacific_ex_japan_size_and_inv_5_x_5_daily"         
#> [251] "factors_ff_25_north_american_size_and_inv_5_x_5_monthly"              
#> [252] "factors_ff_25_north_american_size_and_inv_5_x_5_daily"                
#> [253] "factors_ff_6_developed_size_and_momentum_2_x_3_monthly"               
#> [254] "factors_ff_6_developed_size_and_momentum_2_x_3_daily"                 
#> [255] "factors_ff_6_developed_ex_us_size_and_momentum_2_x_3_monthly"         
#> [256] "factors_ff_6_developed_ex_us_size_and_momentum_2_x_3_daily"           
#> [257] "factors_ff_6_european_size_and_momentum_2_x_3_monthly"                
#> [258] "factors_ff_6_european_size_and_momentum_2_x_3_daily"                  
#> [259] "factors_ff_6_japanese_size_and_momentum_2_x_3_monthly"                
#> [260] "factors_ff_6_japanese_size_and_momentum_2_x_3_daily"                  
#> [261] "factors_ff_6_asia_pacific_ex_japan_size_and_momentum_2_x_3_monthly"   
#> [262] "factors_ff_6_asia_pacific_ex_japan_size_and_momentum_2_x_3_daily"     
#> [263] "factors_ff_6_north_american_size_and_momentum_2_x_3_monthly"          
#> [264] "factors_ff_6_north_american_size_and_momentum_2_x_3_daily"            
#> [265] "factors_ff_25_developed_size_and_momentum_5_x_5_monthly"              
#> [266] "factors_ff_25_developed_size_and_momentum_5_x_5_daily"                
#> [267] "factors_ff_25_developed_ex_us_size_and_momentum_5_x_5_monthly"        
#> [268] "factors_ff_25_developed_ex_us_size_and_momentum_5_x_5_daily"          
#> [269] "factors_ff_25_european_size_and_momentum_5_x_5_monthly"               
#> [270] "factors_ff_25_european_size_and_momentum_5_x_5_daily"                 
#> [271] "factors_ff_25_japanese_size_and_momentum_5_x_5_monthly"               
#> [272] "factors_ff_25_japanese_size_and_momentum_5_x_5_daily"                 
#> [273] "factors_ff_25_asia_pacific_ex_japan_size_and_momentum_5_x_5_monthly"  
#> [274] "factors_ff_25_asia_pacific_ex_japan_size_and_momentum_5_x_5_daily"    
#> [275] "factors_ff_25_north_american_size_and_momentum_5_x_5_monthly"         
#> [276] "factors_ff_25_north_american_size_and_momentum_5_x_5_daily"           
#> [277] "factors_ff_32_developed_size_bm_and_op_2_x_4_x_4_monthly"             
#> [278] "factors_ff_32_developed_ex_us_size_bm_and_op_2_x_4_x_4_monthly"       
#> [279] "factors_ff_32_european_size_bm_and_op_2_x_4_x_4_monthly"              
#> [280] "factors_ff_32_japanese_size_bm_and_op_2_x_4_x_4_monthly"              
#> [281] "factors_ff_32_asia_pacific_ex_japan_size_bm_and_op_2_x_4_x_4_monthly" 
#> [282] "factors_ff_32_north_american_size_bm_and_op_2_x_4_x_4_monthly"        
#> [283] "factors_ff_32_developed_size_bm_and_inv_2_x_4_x_4_monthly"            
#> [284] "factors_ff_32_developed_ex_us_size_bm_and_inv_2_x_4_x_4_monthly"      
#> [285] "factors_ff_32_european_size_bm_and_inv_2_x_4_x_4_monthly"             
#> [286] "factors_ff_32_japanese_size_bm_and_inv_2_x_4_x_4_monthly"             
#> [287] "factors_ff_32_asia_pacific_ex_japan_size_bm_and_inv_2_x_4_x_4_monthly"
#> [288] "factors_ff_32_north_american_size_bm_and_inv_2_x_4_x_4_monthly"       
#> [289] "factors_ff_32_developed_size_op_and_inv_2_x_4_x_4_monthly"            
#> [290] "factors_ff_32_developed_ex_us_size_op_and_inv_2_x_4_x_4_monthly"      
#> [291] "factors_ff_32_european_size_op_and_inv_2_x_4_x_4_monthly"             
#> [292] "factors_ff_32_japanese_size_op_and_inv_2_x_4_x_4_monthly"             
#> [293] "factors_ff_32_asia_pacific_ex_japan_size_op_and_inv_2_x_4_x_4_monthly"
#> [294] "factors_ff_32_north_american_size_op_and_inv_2_x_4_x_4_monthly"       
#> [295] "factors_ff_emerging_5_monthly"                                        
#> [296] "factors_ff_emerging_momentum_factor_monthly"                          
#> [297] "factors_ff_6_emerging_market_size_and_bm_2_x_3_monthly"               
#> [298] "factors_ff_6_emerging_market_size_and_op_2_x_3_monthly"               
#> [299] "factors_ff_6_emerging_market_size_and_inv_2_x_3_monthly"              
#> [300] "factors_ff_6_emerging_market_size_and_momentum_2_x_3_monthly"         
#> [301] "factors_ff_4_emerging_market_bm__and_op_2_x_2___monthly"              
#> [302] "factors_ff_4_emerging_market_op_and_inv_2_x_2_monthly"                
#> [303] "factors_ff_4_emerging_market_bm_and_inv_2_x_2_monthly"                
#> [304] "macro_predictors_monthly"                                             
#> [305] "macro_predictors_quarterly"                                           
#> [306] "macro_predictors_annual"                                              
#> [307] "wrds_crsp_monthly"                                                    
#> [308] "wrds_crsp_daily"                                                      
#> [309] "wrds_compustat_annual"                                                
#> [310] "wrds_compustat_quarterly"                                             
#> [311] "wrds_ccm_links"                                                       
#> [312] "wrds_fisd"                                                            
#> [313] "wrds_trace_enhanced"                                                  
#> [314] "crsp_monthly"                                                         
#> [315] "crsp_daily"                                                           
#> [316] "compustat_annual"                                                     
#> [317] "compustat_quarterly"                                                  
#> [318] "ccm_links"                                                            
#> [319] "stock_prices"                                                         
#> [320] "constituents"                                                         
#> [321] "fred"                                                                 
#> [322] "osap"                                                                 
#> [323] "jkp"                                                                  
#> [324] "liquidity"                                                            
#> [325] "mispricing"                                                           
#> [326] "risk_free"                                                            
#> [327] "high_frequency_sp500"                                                 
#> [328] "factor_library"                                                       
#> [329] "factor_library_grid"                                                  
```
