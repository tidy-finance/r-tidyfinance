# List Chapters of Tidy Finance

Returns a character vector containing the names of the chapters
available in the Tidy Finance resource. It provides a quick reference to
the various topics covered.

## Usage

``` r
list_tidy_finance_chapters()
```

## Value

A character vector where each element is the name of a chapter available
in the Tidy Finance resource. These names correspond to specific
chapters in Tidy Finance with R.

## See also

Other utility functions:
[`create_summary_statistics()`](https://r.tidy-finance.org/reference/create_summary_statistics.md),
[`get_available_huggingface_files()`](https://r.tidy-finance.org/reference/get_available_huggingface_files.md),
[`list_supported_datasets()`](https://r.tidy-finance.org/reference/list_supported_datasets.md),
[`list_supported_datasets_ff()`](https://r.tidy-finance.org/reference/list_supported_datasets_ff.md),
[`list_supported_datasets_ff_legacy()`](https://r.tidy-finance.org/reference/list_supported_datasets_ff_legacy.md),
[`list_supported_datasets_macro_predictors()`](https://r.tidy-finance.org/reference/list_supported_datasets_macro_predictors.md),
[`list_supported_datasets_other()`](https://r.tidy-finance.org/reference/list_supported_datasets_other.md),
[`list_supported_datasets_pseudo()`](https://r.tidy-finance.org/reference/list_supported_datasets_pseudo.md),
[`list_supported_datasets_wrds()`](https://r.tidy-finance.org/reference/list_supported_datasets_wrds.md),
[`list_supported_indexes()`](https://r.tidy-finance.org/reference/list_supported_indexes.md),
[`list_supported_jkp_factors()`](https://r.tidy-finance.org/reference/list_supported_jkp_factors.md),
[`open_tidy_finance_website()`](https://r.tidy-finance.org/reference/open_tidy_finance_website.md),
[`trim()`](https://r.tidy-finance.org/reference/trim.md),
[`validate_dates()`](https://r.tidy-finance.org/reference/validate_dates.md),
[`winsorize()`](https://r.tidy-finance.org/reference/winsorize.md)

## Examples

``` r
list_tidy_finance_chapters()
#>  [1] "setting-up-your-environment"                
#>  [2] "introduction-to-tidy-finance"               
#>  [3] "accessing-and-managing-financial-data"      
#>  [4] "wrds-crsp-and-compustat"                    
#>  [5] "trace-and-fisd"                             
#>  [6] "other-data-providers"                       
#>  [7] "beta-estimation"                            
#>  [8] "univariate-portfolio-sorts"                 
#>  [9] "size-sorts-and-p-hacking"                   
#> [10] "value-and-bivariate-sorts"                  
#> [11] "replicating-fama-and-french-factors"        
#> [12] "fama-macbeth-regressions"                   
#> [13] "fixed-effects-and-clustered-standard-errors"
#> [14] "difference-in-differences"                  
#> [15] "factor-selection-via-machine-learning"      
#> [16] "option-pricing-via-machine-learning"        
#> [17] "parametric-portfolio-policies"              
#> [18] "constrained-optimization-and-backtesting"   
#> [19] "wrds-dummy-data"                            
#> [20] "cover-and-logo-design"                      
#> [21] "clean-enhanced-trace-with-r"                
#> [22] "proofs"                                     
#> [23] "hex-sticker"                                
#> [24] "changelog"                                  
```
