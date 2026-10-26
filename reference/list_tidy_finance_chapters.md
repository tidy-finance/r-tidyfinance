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
in the Tidy Finance resource, in the order of the website. These names
correspond to the chapter pages at
`https://www.tidy-finance.org/chapters/`.

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
#>  [2] "the-tidyfinance-package"                    
#>  [3] "working-with-stock-returns"                 
#>  [4] "modern-portfolio-theory"                    
#>  [5] "capital-asset-pricing-model"                
#>  [6] "financial-statement-analysis"               
#>  [7] "discounted-cash-flow-analysis"              
#>  [8] "accessing-and-managing-financial-data"      
#>  [9] "wrds-crsp-and-compustat"                    
#> [10] "trace-and-fisd"                             
#> [11] "other-data-providers"                       
#> [12] "beta-estimation"                            
#> [13] "univariate-portfolio-sorts"                 
#> [14] "size-sorts-and-p-hacking"                   
#> [15] "value-and-bivariate-sorts"                  
#> [16] "replicating-fama-and-french-factors"        
#> [17] "fama-macbeth-regressions"                   
#> [18] "fixed-effects-and-clustered-standard-errors"
#> [19] "difference-in-differences"                  
#> [20] "factor-selection-via-machine-learning"      
#> [21] "option-pricing-via-machine-learning"        
#> [22] "parametric-portfolio-policies"              
#> [23] "constrained-optimization-and-backtesting"   
#> [24] "wrds-dummy-data"                            
#> [25] "cover-and-logo-design"                      
#> [26] "clean-enhanced-trace"                       
#> [27] "proofs"                                     
#> [28] "colophon"                                   
```
