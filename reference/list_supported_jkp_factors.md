# List Supported Global Factor Data Regions and Factors

Queries the live availability manifest of [Global Factor
Data](https://jkpfactors.com/data) and returns the regions and selectors
that can be passed to
[`download_data_jkp()`](https://r.tidy-finance.org/reference/download_data_jkp.md).

## Usage

``` r
list_supported_jkp_factors(region = NULL, dataset = "factors")
```

## Arguments

- region:

  Optional. A region or country code. If provided, the function returns
  the selectors (factor codes, or industry classifications when
  `dataset = "industry"`) available for that region. If `NULL` (the
  default), it returns the available region codes.

- dataset:

  The Global Factor Data product to query, one of `"factors"` (default),
  `"portfolios"`, or `"industry"`.

## Value

A tibble. When `region` is `NULL`, it has a single `region` column
listing the available region codes. When `region` is provided, it has a
`region` column and a `factor` column listing the selectors (factor
codes, or industry classifications when `dataset = "industry"`)
available for that region.

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
[`list_tidy_finance_chapters()`](https://r.tidy-finance.org/reference/list_tidy_finance_chapters.md),
[`open_tidy_finance_website()`](https://r.tidy-finance.org/reference/open_tidy_finance_website.md),
[`trim()`](https://r.tidy-finance.org/reference/trim.md),
[`validate_dates()`](https://r.tidy-finance.org/reference/validate_dates.md),
[`winsorize()`](https://r.tidy-finance.org/reference/winsorize.md)

## Examples

``` r
# \donttest{
  list_supported_jkp_factors()
#> # A tibble: 102 × 1
#>    region       
#>    <chr>        
#>  1 all_countries
#>  2 all_regions  
#>  3 are          
#>  4 arg          
#>  5 aus          
#>  6 aut          
#>  7 bel          
#>  8 bgd          
#>  9 bgr          
#> 10 bhr          
#> # ℹ 92 more rows
  list_supported_jkp_factors("usa")
#> # A tibble: 169 × 2
#>    region factor     
#>    <chr>  <chr>      
#>  1 usa    accruals   
#>  2 usa    age        
#>  3 usa    aliq_at    
#>  4 usa    aliq_mat   
#>  5 usa    all_factors
#>  6 usa    all_themes 
#>  7 usa    ami_126d   
#>  8 usa    at_be      
#>  9 usa    at_gr1     
#> 10 usa    at_me      
#> # ℹ 159 more rows
  list_supported_jkp_factors("usa", dataset = "portfolios")
#> # A tibble: 153 × 2
#>    region factor     
#>    <chr>  <chr>      
#>  1 usa    age        
#>  2 usa    aliq_at    
#>  3 usa    aliq_mat   
#>  4 usa    ami_126d   
#>  5 usa    at_be      
#>  6 usa    at_gr1     
#>  7 usa    at_me      
#>  8 usa    at_turnover
#>  9 usa    be_gr1a    
#> 10 usa    be_me      
#> # ℹ 143 more rows
# }
```
