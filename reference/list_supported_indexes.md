# List Supported Indexes

Returns a tibble containing information about supported financial
indexes. Each index is associated with a URL that points to a CSV file
containing the holdings of the index. Additionally, each index has a
corresponding `skip` value, which indicates the number of lines to skip
when reading the CSV file.

## Usage

``` r
list_supported_indexes()
```

## Value

A tibble with three columns:

- index:

  The name of the financial index (e.g., "DAX", "S&P 500").

- url:

  The URL to the CSV file containing the holdings data for the index.

- skip:

  The number of lines to skip when reading the CSV file.

## See also

Other utility functions:
[`create_summary_statistics()`](https://package.tidy-finance.org/reference/create_summary_statistics.md),
[`get_available_huggingface_files()`](https://package.tidy-finance.org/reference/get_available_huggingface_files.md),
[`list_supported_datasets()`](https://package.tidy-finance.org/reference/list_supported_datasets.md),
[`list_supported_datasets_ff()`](https://package.tidy-finance.org/reference/list_supported_datasets_ff.md),
[`list_supported_datasets_ff_legacy()`](https://package.tidy-finance.org/reference/list_supported_datasets_ff_legacy.md),
[`list_supported_datasets_macro_predictors()`](https://package.tidy-finance.org/reference/list_supported_datasets_macro_predictors.md),
[`list_supported_datasets_other()`](https://package.tidy-finance.org/reference/list_supported_datasets_other.md),
[`list_supported_datasets_pseudo()`](https://package.tidy-finance.org/reference/list_supported_datasets_pseudo.md),
[`list_supported_datasets_wrds()`](https://package.tidy-finance.org/reference/list_supported_datasets_wrds.md),
[`list_tidy_finance_chapters()`](https://package.tidy-finance.org/reference/list_tidy_finance_chapters.md),
[`open_tidy_finance_website()`](https://package.tidy-finance.org/reference/open_tidy_finance_website.md),
[`trim()`](https://package.tidy-finance.org/reference/trim.md),
[`validate_dates()`](https://package.tidy-finance.org/reference/validate_dates.md),
[`winsorize()`](https://package.tidy-finance.org/reference/winsorize.md)

## Examples

``` r
supported_indexes <- list_supported_indexes()
print(supported_indexes)
#> # A tibble: 20 × 3
#>    index                        url                                         skip
#>    <chr>                        <chr>                                      <dbl>
#>  1 DAX                          https://www.ishares.com/de/privatanleger/…     2
#>  2 EURO STOXX 50                https://www.ishares.com/de/privatanleger/…     2
#>  3 Dow Jones Industrial Average https://www.ishares.com/de/privatanleger/…     2
#>  4 Russell 1000                 https://www.ishares.com/ch/professionelle…     9
#>  5 Russell 2000                 https://www.ishares.com/ch/professionelle…     9
#>  6 Russell 3000                 https://www.ishares.com/ch/professionelle…     9
#>  7 S&P 100                      https://www.ishares.com/ch/professionelle…     9
#>  8 S&P 500                      https://www.ishares.com/de/privatanleger/…     2
#>  9 Nasdaq 100                   https://www.ishares.com/de/privatanleger/…     2
#> 10 FTSE 100                     https://www.ishares.com/de/privatanleger/…     2
#> 11 MSCI World                   https://www.ishares.com/de/privatanleger/…     2
#> 12 Nikkei 225                   https://www.ishares.com/ch/professionelle…     2
#> 13 TOPIX                        https://www.blackrock.com/jp/individual-e…     2
#> 14 STOXX Europe 600             https://www.ishares.com/de/privatanleger/…     2
#> 15 MDAX                         https://www.ishares.com/de/privatanleger/…     2
#> 16 TecDAX                       https://www.ishares.com/de/privatanleger/…     2
#> 17 MSCI Emerging Markets        https://www.ishares.com/de/privatanleger/…     2
#> 18 MSCI Europe                  https://www.ishares.com/de/privatanleger/…     2
#> 19 MSCI ACWI                    https://www.ishares.com/ch/professionelle…     9
#> 20 S&P SmallCap 600             https://www.ishares.com/ch/professionelle…     9
```
