# Validate and Coerce Date Range Arguments

Checks that `start_date` and `end_date` are a valid pair, coerces them
to `Date`, and handles the case where both are `NULL`. When both are
`NULL` and `use_default_range = TRUE`, a default range spanning from two
years ago to one year ago is applied and reported to the user.

## Usage

``` r
validate_dates(start_date, end_date, use_default_range = FALSE)
```

## Arguments

- start_date:

  A scalar coercible to `Date` via
  [`as.Date()`](https://rdrr.io/r/base/as.Date.html), or `NULL`.

- end_date:

  A scalar coercible to `Date` via
  [`as.Date()`](https://rdrr.io/r/base/as.Date.html), or `NULL`.

- use_default_range:

  A logical scalar. If `TRUE` and both date arguments are `NULL`, a
  default range spanning from two years ago to one year ago is used
  instead of returning `NULL`. Defaults to `FALSE`.

## Value

A named list with elements `start_date` and `end_date`, both of class
`Date` (or `NULL` when no dates are provided and
`use_default_range = FALSE`).

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
[`winsorize()`](https://r.tidy-finance.org/reference/winsorize.md)

## Examples

``` r
# Explicit range
validate_dates("2020-01-01", "2020-12-31")
#> $start_date
#> [1] "2020-01-01"
#> 
#> $end_date
#> [1] "2020-12-31"
#> 

# Both NULL with the default range applied (two years ago to one year ago)
validate_dates(NULL, NULL, use_default_range = TRUE)
#> No `start_date` or `end_date` provided. Using the range 2024-06-30 to
#> 2025-06-30 to avoid downloading large amounts of data.
#> $start_date
#> [1] "2024-06-30"
#> 
#> $end_date
#> [1] "2025-06-30"
#> 

# Both NULL returns NULL dates
validate_dates(NULL, NULL)
#> No `start_date` or `end_date` provided. Returning the full data set.
#> $start_date
#> NULL
#> 
#> $end_date
#> NULL
#> 
```
