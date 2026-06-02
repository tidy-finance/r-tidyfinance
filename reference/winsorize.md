# Winsorize a Numeric Vector

Replaces the values in a numeric vector that are beyond the specified
quantiles with the boundary values of those quantiles. This is done for
both tails of the distribution based on the `cut` parameter.

## Usage

``` r
winsorize(x, cut)
```

## Arguments

- x:

  A numeric vector to be winsorized.

- cut:

  The proportion of data to be winsorized from both ends of the
  distribution. For example, a `cut` of 0.05 will winsorize the lowest
  and highest 5% of the data. Must be in \[0, 0.5\].

## Value

A numeric vector with the extreme values replaced by the corresponding
quantile values.

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
[`list_supported_indexes()`](https://package.tidy-finance.org/reference/list_supported_indexes.md),
[`list_tidy_finance_chapters()`](https://package.tidy-finance.org/reference/list_tidy_finance_chapters.md),
[`open_tidy_finance_website()`](https://package.tidy-finance.org/reference/open_tidy_finance_website.md),
[`trim()`](https://package.tidy-finance.org/reference/trim.md),
[`validate_dates()`](https://package.tidy-finance.org/reference/validate_dates.md)

## Examples

``` r
set.seed(123)
data <- rnorm(100)
winsorized_data <- winsorize(data, 0.05)
```
