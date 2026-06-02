# Open Tidy Finance Website or Specific Chapter in Browser

Opens the main Tidy Finance website or a specific chapter within the
site in the user's default web browser. If a chapter is specified, the
function constructs the URL to access the chapter directly.

## Usage

``` r
open_tidy_finance_website(chapter = NULL)
```

## Arguments

- chapter:

  An optional character string specifying the chapter to open. If `NULL`
  (the default), the function opens the main page of Tidy Finance
  with R. If a chapter name is provided (e.g., "beta-estimation"), the
  function opens the corresponding chapter's page (e.g.,
  "beta-estimation.html"). If the chapter name does not exist, then the
  function opens the main page.

## Value

Invisible `NULL`. The function is called for its side effect of opening
a web page.

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
[`trim()`](https://package.tidy-finance.org/reference/trim.md),
[`validate_dates()`](https://package.tidy-finance.org/reference/validate_dates.md),
[`winsorize()`](https://package.tidy-finance.org/reference/winsorize.md)

## Examples

``` r
if (FALSE) { # \dontrun{
open_tidy_finance_website()
open_tidy_finance_website("beta-estimation")
} # }
```
