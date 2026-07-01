# List Parquet Files in a Hugging Face Dataset

Query the Hugging Face Datasets API and return a tibble of files with a
`.parquet` suffix. The function follows pagination links returned in the
response `Link` header and returns path, size, and a resolved URL.

## Usage

``` r
get_available_huggingface_files(organization, dataset)
```

## Arguments

- organization:

  Character(1). Hugging Face organization or user name.

- dataset:

  Character(1). Dataset name under the organization.

## Value

A tibble with columns: `path` (character), `size` (numeric), and `url`
(character).

## Details

Uses httr2 to perform HTTP requests. Requires internet access and the
dataset to be publicly accessible or accessible with appropriate
authentication.

## See also

Other utility functions:
[`create_summary_statistics()`](https://r.tidy-finance.org/reference/create_summary_statistics.md),
[`list_supported_datasets()`](https://r.tidy-finance.org/reference/list_supported_datasets.md),
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
if (FALSE) { # \dontrun{
get_available_huggingface_files("voigtstefan", "sp500")
} # }
```
