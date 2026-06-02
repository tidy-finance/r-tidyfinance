# List Supported Fama-French Datasets

Returns a tibble with the supported Fama-French datasets, including
their names and frequencies (daily, weekly, monthly). Each dataset type
is associated with a specific Fama-French model (e.g., 3 factors, 5
factors). Additionally, it annotates each dataset with the domain
"Fama-French".

## Usage

``` r
list_supported_datasets_ff()
```

## Value

A tibble with columns: `type` (the type of dataset), `dataset_name` (a
descriptive name of the dataset), `file_url` (the path of the source ZIP
relative to Kenneth French's data library), and `domain` (the domain to
which the dataset belongs, always "Fama-French").

## See also

Other utility functions:
[`create_summary_statistics()`](https://package.tidy-finance.org/reference/create_summary_statistics.md),
[`get_available_huggingface_files()`](https://package.tidy-finance.org/reference/get_available_huggingface_files.md),
[`list_supported_datasets()`](https://package.tidy-finance.org/reference/list_supported_datasets.md),
[`list_supported_datasets_ff_legacy()`](https://package.tidy-finance.org/reference/list_supported_datasets_ff_legacy.md),
[`list_supported_datasets_macro_predictors()`](https://package.tidy-finance.org/reference/list_supported_datasets_macro_predictors.md),
[`list_supported_datasets_other()`](https://package.tidy-finance.org/reference/list_supported_datasets_other.md),
[`list_supported_datasets_pseudo()`](https://package.tidy-finance.org/reference/list_supported_datasets_pseudo.md),
[`list_supported_datasets_wrds()`](https://package.tidy-finance.org/reference/list_supported_datasets_wrds.md),
[`list_supported_indexes()`](https://package.tidy-finance.org/reference/list_supported_indexes.md),
[`list_tidy_finance_chapters()`](https://package.tidy-finance.org/reference/list_tidy_finance_chapters.md),
[`open_tidy_finance_website()`](https://package.tidy-finance.org/reference/open_tidy_finance_website.md),
[`trim()`](https://package.tidy-finance.org/reference/trim.md),
[`validate_dates()`](https://package.tidy-finance.org/reference/validate_dates.md),
[`winsorize()`](https://package.tidy-finance.org/reference/winsorize.md)
