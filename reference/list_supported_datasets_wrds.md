# List Supported WRDS Datasets

Returns a tibble with the supported datasets provided via WRDS.
Additionally, it annotates each dataset with the domain "WRDS".

## Usage

``` r
list_supported_datasets_wrds()
```

## Value

A tibble with columns: `type` (the type of dataset), `dataset_name` (the
file name of the dataset), and `domain` (the domain to which the dataset
belongs, always "WRDS").

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
[`list_supported_indexes()`](https://package.tidy-finance.org/reference/list_supported_indexes.md),
[`list_tidy_finance_chapters()`](https://package.tidy-finance.org/reference/list_tidy_finance_chapters.md),
[`open_tidy_finance_website()`](https://package.tidy-finance.org/reference/open_tidy_finance_website.md),
[`trim()`](https://package.tidy-finance.org/reference/trim.md),
[`validate_dates()`](https://package.tidy-finance.org/reference/validate_dates.md),
[`winsorize()`](https://package.tidy-finance.org/reference/winsorize.md)
