# List Supported Global Q Datasets

Returns a tibble with the supported Global Q datasets, including their
names and frequencies (daily, weekly, weekly week-to-week, monthly,
quarterly, annual). Each dataset type is associated with the Global Q
model, specifically the q5 factors model for the year 2023.
Additionally, it annotates each dataset with the domain "Global Q".

## Usage

``` r
list_supported_datasets_q()
```

## Value

A tibble with columns: `type` (the type of dataset), `dataset_name` (the
file name of the dataset), and `domain` (the domain to which the dataset
belongs, always "Global Q").
