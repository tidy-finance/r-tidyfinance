# Create Summary Statistics for Specified Variables

Computes a set of summary statistics for numeric and integer variables
in a data frame. It allows users to select specific variables for
summarization and can calculate statistics for the whole dataset or
within groups specified by the `by` argument. Additional detail levels
for quantiles can be included.

## Usage

``` r
create_summary_statistics(
  data,
  ...,
  by = NULL,
  detail = FALSE,
  drop_na = FALSE
)
```

## Arguments

- data:

  A data frame containing the variables to be summarized.

- ...:

  Comma-separated list of unquoted variable names in the data frame to
  summarize. These variables must be either numeric, integer, or
  logical.

- by:

  An optional unquoted variable name to group the data before
  summarizing. If `NULL` (the default), summary statistics are computed
  across all observations.

- detail:

  A logical flag indicating whether to compute detailed summary
  statistics, including additional quantiles. Defaults to `FALSE`, which
  computes basic statistics (n, mean, sd, min, median, max). When
  `TRUE`, additional quantiles (1%, 5%, 10%, 25%, 75%, 90%, 95%, 99%)
  are computed.

- drop_na:

  A logical flag indicating whether to drop missing values for each
  variable (default is `FALSE`).

## Value

A tibble with summary statistics for each selected variable. If `by` is
specified, the output includes the grouping variable as well. Each row
represents a variable (and a group if `by` is used), and each column
contains the computed statistics.

## Details

The function first checks that all specified variables are of type
numeric, integer, or logical. If any variables do not meet this
criterion, the function stops and returns an error message indicating
the non-conforming variables.

The basic set of summary statistics includes the count of non-NA values
(n), mean, standard deviation (sd), minimum (min), median (q50), and
maximum (max). If `detail` is `TRUE`, the function also computes the
1st, 5th, 10th, 25th, 75th, 90th, 95th, and 99th percentiles.

Summary statistics are computed for each variable specified in `...`. If
a `by` variable is provided, statistics are computed within each level
of the `by` variable.

## See also

Other utility functions:
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
[`list_tidy_finance_chapters()`](https://r.tidy-finance.org/reference/list_tidy_finance_chapters.md),
[`open_tidy_finance_website()`](https://r.tidy-finance.org/reference/open_tidy_finance_website.md),
[`trim()`](https://r.tidy-finance.org/reference/trim.md),
[`validate_dates()`](https://r.tidy-finance.org/reference/validate_dates.md),
[`winsorize()`](https://r.tidy-finance.org/reference/winsorize.md)

## Examples

``` r
data <- data.frame(
  ret = c(0.01, -0.02, 0.03, NA, 0.005),
  size = c(100, 200, 150, 300, 250),
  group = c("A", "A", "B", "B", "A")
)

# Basic summary across all observations
create_summary_statistics(data, ret, size)
#> # A tibble: 2 × 7
#>   variable     n  mean    sd   min   q50   max
#>   <chr>    <int> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 ret          4    NA  NA      NA    NA    NA
#> 2 size         5   200  79.1   100   200   300

# Grouped summary
create_summary_statistics(data, ret, size, by = group)
#> # A tibble: 4 × 8
#>   variable group     n      mean       sd    min     q50    max
#>   <chr>    <chr> <int>     <dbl>    <dbl>  <dbl>   <dbl>  <dbl>
#> 1 ret      A         3  -0.00167   0.0161  -0.02   0.005   0.01
#> 2 ret      B         1  NA        NA       NA     NA      NA   
#> 3 size     A         3 183.       76.4    100    200     250   
#> 4 size     B         2 225       106.     150    225     300   

# Detailed quantiles
create_summary_statistics(data, ret, detail = TRUE)
#> # A tibble: 1 × 15
#>   variable     n  mean    sd   min   q01   q05   q10   q25   q50   q75   q90
#>   <chr>    <int> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 ret          4    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA
#> # ℹ 3 more variables: q95 <dbl>, q99 <dbl>, max <dbl>
```
