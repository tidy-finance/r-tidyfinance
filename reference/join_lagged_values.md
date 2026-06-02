# Join Lagged Variable Values over a Date Range

Joins lagged values of selected variables from one dataset (`new_data`)
into another (`original_data`), based on date ranges defined by
`min_lag` and `max_lag`. Unlike
[`add_lagged_columns()`](https://package.tidy-finance.org/reference/add_lagged_columns.md),
this function supports joining across data frames with different date
grids (e.g., monthly source data into quarterly target data).

## Usage

``` r
join_lagged_values(
  original_data,
  new_data,
  id_keys,
  min_lag,
  max_lag,
  ff_adjustment = FALSE,
  data_options = NULL
)
```

## Arguments

- original_data:

  A data frame containing the target panel data.

- new_data:

  A data frame containing the source variables to lag and merge. All
  columns besides `id_keys` and the date column will be lagged and
  joined.

- id_keys:

  A character vector specifying the identifier column(s).

- min_lag:

  A `lubridate::Period` specifying the lower lag bound (inclusive).

- max_lag:

  A `lubridate::Period` specifying the upper lag bound (inclusive).

- ff_adjustment:

  Logical; if `TRUE`, keeps only the last observation per identifier and
  year before lagging (Fama-French convention). Defaults to `FALSE`.

- data_options:

  A list of class `tidyfinance_data_options` (created via
  [`data_options()`](https://package.tidy-finance.org/reference/data_options.md))
  specifying column name mappings. The `date` element is used to
  identify the date column. Uses
  [`data_options()`](https://package.tidy-finance.org/reference/data_options.md)
  default if `NULL`: `"date" = "date"`.

## Value

A data frame with all columns from `original_data` plus the lagged
columns from `new_data` (keeping their original names).

## See also

Other rolling and lagging functions:
[`add_lagged_columns()`](https://package.tidy-finance.org/reference/add_lagged_columns.md),
[`compute_rolling_value()`](https://package.tidy-finance.org/reference/compute_rolling_value.md)

## Examples

``` r
set.seed(42)
library(dplyr)
library(lubridate)
#> 
#> Attaching package: ‘lubridate’
#> The following objects are masked from ‘package:base’:
#> 
#>     date, intersect, setdiff, union

df1 <- tibble(
  id = rep(1:2, each = 6),
  date = rep(seq(as.Date("2020-01-01"), by = "month", length.out = 6), 2)
)

df2 <- df1 |>
  mutate(x = rnorm(n()))

join_lagged_values(
  original_data = df1,
  new_data = df2,
  id_keys = "id",
  min_lag = months(1),
  max_lag = months(3)
)
#> # A tibble: 12 × 3
#>       id date             x
#>    <int> <date>       <dbl>
#>  1     1 2020-01-01 NA     
#>  2     1 2020-02-01  1.37  
#>  3     1 2020-03-01 -0.565 
#>  4     1 2020-04-01  0.363 
#>  5     1 2020-05-01  0.633 
#>  6     1 2020-06-01  0.404 
#>  7     2 2020-01-01 NA     
#>  8     2 2020-02-01  1.51  
#>  9     2 2020-03-01 -0.0947
#> 10     2 2020-04-01  2.02  
#> 11     2 2020-05-01 -0.0627
#> 12     2 2020-06-01  1.30  
```
