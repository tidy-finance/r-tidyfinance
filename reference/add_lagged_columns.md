# Add Lagged Columns via Join

Appends lagged versions of specified columns to a data frame using a
join-based approach.

## Usage

``` r
add_lagged_columns(
  data,
  cols,
  lag,
  max_lag = lag,
  by = NULL,
  drop_na = FALSE,
  ff_adjustment = FALSE,
  data_options = NULL
)
```

## Arguments

- data:

  A data frame containing the variables to lag.

- cols:

  A character vector specifying the names of the columns to be lagged.
  Each column produces a new column suffixed with `_lag`.

- lag:

  An integer or a
  [`lubridate::periods()`](https://lubridate.tidyverse.org/reference/period.html)
  object, e.g., `months(1)`, specifying the minimum lag (inclusive) to
  apply.

- max_lag:

  An integer or a
  [`lubridate::periods()`](https://lubridate.tidyverse.org/reference/period.html)
  object specifying the maximum lag (inclusive) to apply. Defaults to
  `lag` (exact lag).

- by:

  An optional character vector specifying grouping columns (e.g., a
  stock identifier). Lagged values are matched within groups. Defaults
  to `NULL`.

- drop_na:

  A logical value. If `TRUE`, `NA` values in the source columns are
  excluded before matching, so the lookup skips over missing
  observations. Applied independently per column. Defaults to `FALSE`.

- ff_adjustment:

  A logical value. If `TRUE`, only the last observation per year (within
  each group defined by `by`) is retained as a source for lagged values,
  following Fama-French conventions for annual accounting data. Defaults
  to `FALSE`.

- data_options:

  A list of class `tidyfinance_data_options` (created via
  [`data_options()`](https://r.tidy-finance.org/reference/data_options.md))
  specifying column name mappings. The `date` element is used to specify
  the date column. Uses
  [`data_options()`](https://r.tidy-finance.org/reference/data_options.md)
  default if `NULL`: `"date" = "date"`.

## Value

A data frame with the same rows as `data` and new columns appended, each
suffixed with `_lag`. Unmatched rows receive `NA` in the lagged columns.

## Details

When `lag == max_lag` (the default), an equi-join is used: source dates
are shifted forward by `lag` and matched exactly. When `lag < max_lag`,
an inequality join is used: for each row, the most recent source value
within the window `[date - max_lag, date - lag]` is selected.

The combination of `by` and date columns must be unique in `data`. If
`by` is `NULL`, dates alone must be unique.

## See also

Other rolling and lagging functions:
[`compute_rolling_value()`](https://r.tidy-finance.org/reference/compute_rolling_value.md),
[`join_lagged_values()`](https://r.tidy-finance.org/reference/join_lagged_values.md)

## Examples

``` r
set.seed(42)
data <- tibble::tibble(
  permno = rep(1:2, each = 10),
  date = rep(
    seq.Date(as.Date("2023-01-01"), by = "month", length.out = 10),
    2
  ),
  size = runif(20, 100, 200),
  bm = runif(20, 0.5, 1.5)
)

# Exact lag: each row gets the value from exactly 2 months earlier
add_lagged_columns(
  data,
  cols = c("size", "bm"),
  lag = months(2),
  by = "permno"
)
#> # A tibble: 20 × 6
#>    permno date        size    bm size_lag bm_lag
#>     <int> <date>     <dbl> <dbl>    <dbl>  <dbl>
#>  1      1 2023-01-01  191. 1.40       NA  NA    
#>  2      1 2023-02-01  194. 0.639      NA  NA    
#>  3      1 2023-03-01  129. 1.49      191.  1.40 
#>  4      1 2023-04-01  183. 1.45      194.  0.639
#>  5      1 2023-05-01  164. 0.582     129.  1.49 
#>  6      1 2023-06-01  152. 1.01      183.  1.45 
#>  7      1 2023-07-01  174. 0.890     164.  0.582
#>  8      1 2023-08-01  113. 1.41      152.  1.01 
#>  9      1 2023-09-01  166. 0.947     174.  0.890
#> 10      1 2023-10-01  171. 1.34      113.  1.41 
#> 11      2 2023-01-01  146. 1.24       NA  NA    
#> 12      2 2023-02-01  172. 1.31       NA  NA    
#> 13      2 2023-03-01  193. 0.888     146.  1.24 
#> 14      2 2023-04-01  126. 1.19      172.  1.31 
#> 15      2 2023-05-01  146. 0.504     193.  0.888
#> 16      2 2023-06-01  194. 1.33      126.  1.19 
#> 17      2 2023-07-01  198. 0.507     146.  0.504
#> 18      2 2023-08-01  112. 0.708     194.  1.33 
#> 19      2 2023-09-01  147. 1.41      198.  0.507
#> 20      2 2023-10-01  156. 1.11      112.  0.708

# Window lag: each row gets the most recent value from 2 to 4 months earlier
add_lagged_columns(
  data,
  cols = "size",
  lag = months(2),
  max_lag = months(4),
  by = "permno"
)
#> # A tibble: 20 × 5
#>    permno date        size    bm size_lag
#>     <int> <date>     <dbl> <dbl>    <dbl>
#>  1      1 2023-01-01  191. 1.40       NA 
#>  2      1 2023-02-01  194. 0.639      NA 
#>  3      1 2023-03-01  129. 1.49      191.
#>  4      1 2023-04-01  183. 1.45      194.
#>  5      1 2023-05-01  164. 0.582     129.
#>  6      1 2023-06-01  152. 1.01      183.
#>  7      1 2023-07-01  174. 0.890     164.
#>  8      1 2023-08-01  113. 1.41      152.
#>  9      1 2023-09-01  166. 0.947     174.
#> 10      1 2023-10-01  171. 1.34      113.
#> 11      2 2023-01-01  146. 1.24       NA 
#> 12      2 2023-02-01  172. 1.31       NA 
#> 13      2 2023-03-01  193. 0.888     146.
#> 14      2 2023-04-01  126. 1.19      172.
#> 15      2 2023-05-01  146. 0.504     193.
#> 16      2 2023-06-01  194. 1.33      126.
#> 17      2 2023-07-01  198. 0.507     146.
#> 18      2 2023-08-01  112. 0.708     194.
#> 19      2 2023-09-01  147. 1.41      198.
#> 20      2 2023-10-01  156. 1.11      112.
```
