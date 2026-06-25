# Compute a Rolling Value by Period

Applies an arbitrary summary function over rolling time-period windows.
Each window spans `periods` units of `period` (e.g., 12 months). Before
calling `.f`, rows with any missing values are dropped from the window;
if fewer than `min_obs` rows remain, the result is `NA_real_` instead.

## Usage

``` r
compute_rolling_value(
  data,
  .f,
  period = "month",
  periods = 12,
  min_obs = periods,
  data_options = NULL
)
```

## Arguments

- data:

  A data frame with a date column of class `Date`, named according to
  `data_options$date` (default `"date"`).

- .f:

  A function applied to each window. Receives a data-frame slice
  (complete cases only) and must return a single scalar value.

- period:

  A string specifying the period for rolling windows (e.g., `"month"`,
  `"quarter"`, `"year"`).

- periods:

  Number of periods to include in the rolling window.

- min_obs:

  Minimum number of non-missing rows required per window. Defaults to
  `periods`.

- data_options:

  A list of class `tidyfinance_data_options` (created via
  [`data_options()`](https://r.tidy-finance.org/reference/data_options.md))
  specifying column name mappings. The `date` element is used to specify
  the date column. Uses
  [`data_options()`](https://r.tidy-finance.org/reference/data_options.md)
  default if `NULL`: `"date" = "date"`.

## Value

A numeric vector aligned with the rows of `data`.

## See also

Other rolling and lagging functions:
[`add_lagged_columns()`](https://r.tidy-finance.org/reference/add_lagged_columns.md),
[`join_lagged_values()`](https://r.tidy-finance.org/reference/join_lagged_values.md)

## Examples

``` r
library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union

# Rolling standard deviation
set.seed(42)
df <- tibble(
  date = seq.Date(
    from = as.Date("2020-01-01"),
    by = "month",
    length.out = 24
  ),
  value = rnorm(24)
)

df |>
  mutate(
    rolling_sd = compute_rolling_value(
      pick(everything()),
      .f = ~ sd(.x$value, na.rm = TRUE),
      period = "month",
      periods = 4,
      min_obs = 2
    )
  )
#> # A tibble: 24 × 3
#>    date         value rolling_sd
#>    <date>       <dbl>      <dbl>
#>  1 2020-01-01  1.37       NA    
#>  2 2020-02-01 -0.565       1.37 
#>  3 2020-03-01  0.363       0.968
#>  4 2020-04-01  0.633       0.800
#>  5 2020-05-01  0.404       0.529
#>  6 2020-06-01 -0.106       0.310
#>  7 2020-07-01  1.51        0.675
#>  8 2020-08-01 -0.0947      0.760
#>  9 2020-09-01  2.02        1.10 
#> 10 2020-10-01 -0.0627      1.08 
#> # ℹ 14 more rows

# Rolling last residual from a regression
set.seed(42)
df_reg <- tibble(
  date = seq.Date(
    from = as.Date("2020-01-01"),
    by = "month",
    length.out = 60
  ),
  ret_excess = rnorm(60, 0, 0.05),
  mkt_excess = rnorm(60, 0, 0.04),
  smb = rnorm(60, 0, 0.03),
  hml = rnorm(60, 0, 0.03)
)

df_reg |>
  mutate(
    residual = compute_rolling_value(
      pick(everything()),
      .f = \(x) {
        last(lm(ret_excess ~ mkt_excess + smb + hml, data = x)$residuals)
      },
      period = "month",
      periods = 24,
      min_obs = 12
    )
  )
#> # A tibble: 60 × 6
#>    date       ret_excess mkt_excess        smb      hml residual
#>    <date>          <dbl>      <dbl>      <dbl>    <dbl>    <dbl>
#>  1 2020-01-01    0.0685    -0.0147  -0.0448     0.00454       NA
#>  2 2020-02-01   -0.0282     0.00741 -0.0441    -0.0175        NA
#>  3 2020-03-01    0.0182     0.0233   0.00374    0.0111        NA
#>  4 2020-04-01    0.0316     0.0560  -0.0299     0.00884       NA
#>  5 2020-05-01    0.0202    -0.0291  -0.0000547 -0.00838       NA
#>  6 2020-06-01   -0.00531    0.0521  -0.0128    -0.0401        NA
#>  7 2020-07-01    0.0756     0.0134  -0.0184     0.0210        NA
#>  8 2020-08-01   -0.00473    0.0415  -0.0607     0.0166        NA
#>  9 2020-09-01    0.101      0.0368  -0.0367    -0.0251        NA
#> 10 2020-10-01   -0.00314    0.0288   0.00539   -0.0478        NA
#> # ℹ 50 more rows

# Rolling cumulative-return-to-SD ratio
set.seed(42)
df_resid <- tibble(
  date = seq.Date(
    from = as.Date("2020-01-01"),
    by = "month",
    length.out = 24
  ),
  int_roll_residual = rnorm(24, 0, 0.02)
)

df_resid |>
  mutate(
    return_to_sd = compute_rolling_value(
      pick(everything()),
      .f = ~ (prod(1 + .x$int_roll_residual) - 1) / sd(.x$int_roll_residual),
      period = "month",
      periods = 12,
      min_obs = 12
    )
  )
#> # A tibble: 24 × 3
#>    date       int_roll_residual return_to_sd
#>    <date>                 <dbl>        <dbl>
#>  1 2020-01-01           0.0274            NA
#>  2 2020-02-01          -0.0113            NA
#>  3 2020-03-01           0.00726           NA
#>  4 2020-04-01           0.0127            NA
#>  5 2020-05-01           0.00809           NA
#>  6 2020-06-01          -0.00212           NA
#>  7 2020-07-01           0.0302            NA
#>  8 2020-08-01          -0.00189           NA
#>  9 2020-09-01           0.0404            NA
#> 10 2020-10-01          -0.00125           NA
#> # ℹ 14 more rows
```
