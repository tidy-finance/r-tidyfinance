# Estimate Rolling Betas

Estimates rolling betas for a given linear model using a fast,
vectorized approach. Instead of fitting one regression per stock and
estimation window, the function collapses the data to additive cumulants
(the entries of the moment matrices \\X'X\\ and \\X'y\\) per stock and
period, aggregates these cumulants over rolling calendar windows in a
single pass with
[`slider::slide_index_sum()`](https://slider.r-lib.org/reference/summary-index.html),
and recovers the coefficients via the closed-form OLS solution
\\\hat\beta = (X'X)^{-1} X'y\\. This produces estimates that are
numerically identical to a per-window regression, but fast enough that
no per-stock nesting or parallelization is required.

## Usage

``` r
estimate_betas(data, model, lookback, min_obs = NULL, data_options = NULL)
```

## Arguments

- data:

  A data frame containing the data with a date identifier (defaults to
  `date`), a stock identifier (defaults to `permno`), and other
  variables used in the model.

- model:

  A character string describing the model to be estimated (e.g.,
  `"ret_excess ~ mkt_excess + hml + smb"`).

- lookback:

  A Period object specifying the number of months, days, hours, minutes,
  or seconds to look back when estimating the rolling model.

- min_obs:

  An integer specifying the minimum number of observations required to
  estimate the model. Defaults to 80% of `lookback`. Windows with fewer
  observations are dropped from the output.

- data_options:

  A list of class `tidyfinance_data_options` (created via
  [`data_options()`](https://r.tidy-finance.org/reference/data_options.md))
  specifying column name mappings. The `id` is used to specify the
  entity (i.e., firm), and the `date` element is used to specify the
  date column. Uses
  [`data_options()`](https://r.tidy-finance.org/reference/data_options.md)
  default if `NULL`: `"id" = "permno"` and `"date" = "date"`.

## Value

A data frame with the estimated betas for each entity and period. It
contains the entity and date identifiers, an `intercept` column (if the
model includes one), and one `beta_<variable>` column per regressor.

## See also

Other estimation functions:
[`estimate_fama_macbeth()`](https://r.tidy-finance.org/reference/estimate_fama_macbeth.md),
[`estimate_model()`](https://r.tidy-finance.org/reference/estimate_model.md)

## Examples

``` r
# Estimate monthly betas using monthly return data
set.seed(1234)
data_monthly <- tibble::tibble(
  date = rep(seq.Date(from = as.Date("2020-01-01"),
                      to = as.Date("2020-12-01"), by = "month"), each = 50),
  permno = rep(1:50, times = 12),
  ret_excess = rnorm(600, 0, 0.1),
  mkt_excess = rnorm(600, 0, 0.1),
  smb = rnorm(600, 0, 0.1),
  hml = rnorm(600, 0, 0.1),
)

estimate_betas(data_monthly,  "ret_excess ~ mkt_excess", months(3))
#> # A tibble: 550 × 4
#>    permno date       intercept beta_mkt_excess
#>     <int> <date>         <dbl>           <dbl>
#>  1      1 2020-02-01  -0.140            0.622 
#>  2      1 2020-03-01  -0.0926           1.50  
#>  3      1 2020-04-01  -0.0393           2.01  
#>  4      1 2020-05-01   0.0143          -0.138 
#>  5      1 2020-06-01  -0.00440         -0.535 
#>  6      1 2020-07-01   0.0657           0.403 
#>  7      1 2020-08-01  -0.0744           0.0607
#>  8      1 2020-09-01  -0.250           -0.701 
#>  9      1 2020-10-01   0.0165           1.70  
#> 10      1 2020-11-01   0.0764           1.66  
#> # ℹ 540 more rows
estimate_betas(
  data_monthly,
  "ret_excess ~ mkt_excess + smb + hml",
  months(6)
)
#> # A tibble: 400 × 6
#>    permno date       intercept beta_mkt_excess beta_smb beta_hml
#>     <int> <date>         <dbl>           <dbl>    <dbl>    <dbl>
#>  1      1 2020-05-01  -0.0465           0.121   -0.263    0.0772
#>  2      1 2020-06-01  -0.0275           0.0970  -0.188    0.104 
#>  3      1 2020-07-01   0.0160           0.110   -0.426    0.207 
#>  4      1 2020-08-01  -0.0109           0.300    0.291   -0.0311
#>  5      1 2020-09-01  -0.0444           0.213    0.272   -0.242 
#>  6      1 2020-10-01   0.0475           0.732    0.483   -0.488 
#>  7      1 2020-11-01   0.0511           1.02     0.895    0.0611
#>  8      1 2020-12-01  -0.0305           0.506    0.461    0.858 
#>  9      2 2020-05-01  -0.00399         -0.164   -0.0171  -0.420 
#> 10      2 2020-06-01   0.0146          -0.248    0.0451   0.269 
#> # ℹ 390 more rows

data_monthly |>
  dplyr::rename(id = permno) |>
  estimate_betas("ret_excess ~ mkt_excess", months(3),
                 data_options = data_options(id = "id"))
#> # A tibble: 550 × 4
#>       id date       intercept beta_mkt_excess
#>    <int> <date>         <dbl>           <dbl>
#>  1     1 2020-02-01  -0.140            0.622 
#>  2     1 2020-03-01  -0.0926           1.50  
#>  3     1 2020-04-01  -0.0393           2.01  
#>  4     1 2020-05-01   0.0143          -0.138 
#>  5     1 2020-06-01  -0.00440         -0.535 
#>  6     1 2020-07-01   0.0657           0.403 
#>  7     1 2020-08-01  -0.0744           0.0607
#>  8     1 2020-09-01  -0.250           -0.701 
#>  9     1 2020-10-01   0.0165           1.70  
#> 10     1 2020-11-01   0.0764           1.66  
#> # ℹ 540 more rows

# Estimate monthly betas using daily return data
data_daily <- tibble::tibble(
  date = rep(seq.Date(from = as.Date("2020-01-01"),
                      to = as.Date("2020-12-31"), by = "day"), each = 50),
  permno = rep(1:50, times = 366),
  ret_excess = rnorm(18300, 0, 0.02),
  mkt_excess = rnorm(18300, 0, 0.02),
  smb = rnorm(18300, 0, 0.02),
  hml = rnorm(18300, 0, 0.02),
)

data_daily <- data_daily |>
  dplyr::mutate(date = lubridate::floor_date(date, "month"))

estimate_betas(
  data_daily,
  "ret_excess ~ mkt_excess",
  lubridate::days(90)
)
#> # A tibble: 500 × 4
#>    permno date       intercept beta_mkt_excess
#>     <int> <date>         <dbl>           <dbl>
#>  1      1 2020-03-01  0.00321          -0.179 
#>  2      1 2020-04-01  0.00201          -0.129 
#>  3      1 2020-05-01  0.00218          -0.127 
#>  4      1 2020-06-01  0.00427          -0.0253
#>  5      1 2020-07-01  0.00349           0.0992
#>  6      1 2020-08-01  0.00123           0.0152
#>  7      1 2020-09-01  0.000487         -0.0945
#>  8      1 2020-10-01 -0.000858         -0.171 
#>  9      1 2020-11-01  0.000393         -0.112 
#> 10      1 2020-12-01  0.00102          -0.0485
#> # ℹ 490 more rows
```
