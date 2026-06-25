# Estimate Rolling Betas

Estimates rolling betas for a given model using the provided data. It
supports parallel processing for faster computation using the `furrr`
package.

## Usage

``` r
estimate_betas(
  data,
  model,
  lookback,
  min_obs = NULL,
  use_furrr = FALSE,
  data_options = NULL
)
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
  estimate the model. Defaults to 80% of `lookback`.

- use_furrr:

  A logical indicating whether to use the `furrr` package and its
  parallelization capabilities. Defaults to `FALSE`.

- data_options:

  A list of class `tidyfinance_data_options` (created via
  [`data_options()`](https://r.tidy-finance.org/reference/data_options.md))
  specifying column name mappings. The `id` is used to specify the
  entity (i.e., firm), and the `date` element is used to specify the
  date column. Uses
  [`data_options()`](https://r.tidy-finance.org/reference/data_options.md)
  default if `NULL`: `"id" = "permno"` and `"date" = "date"`.

## Value

A data frame with the estimated betas for each time period.

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
#> # A tibble: 600 × 4
#>    permno date       beta_mkt_excess intercept
#>     <int> <date>               <dbl>     <dbl>
#>  1      1 2020-01-01         NA       NA      
#>  2      1 2020-02-01          0.622   -0.140  
#>  3      1 2020-03-01          1.50    -0.0926 
#>  4      1 2020-04-01          2.01    -0.0393 
#>  5      1 2020-05-01         -0.138    0.0143 
#>  6      1 2020-06-01         -0.535   -0.00440
#>  7      1 2020-07-01          0.403    0.0657 
#>  8      1 2020-08-01          0.0607  -0.0744 
#>  9      1 2020-09-01         -0.701   -0.250  
#> 10      1 2020-10-01          1.70     0.0165 
#> # ℹ 590 more rows
estimate_betas(
  data_monthly,
  "ret_excess ~ mkt_excess + smb + hml",
  months(6)
)
#> # A tibble: 600 × 6
#>    permno date       beta_mkt_excess beta_smb beta_hml intercept
#>     <int> <date>               <dbl>    <dbl>    <dbl>     <dbl>
#>  1      1 2020-01-01         NA        NA      NA        NA     
#>  2      1 2020-02-01         NA        NA      NA        NA     
#>  3      1 2020-03-01         NA        NA      NA        NA     
#>  4      1 2020-04-01         NA        NA      NA        NA     
#>  5      1 2020-05-01          0.121    -0.263   0.0772   -0.0465
#>  6      1 2020-06-01          0.0970   -0.188   0.104    -0.0275
#>  7      1 2020-07-01          0.110    -0.426   0.207     0.0160
#>  8      1 2020-08-01          0.300     0.291  -0.0311   -0.0109
#>  9      1 2020-09-01          0.213     0.272  -0.242    -0.0444
#> 10      1 2020-10-01          0.732     0.483  -0.488     0.0475
#> # ℹ 590 more rows

data_monthly |>
  dplyr::rename(id = permno) |>
  estimate_betas("ret_excess ~ mkt_excess", months(3),
                 data_options = data_options(id = "id"))
#> # A tibble: 600 × 4
#>       id date       beta_mkt_excess intercept
#>    <int> <date>               <dbl>     <dbl>
#>  1     1 2020-01-01         NA       NA      
#>  2     1 2020-02-01          0.622   -0.140  
#>  3     1 2020-03-01          1.50    -0.0926 
#>  4     1 2020-04-01          2.01    -0.0393 
#>  5     1 2020-05-01         -0.138    0.0143 
#>  6     1 2020-06-01         -0.535   -0.00440
#>  7     1 2020-07-01          0.403    0.0657 
#>  8     1 2020-08-01          0.0607  -0.0744 
#>  9     1 2020-09-01         -0.701   -0.250  
#> 10     1 2020-10-01          1.70     0.0165 
#> # ℹ 590 more rows

# Estimate monthly betas using daily return data and parallelization
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

# Change settings via future::plan(strategy = "multisession", workers = 4)
estimate_betas(
  data_daily,
  "ret_excess ~ mkt_excess",
  lubridate::days(90),
  use_furrr = TRUE
)
#> # A tibble: 600 × 4
#>    permno date       beta_mkt_excess intercept
#>     <int> <date>               <dbl>     <dbl>
#>  1      1 2020-01-01         NA      NA       
#>  2      1 2020-02-01         NA      NA       
#>  3      1 2020-03-01         -0.179   0.00321 
#>  4      1 2020-04-01         -0.129   0.00201 
#>  5      1 2020-05-01         -0.127   0.00218 
#>  6      1 2020-06-01         -0.0253  0.00427 
#>  7      1 2020-07-01          0.0992  0.00349 
#>  8      1 2020-08-01          0.0152  0.00123 
#>  9      1 2020-09-01         -0.0945  0.000487
#> 10      1 2020-10-01         -0.171  -0.000858
#> # ℹ 590 more rows
```
