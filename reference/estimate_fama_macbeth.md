# Estimate Fama-MacBeth Regressions

Estimates Fama-MacBeth regressions (Fama and MacBeth, 1973) by first
running cross-sectional regressions for each time period and then
aggregating the results over time to obtain average risk premia and
corresponding t-statistics.

## Usage

``` r
estimate_fama_macbeth(
  data,
  model,
  vcov = "newey-west",
  vcov_options = NULL,
  data_options = NULL,
  detail = FALSE
)
```

## Arguments

- data:

  A data frame containing the data for the regression. It must include a
  column representing the time periods (defaults to `date`) and the
  variables specified in the `model`.

- model:

  A character string describing the model to be estimated in each
  cross-section (e.g., `"ret_excess ~ beta + bm + log_mktcap"`).

- vcov:

  A character string indicating the type of standard errors to compute.
  Options are `"iid"` for independent and identically distributed errors
  or `"newey-west"` for Newey-West standard errors. Default is
  `"newey-west"`.

- vcov_options:

  A list of additional arguments to be passed to the `NeweyWest()`
  function when `vcov = "newey-west"`. These can include options such as
  `lag`, which specifies the number of lags to use in the Newey-West
  covariance matrix estimation, and `prewhite`, which indicates whether
  to apply a prewhitening transformation. Default is an empty list.

- data_options:

  A list of class `tidyfinance_data_options` (created via
  [`data_options()`](https://package.tidy-finance.org/reference/data_options.md))
  specifying column name mappings. The `date` element is used to specify
  the date column. Uses
  [`data_options()`](https://package.tidy-finance.org/reference/data_options.md)
  default if `NULL`: `"date" = "date"`.

- detail:

  A logical value indicating whether to return additional summary
  statistics. If `FALSE` (default), the function returns only the
  coefficient estimates. If `TRUE`, it returns a list with two elements:
  `coefficients` (the usual estimates table) and `summary_statistics` (a
  one-row tibble with the average cross-sectional R-squared and the
  average number of observations per cross-section).

## Value

If `detail = FALSE` (default), a tibble with columns `factor`,
`risk_premium`, `n` (number of time periods), `standard_error`, and
`t_statistic`.

If `detail = TRUE`, a named list with two elements:

- coefficients:

  The same tibble described above.

- summary_statistics:

  A one-row tibble with `r_squared` (mean cross-sectional R-squared) and
  `n_obs` (mean cross-sectional observation count).

## References

Fama, E. F., & MacBeth, J. D. (1973). Risk, return, and equilibrium:
Empirical tests. *Journal of Political Economy*, 81(3), 607-636.
[doi:10.1086/260061](https://doi.org/10.1086/260061)

Newey, W. K., & West, K. D. (1987). A simple, positive semi-definite,
heteroskedasticity and autocorrelation consistent covariance matrix.
*Econometrica*, 55(3), 703-708.
[doi:10.2307/1913610](https://doi.org/10.2307/1913610)

## See also

Other estimation functions:
[`estimate_betas()`](https://package.tidy-finance.org/reference/estimate_betas.md),
[`estimate_model()`](https://package.tidy-finance.org/reference/estimate_model.md)

## Examples

``` r
set.seed(1234)

data <- tibble::tibble(
  date = rep(seq.Date(from = as.Date("2020-01-01"),
                      to = as.Date("2020-12-01"), by = "month"), each = 50),
  permno = rep(1:50, times = 12),
  ret_excess = rnorm(600, 0, 0.1),
  beta = rnorm(600, 1, 0.2),
  bm = rnorm(600, 0.5, 0.1),
  log_mktcap = rnorm(600, 10, 1)
)

estimate_fama_macbeth(data, "ret_excess ~ beta + bm + log_mktcap")
#> # A tibble: 4 × 5
#>   factor     risk_premium     n standard_error t_statistic
#>   <chr>             <dbl> <dbl>          <dbl>       <dbl>
#> 1 intercept       0.0114     12        0.0362       0.314 
#> 2 beta            0.00175    12        0.0196       0.0893
#> 3 bm              0.0712     12        0.0132       5.39  
#> 4 log_mktcap     -0.00505    12        0.00318     -1.59  
estimate_fama_macbeth(
  data,
  "ret_excess ~ beta + bm + log_mktcap",
  vcov = "iid"
)
#> # A tibble: 4 × 5
#>   factor     risk_premium     n standard_error t_statistic
#>   <chr>             <dbl> <dbl>          <dbl>       <dbl>
#> 1 intercept       0.0114     12        0.0647        0.610
#> 2 beta            0.00175    12        0.0222        0.274
#> 3 bm              0.0712     12        0.0386        6.40 
#> 4 log_mktcap     -0.00505    12        0.00528      -3.31 
estimate_fama_macbeth(
  data,
  "ret_excess ~ beta + bm + log_mktcap",
  vcov = "newey-west",
  vcov_options = list(lag = 6, prewhite = FALSE)
)
#> # A tibble: 4 × 5
#>   factor     risk_premium     n standard_error t_statistic
#>   <chr>             <dbl> <dbl>          <dbl>       <dbl>
#> 1 intercept       0.0114     12        0.0324        0.351
#> 2 beta            0.00175    12        0.0145        0.121
#> 3 bm              0.0712     12        0.0205        3.47 
#> 4 log_mktcap     -0.00505    12        0.00403      -1.26 

# Return detailed output including R-squared and observation counts
estimate_fama_macbeth(
  data,
  "ret_excess ~ beta + bm + log_mktcap",
  detail = TRUE
)
#> $coefficients
#> # A tibble: 4 × 5
#>   factor     risk_premium     n standard_error t_statistic
#>   <chr>             <dbl> <dbl>          <dbl>       <dbl>
#> 1 intercept       0.0114     12        0.0362       0.314 
#> 2 beta            0.00175    12        0.0196       0.0893
#> 3 bm              0.0712     12        0.0132       5.39  
#> 4 log_mktcap     -0.00505    12        0.00318     -1.59  
#> 
#> $summary_statistics
#> # A tibble: 1 × 3
#>   r_squared adj_r_squared n_obs
#>       <dbl>         <dbl> <dbl>
#> 1    0.0761        0.0159    50
#> 

# Use different column name for date
data |>
  dplyr::rename(month = date) |>
  estimate_fama_macbeth(
    "ret_excess ~ beta + bm + log_mktcap",
    data_options = data_options(date = "month")
 )
#> # A tibble: 4 × 5
#>   factor     risk_premium     n standard_error t_statistic
#>   <chr>             <dbl> <dbl>          <dbl>       <dbl>
#> 1 intercept       0.0114     12        0.0362       0.314 
#> 2 beta            0.00175    12        0.0196       0.0893
#> 3 bm              0.0712     12        0.0132       5.39  
#> 4 log_mktcap     -0.00505    12        0.00318     -1.59  
```
