# Estimate a Linear Model

Estimates a linear model specified by one or more independent variables.
It checks for the presence of the specified independent variables in the
dataset and whether the dataset has a sufficient number of observations.
Depending on the `output` parameter, it returns the model's
coefficients, t-statistics, residuals, or any combination in a named
list.

## Usage

``` r
estimate_model(data, model, min_obs = 1, output = "coefficients")
```

## Arguments

- data:

  A data frame containing the dependent variable and one or more
  independent variables.

- model:

  A character that describes the model to be estimated (e.g.,
  `"ret_excess ~ mkt_excess + hml + smb"`).

- min_obs:

  The minimum number of observations required to estimate the model.
  Defaults to 1.

- output:

  A character vector specifying what to return. Must contain one or more
  of `"coefficients"` (default), `"residuals"`, and `"tstats"`. If a
  single value is provided, the corresponding object is returned
  directly. If multiple values are provided, a named list is returned.

## Value

If `output` contains a single value: a data frame of coefficients or
t-statistics, or a numeric vector of residuals. If `output` contains
multiple values: a named list with the requested elements. Coefficients
and t-statistics are returned as data frames with column names
corresponding to the model terms. Residuals are returned as a numeric
vector of length `nrow(data)` with `NA` for rows with missing data or
insufficient observations.

## See also

Other estimation functions:
[`estimate_betas()`](https://package.tidy-finance.org/reference/estimate_betas.md),
[`estimate_fama_macbeth()`](https://package.tidy-finance.org/reference/estimate_fama_macbeth.md)

## Examples

``` r
set.seed(42)
data <- data.frame(
  ret_excess = rnorm(100),
  mkt_excess = rnorm(100),
  smb = rnorm(100),
  hml = rnorm(100)
)

# Estimate model with a single independent variable
estimate_model(data, "ret_excess ~ mkt_excess")
#> # A tibble: 1 × 2
#>   intercept mkt_excess
#>       <dbl>      <dbl>
#> 1    0.0357     0.0360

# Estimate model with multiple independent variables
estimate_model(data, "ret_excess ~ mkt_excess + smb + hml")
#> # A tibble: 1 × 4
#>   intercept mkt_excess    smb    hml
#>       <dbl>      <dbl>  <dbl>  <dbl>
#> 1    0.0325     0.0472 -0.148 0.0799

# Estimate model without intercept
estimate_model(data, "ret_excess ~ mkt_excess - 1")
#> # A tibble: 1 × 1
#>   mkt_excess
#>        <dbl>
#> 1     0.0322

# Calculate residuals
estimate_model(data, "ret_excess ~ mkt_excess + smb + hml",
  output = "residuals"
)
#>   [1]  0.985867948 -0.657836015  0.548339828  0.759314289  0.211109533
#>   [6] -0.309364254  1.355927678 -0.356775376  1.981041072 -0.125591971
#>  [11]  1.101384128  2.611316588 -1.299806903 -0.249512497 -0.060869724
#>  [16]  0.660332286 -0.249364557 -2.610907995 -2.341940738  1.007520793
#>  [21] -0.172762352 -1.811515219 -0.291621196  1.071119784  1.781645300
#>  [26] -0.402542678 -0.275948821 -1.737263588  0.454685847 -0.490335785
#>  [31]  0.296257212  0.869994839  0.886401386 -0.775650256  0.285609870
#>  [36] -1.749702335 -0.500864641 -0.839077487 -2.326943911 -0.091293493
#>  [41]  0.071324540 -0.257416687  0.967751518 -0.483869769 -1.634157102
#>  [46]  0.629288402 -0.763074325  1.562562760 -0.399625786  0.421075632
#>  [51]  0.071196366 -0.669265632  1.252143683  0.721150826  0.307751116
#>  [56]  0.054565149  0.561022214  0.035830440 -3.127779684  0.260812554
#>  [61] -0.194371809  0.433899114  0.637172228  1.137104497 -0.783371236
#>  [66]  1.379522327  0.319450531  1.067056929  0.517616359  0.627244981
#>  [71] -1.109917972 -0.162460741  0.597729669 -0.870463582 -0.790431817
#>  [76]  0.804783460  0.895859442  0.305578132 -0.747201314 -1.096279544
#>  [81]  1.401547113  0.092888556 -0.084579104 -0.184142319 -1.191513021
#>  [86]  0.711576931  0.105833125 -0.410350684  0.515795860  0.850168725
#>  [91]  1.222563642 -0.402192070  0.445832327  1.791178943 -1.093978826
#>  [96] -1.102837577 -1.243221104 -1.410927136 -0.003371234  0.299545399

# Return t-statistics
estimate_model(data, "ret_excess ~ mkt_excess + smb + hml",
  output = "tstats"
)
#> # A tibble: 1 × 4
#>   intercept mkt_excess   smb   hml
#>       <dbl>      <dbl> <dbl> <dbl>
#> 1     0.310      0.406 -1.43 0.667

# Return coefficients, t-statistics, and residuals
estimate_model(data, "ret_excess ~ mkt_excess + smb + hml",
  output = c("coefficients", "tstats", "residuals")
)
#> $coefficients
#> # A tibble: 1 × 4
#>   intercept mkt_excess    smb    hml
#>       <dbl>      <dbl>  <dbl>  <dbl>
#> 1    0.0325     0.0472 -0.148 0.0799
#> 
#> $tstats
#> # A tibble: 1 × 4
#>   intercept mkt_excess   smb   hml
#>       <dbl>      <dbl> <dbl> <dbl>
#> 1     0.310      0.406 -1.43 0.667
#> 
#> $residuals
#>   [1]  0.985867948 -0.657836015  0.548339828  0.759314289  0.211109533
#>   [6] -0.309364254  1.355927678 -0.356775376  1.981041072 -0.125591971
#>  [11]  1.101384128  2.611316588 -1.299806903 -0.249512497 -0.060869724
#>  [16]  0.660332286 -0.249364557 -2.610907995 -2.341940738  1.007520793
#>  [21] -0.172762352 -1.811515219 -0.291621196  1.071119784  1.781645300
#>  [26] -0.402542678 -0.275948821 -1.737263588  0.454685847 -0.490335785
#>  [31]  0.296257212  0.869994839  0.886401386 -0.775650256  0.285609870
#>  [36] -1.749702335 -0.500864641 -0.839077487 -2.326943911 -0.091293493
#>  [41]  0.071324540 -0.257416687  0.967751518 -0.483869769 -1.634157102
#>  [46]  0.629288402 -0.763074325  1.562562760 -0.399625786  0.421075632
#>  [51]  0.071196366 -0.669265632  1.252143683  0.721150826  0.307751116
#>  [56]  0.054565149  0.561022214  0.035830440 -3.127779684  0.260812554
#>  [61] -0.194371809  0.433899114  0.637172228  1.137104497 -0.783371236
#>  [66]  1.379522327  0.319450531  1.067056929  0.517616359  0.627244981
#>  [71] -1.109917972 -0.162460741  0.597729669 -0.870463582 -0.790431817
#>  [76]  0.804783460  0.895859442  0.305578132 -0.747201314 -1.096279544
#>  [81]  1.401547113  0.092888556 -0.084579104 -0.184142319 -1.191513021
#>  [86]  0.711576931  0.105833125 -0.410350684  0.515795860  0.850168725
#>  [91]  1.222563642 -0.402192070  0.445832327  1.791178943 -1.093978826
#>  [96] -1.102837577 -1.243221104 -1.410927136 -0.003371234  0.299545399
#> 
```
