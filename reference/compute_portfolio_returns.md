# Compute Portfolio Returns

Computes individual portfolio returns based on specified sorting
variables and sorting methods. The portfolios can be rebalanced every
period or on an annual frequency by specifying a rebalancing month,
which is only applicable at a monthly return frequency. The function
supports univariate and bivariate sorts, with the latter supporting
dependent and independent sorting methods.

## Usage

``` r
compute_portfolio_returns(
  data,
  sorting_variables,
  sorting_method,
  rebalancing_month = NULL,
  breakpoint_options_main,
  breakpoint_options_secondary = NULL,
  breakpoint_function_main = compute_breakpoints,
  breakpoint_function_secondary = compute_breakpoints,
  min_portfolio_size = 1L,
  cap_weight = 0.8,
  data_options = NULL,
  quiet = FALSE
)
```

## Arguments

- data:

  A data frame containing the dataset for portfolio assignment and
  return computation. The panel data must include individual stock
  identifiers and the time point. It must contain columns for the
  sorting variables and excess returns. Additionally, lagged market
  capitalization is required for value-weighted returns (see parameter
  `data_options`).

- sorting_variables:

  A character vector specifying the column names in `data` to be used
  for sorting and determining portfolio assignments. For univariate
  sorts, provide a single variable. For bivariate sorts, provide two
  variables, where the first string refers to the main variable and the
  second string refers to the secondary ("control") variable.

- sorting_method:

  A string specifying the sorting method to be used. Possible values
  are:

  - `"univariate"`: For a single sorting variable.

  - `"bivariate-dependent"`: For two sorting variables, where the main
    sort depends on the secondary variable.

  - `"bivariate-independent"`: For two independent sorting variables.

  For bivariate sorts, the portfolio returns are averaged over the
  controlling sorting variable (i.e., the second sorting variable), and
  only portfolio returns for the main sorting variable (given as the
  first element of `sorting_variables`) are returned.

- rebalancing_month:

  An integer between 1 and 12 specifying the month in which to form
  portfolios that are held constant for one year. For example, setting
  it to `7` creates portfolios in July that are held constant until June
  of the following year. The default `NULL` corresponds to periodic
  rebalancing.

- breakpoint_options_main:

  A named list of
  [`breakpoint_options()`](https://package.tidy-finance.org/reference/breakpoint_options.md)
  passed to `breakpoint_function_main` for the main sorting variable.

- breakpoint_options_secondary:

  An optional named list of
  [`breakpoint_options()`](https://package.tidy-finance.org/reference/breakpoint_options.md)
  passed to `breakpoint_function_secondary`.

- breakpoint_function_main:

  A function to compute the breakpoints for the main sorting variable.
  The default is set to
  [`compute_breakpoints()`](https://package.tidy-finance.org/reference/compute_breakpoints.md).

- breakpoint_function_secondary:

  A function to compute the breakpoints for the secondary sorting
  variable. The default is set to
  [`compute_breakpoints()`](https://package.tidy-finance.org/reference/compute_breakpoints.md).

- min_portfolio_size:

  An integer specifying the minimum number of firms required in the
  reported portfolio cross-section on a given date (default `1L`, i.e.
  at least one observation per reported portfolio). For both univariate
  and bivariate sorts the threshold is applied to the firm count per
  `(portfolio, date)` of the reported cross-section: for univariate
  sorts that is firms per portfolio-date; for bivariate sorts that is
  firms per main-portfolio-date summed across the secondary buckets.
  Cross-sections below the threshold have their returns set to `NA`. A
  typical value is `5L` (the Fama-French convention). Set to `0L` to
  deactivate the check entirely.

- cap_weight:

  A numeric value between 0 and 1 specifying the percentile at which
  market capitalization is capped per date when computing capped
  value-weighted excess returns (i.e., `ret_excess_vw_capped`). Defaults
  to `0.8`.

- data_options:

  A list of class `tidyfinance_data_options` (created via
  [`data_options()`](https://package.tidy-finance.org/reference/data_options.md))
  specifying column name mappings. The `id` element is used to specify
  the entity (i.e., firm), `date` is used to specify the date column,
  `ret_excess` is used to specify the excess return column, and
  `mktcap_lag` is used to specify the market capitalization. Uses
  [`data_options()`](https://package.tidy-finance.org/reference/data_options.md)
  default if `NULL`: `"id" = "permno"`, `"date" = "date"`,
  `"ret_excess" = "ret_excess"`, and `"mktcap_lag" = "mktcap_lag"`.

- quiet:

  A logical value indicating whether to suppress informational messages
  about missing values in the output panel (default is `FALSE`).

## Value

A data frame with computed portfolio returns as a complete panel (all
portfolio-date combinations), containing the following columns:

- `portfolio`: The portfolio identifier.

- `date`: The date of the portfolio return.

- `ret_excess_vw`: The value-weighted excess return of the portfolio
  (only computed if `data` contains a lagged market capitalization
  column specified by
  [`data_options()`](https://package.tidy-finance.org/reference/data_options.md),
  which defaults to `mktcap_lag`). `NA` if insufficient observations for
  that portfolio-date.

- `ret_excess_ew`: The equal-weighted excess return of the portfolio.
  `NA` if insufficient observations for that portfolio-date.

- `ret_excess_vw_capped`: The capped value-weighted excess return of the
  portfolio (only computed if `data` contains a lagged market
  capitalization column specified by
  [`data_options()`](https://package.tidy-finance.org/reference/data_options.md),
  which defaults to `mktcap_lag`). Weights are computed using market
  capitalization capped at the `cap_weight` percentile per date. `NA` if
  insufficient observations for that portfolio-date.

## Details

The function checks for consistency in the provided arguments. For
univariate sorts, a single sorting variable and a corresponding number
of portfolios must be provided. For bivariate sorts, two sorting
variables and two corresponding numbers of portfolios (or percentiles)
are required. The sorting method determines how portfolios are assigned
and how returns are computed. The function handles missing and extreme
values appropriately based on the specified sorting method and
rebalancing frequency.

## Note

Ensure that `data` contains all required columns: the specified sorting
variables and excess returns (see options and defaults set in
`data_options`). The function will stop and throw an error if any
required columns are missing.

## See also

Other portfolio functions:
[`assign_portfolio()`](https://package.tidy-finance.org/reference/assign_portfolio.md),
[`breakpoint_options()`](https://package.tidy-finance.org/reference/breakpoint_options.md),
[`compute_breakpoints()`](https://package.tidy-finance.org/reference/compute_breakpoints.md),
[`compute_long_short_returns()`](https://package.tidy-finance.org/reference/compute_long_short_returns.md),
[`data_options()`](https://package.tidy-finance.org/reference/data_options.md),
[`filter_options()`](https://package.tidy-finance.org/reference/filter_options.md),
[`filter_sorting_data()`](https://package.tidy-finance.org/reference/filter_sorting_data.md),
[`implement_portfolio_sort()`](https://package.tidy-finance.org/reference/implement_portfolio_sort.md),
[`portfolio_sort_options()`](https://package.tidy-finance.org/reference/portfolio_sort_options.md)

## Examples

``` r
set.seed(42)
# Univariate sorting with periodic rebalancing
data <- data.frame(
  permno = 1:500,
  date = rep(
    seq.Date(
      from = as.Date("2020-01-01"),
      by = "month",
      length.out = 100
    ),
    each = 10
  ),
  mktcap_lag = runif(500, 100, 1000),
  ret_excess = rnorm(500),
  size = runif(500, 50, 150)
)

compute_portfolio_returns(
  data, "size", "univariate",
  breakpoint_options_main = breakpoint_options(n_portfolios = 5)
)
#> # A tibble: 500 × 5
#>    portfolio date       ret_excess_vw ret_excess_ew ret_excess_vw_capped
#>        <int> <date>             <dbl>         <dbl>                <dbl>
#>  1         1 2020-01-01       -1.08         -1.11                -1.08  
#>  2         1 2020-02-01        1.02          1.02                 1.02  
#>  3         1 2020-03-01        0.949         0.678                0.949 
#>  4         1 2020-04-01        0.0696        0.821                0.0696
#>  5         1 2020-05-01       -0.250        -0.489               -0.250 
#>  6         1 2020-06-01        0.307         0.239                0.307 
#>  7         1 2020-07-01        0.196        -0.0990               0.191 
#>  8         1 2020-08-01       -0.0112       -0.0345              -0.0112
#>  9         1 2020-09-01       -0.154        -0.577               -0.171 
#> 10         1 2020-10-01       -1.26         -1.06                -1.26  
#> # ℹ 490 more rows

# Bivariate dependent sorting with annual rebalancing
compute_portfolio_returns(
  data, c("size", "mktcap_lag"), "bivariate-dependent", 7,
  breakpoint_options_main = breakpoint_options(n_portfolios = 5),
  breakpoint_options_secondary = breakpoint_options(n_portfolios = 3),
)
#> Warning: There were 24 warnings in `dplyr::mutate()`.
#> The first warning was:
#> ℹ In argument: `portfolio_main = assign_portfolio(...)`.
#> ℹ In group 1: `date = 2020-07-01`, `portfolio_secondary = 1`.
#> Caused by warning:
#> ! The number of portfolios differs from the specified parameter due to clusters
#> in the sorting variable.
#> ℹ Run `dplyr::last_dplyr_warnings()` to see the 23 remaining warnings.
#> Returning a complete panel with 430 missing values in factor returns due to
#> insufficient observations (fewer than 1 firm per (portfolio, date)
#> cross-section).
#> # A tibble: 470 × 5
#>    portfolio date       ret_excess_vw ret_excess_ew ret_excess_vw_capped
#>        <int> <date>             <dbl>         <dbl>                <dbl>
#>  1         1 2020-07-01        -0.276        -0.276               -0.276
#>  2         1 2020-08-01        NA            NA                   NA    
#>  3         1 2020-09-01        NA            NA                   NA    
#>  4         1 2020-10-01        NA            NA                   NA    
#>  5         1 2020-11-01        NA            NA                   NA    
#>  6         1 2020-12-01        NA            NA                   NA    
#>  7         1 2021-01-01        NA            NA                   NA    
#>  8         1 2021-02-01        NA            NA                   NA    
#>  9         1 2021-03-01        NA            NA                   NA    
#> 10         1 2021-04-01        NA            NA                   NA    
#> # ℹ 460 more rows
```
