# Implement Portfolio Sort

A convenience wrapper that combines sample construction filtering and
portfolio return computation into a single call. Equivalent to calling
[`filter_sorting_data()`](https://package.tidy-finance.org/reference/filter_sorting_data.md)
followed by
[`compute_portfolio_returns()`](https://package.tidy-finance.org/reference/compute_portfolio_returns.md).

## Usage

``` r
implement_portfolio_sort(
  data,
  sorting_variables,
  sorting_method,
  rebalancing_month = NULL,
  portfolio_sort_options,
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

  A data frame containing the stock-level panel data.

- sorting_variables:

  A character vector of one or two column names to sort portfolios on.

- sorting_method:

  A string specifying the sorting method: `"univariate"`,
  `"bivariate-dependent"`, or `"bivariate-independent"`.

- rebalancing_month:

  An optional integer specifying the month in which portfolios are
  rebalanced annually. `NULL` (the default) means monthly rebalancing.

- portfolio_sort_options:

  A list of class `tidyfinance_portfolio_sort_options` created by
  [`portfolio_sort_options()`](https://package.tidy-finance.org/reference/portfolio_sort_options.md),
  bundling filter and breakpoint specifications. The arguments accepted
  by
  [`portfolio_sort_options()`](https://package.tidy-finance.org/reference/portfolio_sort_options.md)
  include

  - `filter_options` A list of class `tidyfinance_filter_options`
    created by
    [`filter_options()`](https://package.tidy-finance.org/reference/filter_options.md),
    or `NULL` (the default, which applies no filters). Options include
    `exclude_financials`, `exclude_utilities`, `min_stock_price`,
    `min_size_quantile`, `min_listing_age`,
    `exclude_negative_book_equity`, and `exclude_negative_earnings`.

  - `breakpoint_options_main` A list of class
    `tidyfinance_breakpoint_options` created by
    [`breakpoint_options()`](https://package.tidy-finance.org/reference/breakpoint_options.md),
    specifying breakpoints for the primary sorting variable. Options
    include `n_portfolios`, `percentiles`, `breakpoints_exchanges`,
    `smooth_bunching`, and `breakpoints_min_size_threshold`.

  - `breakpoint_options_secondary` A list of class
    `tidyfinance_breakpoint_options` created by
    [`breakpoint_options()`](https://package.tidy-finance.org/reference/breakpoint_options.md),
    specifying breakpoints for the secondary sorting variable, or `NULL`
    (the default) for univariate sorts. Options are the same as for
    `breakpoint_options_main`.

- breakpoint_function_main:

  The function used to compute breakpoints for the main sorting
  variable. Defaults to
  [`compute_breakpoints()`](https://package.tidy-finance.org/reference/compute_breakpoints.md).

- breakpoint_function_secondary:

  The function used to compute breakpoints for the secondary sorting
  variable. Defaults to
  [`compute_breakpoints()`](https://package.tidy-finance.org/reference/compute_breakpoints.md).

- min_portfolio_size:

  An integer specifying the minimum number of firms in the reported
  portfolio cross-section on a given date. Defaults to `1L` (at least
  one observation per reported portfolio). For univariate sorts that is
  firms per portfolio-date; for bivariate sorts that is firms per
  main-portfolio-date summed across the secondary buckets.
  Cross-sections below the threshold have their returns set to `NA`. Set
  to `0L` to deactivate the check. See
  [`compute_portfolio_returns()`](https://package.tidy-finance.org/reference/compute_portfolio_returns.md).

- cap_weight:

  A numeric between 0 and 1 specifying the quantile at which portfolio
  weights are capped for the capped value-weighted return. Defaults to
  `0.8`.

- data_options:

  A list of class `tidyfinance_data_options` (created via
  [`data_options()`](https://package.tidy-finance.org/reference/data_options.md))
  specifying column name mappings. All elements are forwarded to
  [`filter_sorting_data()`](https://package.tidy-finance.org/reference/filter_sorting_data.md)
  and
  [`compute_portfolio_returns()`](https://package.tidy-finance.org/reference/compute_portfolio_returns.md).
  Uses
  [`data_options()`](https://package.tidy-finance.org/reference/data_options.md)
  default if `NULL`: `"id" = "permno"`, `"date" = "date"`,
  `"exchange" = "exchange"`, `"mktcap_lag" = "mktcap_lag"`,
  `"ret_excess" = "ret_excess"`, `"siccd" = "siccd"`,
  `"price" = "prc_adj"`, `"listing_age" = "listing_age"`, `"be" = "be"`,
  and `"earnings" = "ib"`.

- quiet:

  A logical indicating whether informational messages should be
  suppressed. Defaults to `FALSE`.

## Value

A data frame of portfolio returns as returned by
[`compute_portfolio_returns()`](https://package.tidy-finance.org/reference/compute_portfolio_returns.md).

## See also

Other portfolio functions:
[`assign_portfolio()`](https://package.tidy-finance.org/reference/assign_portfolio.md),
[`breakpoint_options()`](https://package.tidy-finance.org/reference/breakpoint_options.md),
[`compute_breakpoints()`](https://package.tidy-finance.org/reference/compute_breakpoints.md),
[`compute_long_short_returns()`](https://package.tidy-finance.org/reference/compute_long_short_returns.md),
[`compute_portfolio_returns()`](https://package.tidy-finance.org/reference/compute_portfolio_returns.md),
[`data_options()`](https://package.tidy-finance.org/reference/data_options.md),
[`filter_options()`](https://package.tidy-finance.org/reference/filter_options.md),
[`filter_sorting_data()`](https://package.tidy-finance.org/reference/filter_sorting_data.md),
[`portfolio_sort_options()`](https://package.tidy-finance.org/reference/portfolio_sort_options.md)

## Examples

``` r

set.seed(123)
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
  prc_adj = runif(500, 0.5, 50),
  size = runif(500, 50, 150)
)
implement_portfolio_sort(
  data = data,
  sorting_variables = "size",
  sorting_method = "univariate",
  portfolio_sort_options = portfolio_sort_options(
    filter_options = filter_options(
      min_stock_price = 1
    ),
    breakpoint_options_main = breakpoint_options(n_portfolios = 5)
  )
)
#> Filter 'min_stock_price': removed 8 observations.
#> # A tibble: 500 × 5
#>    portfolio date       ret_excess_vw ret_excess_ew ret_excess_vw_capped
#>        <int> <date>             <dbl>         <dbl>                <dbl>
#>  1         1 2020-01-01        0.335         0.259                0.333 
#>  2         1 2020-02-01        0.799         0.818                0.814 
#>  3         1 2020-03-01       -0.476        -0.273               -0.334 
#>  4         1 2020-04-01       -0.216        -0.0738              -0.216 
#>  5         1 2020-05-01        0.623         0.538                0.623 
#>  6         1 2020-06-01       -0.357        -0.211               -0.357 
#>  7         1 2020-07-01        1.05          1.00                 1.05  
#>  8         1 2020-08-01        0.190         0.122                0.185 
#>  9         1 2020-09-01        1.38          1.27                 1.38  
#> 10         1 2020-10-01       -0.0496       -0.0619              -0.0498
#> # ℹ 490 more rows

```
