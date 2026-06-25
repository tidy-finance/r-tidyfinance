# Compute Long-Short Returns

Calculates long-short returns based on the returns of portfolios. The
long-short return is computed as the difference between the returns of
the "top" and "bottom" portfolios. The direction of the calculation can
be adjusted based on whether the return from the "bottom" portfolio is
subtracted from or added to the return from the "top" portfolio.

## Usage

``` r
compute_long_short_returns(
  data,
  direction = "top_minus_bottom",
  data_options = NULL
)
```

## Arguments

- data:

  A data frame containing portfolio returns. The data frame must include
  columns for the portfolio identifier, date, and return measurements
  (as specified in `data_options`).

- direction:

  A character string specifying the direction of the long-short return
  calculation. It can be either `"top_minus_bottom"` or
  `"bottom_minus_top"`. Default is `"top_minus_bottom"`. If set to
  `"bottom_minus_top"`, the return will be computed as (bottom - top).

- data_options:

  A list of class `tidyfinance_data_options` (created via
  [`data_options()`](https://r.tidy-finance.org/reference/data_options.md))
  specifying column name mappings. The `date` element is used to specify
  the date column, the `ret_excess` element is used to specify the
  excess return column, and `portfolio` is used to specify the assigned
  portfolio. Uses
  [`data_options()`](https://r.tidy-finance.org/reference/data_options.md)
  default if `NULL`: `"date" = "date"`, `"ret_excess" = "ret_excess"`,
  and `"portfolio" = "portfolio"`.

## Value

A data frame with columns for date, return measurement types (from the
"ret_measure" column), and the computed long-short returns. The data
frame is arranged by date and pivoted to have return measurement types
as columns with their corresponding long-short returns.

## See also

Other portfolio functions:
[`assign_portfolio()`](https://r.tidy-finance.org/reference/assign_portfolio.md),
[`breakpoint_options()`](https://r.tidy-finance.org/reference/breakpoint_options.md),
[`compute_breakpoints()`](https://r.tidy-finance.org/reference/compute_breakpoints.md),
[`compute_portfolio_returns()`](https://r.tidy-finance.org/reference/compute_portfolio_returns.md),
[`data_options()`](https://r.tidy-finance.org/reference/data_options.md),
[`filter_options()`](https://r.tidy-finance.org/reference/filter_options.md),
[`filter_sorting_data()`](https://r.tidy-finance.org/reference/filter_sorting_data.md),
[`implement_portfolio_sort()`](https://r.tidy-finance.org/reference/implement_portfolio_sort.md),
[`portfolio_sort_options()`](https://r.tidy-finance.org/reference/portfolio_sort_options.md)

## Examples

``` r
set.seed(42)
data <- data.frame(
  permno = 1:100,
  date = rep(
    seq.Date(from = as.Date("2020-01-01"), by = "month", length.out = 100),
    each = 10
  ),
  mktcap_lag = runif(100, 100, 1000),
  ret_excess = rnorm(100),
  size = runif(100, 50, 150)
)

portfolio_returns <- compute_portfolio_returns(
  data, "size", "univariate",
  breakpoint_options_main = breakpoint_options(n_portfolios = 5)
)

compute_long_short_returns(portfolio_returns)
#> # A tibble: 100 × 4
#>    date       ret_excess_vw ret_excess_ew ret_excess_vw_capped
#>    <date>             <dbl>         <dbl>                <dbl>
#>  1 2020-01-01     -0.833          -0.650               -0.833 
#>  2 2020-02-01      0.0996         -0.0632               0.0996
#>  3 2020-03-01      0.658           0.687                0.673 
#>  4 2020-04-01      0.998           1.05                 1.01  
#>  5 2020-05-01      0.567           0.747                0.567 
#>  6 2020-06-01     -1.13           -0.817               -1.05  
#>  7 2020-07-01      1.66            1.27                 1.66  
#>  8 2020-08-01      0.922           1.23                 0.948 
#>  9 2020-09-01     -0.488           0.137               -0.488 
#> 10 2020-10-01     -0.000934        0.0858               0.0389
#> # ℹ 90 more rows
```
