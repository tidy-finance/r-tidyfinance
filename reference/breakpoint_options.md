# Create Breakpoint Options for Portfolio Sorting

Generates a structured list of options for defining breakpoints in
portfolio sorting. It includes parameters for the number of portfolios,
percentile thresholds, exchange-specific breakpoints, and smooth
bunching, along with additional optional parameters.

## Usage

``` r
breakpoint_options(
  n_portfolios = NULL,
  percentiles = NULL,
  breakpoints_exchanges = NULL,
  smooth_bunching = FALSE,
  breakpoints_min_size_threshold = NULL,
  ...
)
```

## Arguments

- n_portfolios:

  Integer, optional. The number of portfolios to create. Must be a
  positive integer. If not provided, defaults to `NULL`.

- percentiles:

  Numeric vector, optional. A vector of percentile thresholds for
  defining breakpoints. Each value must be between 0 and 1. If not
  provided, defaults to `NULL`.

- breakpoints_exchanges:

  Character vector, optional. A non-empty vector specifying the exchange
  from which to compute the breakpoints. If not provided, defaults to
  `NULL`.

- smooth_bunching:

  Logical, optional. Indicates whether smooth bunching should be
  applied. Defaults to `FALSE`.

- breakpoints_min_size_threshold:

  Numeric, optional. When set to a value between 0 and 1, stocks with
  market capitalization below this quantile are excluded from breakpoint
  computation. The quantile is computed among `breakpoints_exchanges`
  stocks if specified, otherwise among all stocks. Requires a market
  capitalization column in the data (see
  [`data_options()`](https://r.tidy-finance.org/reference/data_options.md)).
  Defaults to `NULL` (no size filtering).

- ...:

  Additional optional arguments. These will be captured in the resulting
  structure as a list.

## Value

A list of class `"tidyfinance_breakpoint_options"` containing the
provided breakpoint options, including any additional arguments passed
via `...`.

## See also

Other portfolio functions:
[`assign_portfolio()`](https://r.tidy-finance.org/reference/assign_portfolio.md),
[`compute_breakpoints()`](https://r.tidy-finance.org/reference/compute_breakpoints.md),
[`compute_long_short_returns()`](https://r.tidy-finance.org/reference/compute_long_short_returns.md),
[`compute_portfolio_returns()`](https://r.tidy-finance.org/reference/compute_portfolio_returns.md),
[`data_options()`](https://r.tidy-finance.org/reference/data_options.md),
[`filter_options()`](https://r.tidy-finance.org/reference/filter_options.md),
[`filter_sorting_data()`](https://r.tidy-finance.org/reference/filter_sorting_data.md),
[`implement_portfolio_sort()`](https://r.tidy-finance.org/reference/implement_portfolio_sort.md),
[`portfolio_sort_options()`](https://r.tidy-finance.org/reference/portfolio_sort_options.md)

## Examples

``` r
breakpoint_options(
  n_portfolios = 5,
  percentiles = c(0.2, 0.4, 0.6, 0.8),
  breakpoints_exchanges = "NYSE",
  smooth_bunching = TRUE,
  custom_threshold = 0.5,
  another_option = "example"
)
#> $n_portfolios
#> [1] 5
#> 
#> $percentiles
#> [1] 0.2 0.4 0.6 0.8
#> 
#> $breakpoints_exchanges
#> [1] "NYSE"
#> 
#> $smooth_bunching
#> [1] TRUE
#> 
#> $breakpoints_min_size_threshold
#> NULL
#> 
#> $custom_threshold
#> [1] 0.5
#> 
#> $another_option
#> [1] "example"
#> 
#> attr(,"class")
#> [1] "tidyfinance_breakpoint_options"
```
