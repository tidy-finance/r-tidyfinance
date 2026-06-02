# Assign Portfolios Based on Sorting Variable

Assigns data points to portfolios based on a specified sorting variable
and the selected function to compute breakpoints. Users can specify a
function to compute breakpoints. The function must take `data` and
`sorting_variable` as the first two arguments. Additional arguments are
passed with a named list
[`breakpoint_options()`](https://package.tidy-finance.org/reference/breakpoint_options.md).
The function needs to return an ascending vector of breakpoints. By
default, breakpoints are computed with
[`compute_breakpoints()`](https://package.tidy-finance.org/reference/compute_breakpoints.md).
The default column names can be modified using
[`data_options()`](https://package.tidy-finance.org/reference/data_options.md).

## Usage

``` r
assign_portfolio(
  data,
  sorting_variable,
  breakpoint_options = NULL,
  breakpoint_function = compute_breakpoints,
  data_options = NULL
)
```

## Arguments

- data:

  A data frame containing the dataset for portfolio assignment.

- sorting_variable:

  A string specifying the column name in `data` to be used for sorting
  and determining portfolio assignments based on the breakpoints.

- breakpoint_options:

  An optional named list of arguments passed to `breakpoint_function`.

- breakpoint_function:

  A function to compute breakpoints. The default is set to
  [`compute_breakpoints()`](https://package.tidy-finance.org/reference/compute_breakpoints.md).

- data_options:

  A list of class `tidyfinance_data_options` (created via
  [`data_options()`](https://package.tidy-finance.org/reference/data_options.md))
  specifying column name mappings. Passed through to
  `breakpoint_function`. When using the default
  [`compute_breakpoints()`](https://package.tidy-finance.org/reference/compute_breakpoints.md),
  the `exchange` element is used to specify the exchange column, and
  `mktcap_lag` is used to specify the market capitalization column. Uses
  [`data_options()`](https://package.tidy-finance.org/reference/data_options.md)
  default if `NULL`: `"exchange" = "exchange"` and
  `"mktcap_lag" = "mktcap_lag"`.

## Value

A vector of integer portfolio assignments for each row in the input
`data`.

## See also

Other portfolio functions:
[`breakpoint_options()`](https://package.tidy-finance.org/reference/breakpoint_options.md),
[`compute_breakpoints()`](https://package.tidy-finance.org/reference/compute_breakpoints.md),
[`compute_long_short_returns()`](https://package.tidy-finance.org/reference/compute_long_short_returns.md),
[`compute_portfolio_returns()`](https://package.tidy-finance.org/reference/compute_portfolio_returns.md),
[`data_options()`](https://package.tidy-finance.org/reference/data_options.md),
[`filter_options()`](https://package.tidy-finance.org/reference/filter_options.md),
[`filter_sorting_data()`](https://package.tidy-finance.org/reference/filter_sorting_data.md),
[`implement_portfolio_sort()`](https://package.tidy-finance.org/reference/implement_portfolio_sort.md),
[`portfolio_sort_options()`](https://package.tidy-finance.org/reference/portfolio_sort_options.md)

## Examples

``` r
set.seed(42)
data <- data.frame(
  id = 1:100,
  exchange = sample(c("NYSE", "NASDAQ"), 100, replace = TRUE),
  market_cap = 1:100
)

assign_portfolio(data, "market_cap", breakpoint_options(n_portfolios = 5))
#>   [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
#>  [38] 2 2 2 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 4 4 4 4 4 4 4 4 4 4 4 4 4 4
#>  [75] 4 4 4 4 4 4 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5

assign_portfolio(
  data,
  "market_cap",
  breakpoint_options(
    percentiles = c(0.2, 0.4, 0.6, 0.8),
    breakpoints_exchanges = c("NYSE")
  )
)
#>   [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 2 2 3 3 3 3 3 3 3
#>  [38] 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4
#>  [75] 4 4 4 4 4 4 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5
```
