# Compute Breakpoints Based on Sorting Variable

Computes breakpoints based on a specified sorting. It can optionally
filter the data by exchanges or lagged size quantiles before computing
the breakpoints. The function requires either the number of portfolios
to be created or specific percentiles for the breakpoints, but not both.
The function also optionally handles cases where the sorting variable
clusters on the edges, by assigning all extreme values to the edges and
attempting to compute equally populated breakpoints with the remaining
values.

## Usage

``` r
compute_breakpoints(
  data,
  sorting_variable,
  breakpoint_options,
  data_options = NULL
)
```

## Arguments

- data:

  A data frame containing the dataset for breakpoint computation.

- sorting_variable:

  A character string specifying the column name in `data` to be used for
  determining breakpoints.

- breakpoint_options:

  A named list of
  [`breakpoint_options()`](https://r.tidy-finance.org/reference/breakpoint_options.md)
  for the breakpoints. The arguments include

  - `n_portfolios` An optional integer specifying the number of equally
    sized portfolios to create. This parameter is mutually exclusive
    with `percentiles`.

  - `percentiles` An optional numeric vector specifying the percentiles
    for determining the breakpoints of the portfolios. This parameter is
    mutually exclusive with `n_portfolios`.

  - `breakpoints_exchanges` An optional character vector specifying
    exchange names to filter the data before computing breakpoints.
    Exchanges must be stored in a column given by `data_options`
    (defaults to `exchange`). If `NULL`, no filtering is applied.

  - `smooth_bunching` An optional logical parameter specifying if to
    attempt smoothing non-extreme portfolios if the sorting variable
    bunches on the extremes (`TRUE`), or not (`FALSE`, the default). In
    some cases, smoothing will not result in equal-sized portfolios off
    the edges due to multiple clusters. If sufficiently large bunching
    is detected, `percentiles` is ignored and equally-spaced portfolios
    are returned for these cases with a warning.

  - `breakpoints_min_size_threshold` An optional numeric value between 0
    and 1 (exclusive). When set, stocks with market capitalization below
    this quantile are excluded from breakpoint computation. The quantile
    is computed among `breakpoints_exchanges` stocks if specified,
    otherwise among all stocks. Requires a market capitalization column
    in the data (column name determined by `data_options`).

- data_options:

  A list of class `tidyfinance_data_options` (created via
  [`data_options()`](https://r.tidy-finance.org/reference/data_options.md))
  specifying column name mappings. The `exchange` element is used to
  specify the exchange column, and `mktcap_lag` is used to specify the
  market capitalization. Uses
  [`data_options()`](https://r.tidy-finance.org/reference/data_options.md)
  default if `NULL`: `"exchange" = "exchange"` and
  `"mktcap_lag" = "mktcap_lag"`.

## Value

A numeric vector of breakpoints of the desired length.

## Note

This function will stop and throw an error if both `n_portfolios` and
`percentiles` are provided or missing simultaneously.

## See also

Other portfolio functions:
[`assign_portfolio()`](https://r.tidy-finance.org/reference/assign_portfolio.md),
[`breakpoint_options()`](https://r.tidy-finance.org/reference/breakpoint_options.md),
[`compute_long_short_returns()`](https://r.tidy-finance.org/reference/compute_long_short_returns.md),
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
  id = 1:100,
  exchange = sample(c("NYSE", "NASDAQ"), 100, replace = TRUE),
  market_cap = 1:100
)

compute_breakpoints(data, "market_cap", breakpoint_options(n_portfolios = 5))
#> [1]   1.0  20.8  40.6  60.4  80.2 100.0
compute_breakpoints(
  data,
  "market_cap",
  breakpoint_options(
    percentiles = c(0.2, 0.4, 0.6, 0.8),
    breakpoints_exchanges = c("NYSE")
  )
 )
#> [1]  1.0 19.0 31.0 59.8 80.8 97.0
```
