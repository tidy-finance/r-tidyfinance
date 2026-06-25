# Create Portfolio Sort Options

Creates a list of options of class `tidyfinance_portfolio_sort_options`
that bundles sample construction filters and breakpoint specifications
for use with
[`implement_portfolio_sort()`](https://r.tidy-finance.org/reference/implement_portfolio_sort.md).

## Usage

``` r
portfolio_sort_options(
  filter_options = NULL,
  breakpoint_options_main,
  breakpoint_options_secondary = NULL,
  ...
)
```

## Arguments

- filter_options:

  A list of class `tidyfinance_filter_options` created by
  [`filter_options()`](https://r.tidy-finance.org/reference/filter_options.md),
  or `NULL` (the default, which applies no filters). The arguments
  accepted by
  [`filter_options()`](https://r.tidy-finance.org/reference/filter_options.md)
  include

  - `exclude_financials` A logical indicating whether to exclude
    financial firms (SIC codes 6000–6799). Defaults to `FALSE`.

  - `exclude_utilities` A logical indicating whether to exclude utility
    firms (SIC codes 4900–4999). Defaults to `FALSE`.

  - `min_stock_price` A single positive numeric specifying the minimum
    stock price required to include an observation. `NULL` (the default)
    applies no price filter.

  - `min_size_quantile` A single numeric strictly between 0 and 1
    specifying the minimum cross-sectional size quantile (based on
    lagged market cap) required to include an observation. `NULL` (the
    default) applies no size quantile filter.

  - `min_listing_age` A single non-negative integer or numeric
    specifying the minimum number of months a stock must have been
    listed in CRSP. `NULL` (the default) applies no listing age filter.

  - `exclude_negative_book_equity` A logical indicating whether to
    exclude observations with non-positive book equity. Defaults to
    `FALSE`.

  - `exclude_negative_earnings` A logical indicating whether to exclude
    observations with non-positive earnings. Defaults to `FALSE`.

- breakpoint_options_main:

  A list of class `tidyfinance_breakpoint_options` created by
  [`breakpoint_options()`](https://r.tidy-finance.org/reference/breakpoint_options.md),
  specifying breakpoints for the primary sorting variable. The arguments
  accepted by
  [`breakpoint_options()`](https://r.tidy-finance.org/reference/breakpoint_options.md)
  include

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
    bunches on the extremes (`TRUE`), or not (`FALSE`, the default).

  - `breakpoints_min_size_threshold` An optional numeric value between 0
    and 1 (exclusive). When set, stocks with market capitalization below
    this quantile are excluded from breakpoint computation.

- breakpoint_options_secondary:

  A list of class `tidyfinance_breakpoint_options` created by
  [`breakpoint_options()`](https://r.tidy-finance.org/reference/breakpoint_options.md),
  specifying breakpoints for the secondary sorting variable, or `NULL`
  (the default) for univariate sorts. The arguments accepted by
  [`breakpoint_options()`](https://r.tidy-finance.org/reference/breakpoint_options.md)
  are the same as for `breakpoint_options_main`.

- ...:

  Additional arguments to be included in the options list.

## Value

A list of class `tidyfinance_portfolio_sort_options` containing the
specified options.

## See also

Other portfolio functions:
[`assign_portfolio()`](https://r.tidy-finance.org/reference/assign_portfolio.md),
[`breakpoint_options()`](https://r.tidy-finance.org/reference/breakpoint_options.md),
[`compute_breakpoints()`](https://r.tidy-finance.org/reference/compute_breakpoints.md),
[`compute_long_short_returns()`](https://r.tidy-finance.org/reference/compute_long_short_returns.md),
[`compute_portfolio_returns()`](https://r.tidy-finance.org/reference/compute_portfolio_returns.md),
[`data_options()`](https://r.tidy-finance.org/reference/data_options.md),
[`filter_options()`](https://r.tidy-finance.org/reference/filter_options.md),
[`filter_sorting_data()`](https://r.tidy-finance.org/reference/filter_sorting_data.md),
[`implement_portfolio_sort()`](https://r.tidy-finance.org/reference/implement_portfolio_sort.md)

## Examples

``` r
portfolio_sort_options(
  filter_options = filter_options(exclude_financials = TRUE),
  breakpoint_options_main = breakpoint_options(n_portfolios = 10)
)
#> $filter_options
#> $exclude_financials
#> [1] TRUE
#> 
#> $exclude_utilities
#> [1] FALSE
#> 
#> $min_stock_price
#> NULL
#> 
#> $min_size_quantile
#> NULL
#> 
#> $min_listing_age
#> NULL
#> 
#> $exclude_negative_book_equity
#> [1] FALSE
#> 
#> $exclude_negative_earnings
#> [1] FALSE
#> 
#> attr(,"class")
#> [1] "tidyfinance_filter_options"
#> 
#> $breakpoint_options_main
#> $n_portfolios
#> [1] 10
#> 
#> $percentiles
#> NULL
#> 
#> $breakpoints_exchanges
#> NULL
#> 
#> $smooth_bunching
#> [1] FALSE
#> 
#> $breakpoints_min_size_threshold
#> NULL
#> 
#> attr(,"class")
#> [1] "tidyfinance_breakpoint_options"
#> 
#> $breakpoint_options_secondary
#> NULL
#> 
#> attr(,"class")
#> [1] "tidyfinance_portfolio_sort_options"
```
