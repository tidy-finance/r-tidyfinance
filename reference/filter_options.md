# Create Filter Options

Creates a list of filter options of class `tidyfinance_filter_options`
used for sample construction in TidyFinance-related functions. These
options control which observations are retained before portfolio
sorting.

## Usage

``` r
filter_options(
  exclude_financials = FALSE,
  exclude_utilities = FALSE,
  min_stock_price = NULL,
  min_size_quantile = NULL,
  min_listing_age = NULL,
  exclude_negative_book_equity = FALSE,
  exclude_negative_earnings = FALSE,
  ...
)
```

## Arguments

- exclude_financials:

  A logical indicating whether to exclude financial firms (SIC codes
  6000–6799). Defaults to `FALSE`.

- exclude_utilities:

  A logical indicating whether to exclude utility firms (SIC codes
  4900–4999). Defaults to `FALSE`.

- min_stock_price:

  A single positive numeric specifying the minimum stock price required
  to include an observation. `NULL` (the default) applies no price
  filter.

- min_size_quantile:

  A single numeric strictly between 0 and 1 specifying the minimum
  cross-sectional size quantile (based on lagged market cap) required to
  include an observation. `NULL` (the default) applies no size quantile
  filter. The cutoff is computed from NYSE stocks only. This requires an
  `exchange` column in the data (as mapped via
  [`data_options()`](https://r.tidy-finance.org/reference/data_options.md));
  an error is raised if it is missing.

- min_listing_age:

  A single non-negative integer or numeric specifying the minimum number
  of months a stock must have been listed in CRSP. `NULL` (the default)
  applies no listing age filter.

- exclude_negative_book_equity:

  A logical indicating whether to exclude observations with non-positive
  book equity. Defaults to `FALSE`.

- exclude_negative_earnings:

  A logical indicating whether to exclude observations with non-positive
  earnings. Defaults to `FALSE`.

- ...:

  Additional arguments to be included in the filter options list.

## Value

A list of class `tidyfinance_filter_options` containing the specified
filter options.

## See also

Other portfolio functions:
[`assign_portfolio()`](https://r.tidy-finance.org/reference/assign_portfolio.md),
[`breakpoint_options()`](https://r.tidy-finance.org/reference/breakpoint_options.md),
[`compute_breakpoints()`](https://r.tidy-finance.org/reference/compute_breakpoints.md),
[`compute_long_short_returns()`](https://r.tidy-finance.org/reference/compute_long_short_returns.md),
[`compute_portfolio_returns()`](https://r.tidy-finance.org/reference/compute_portfolio_returns.md),
[`data_options()`](https://r.tidy-finance.org/reference/data_options.md),
[`filter_sorting_data()`](https://r.tidy-finance.org/reference/filter_sorting_data.md),
[`implement_portfolio_sort()`](https://r.tidy-finance.org/reference/implement_portfolio_sort.md),
[`portfolio_sort_options()`](https://r.tidy-finance.org/reference/portfolio_sort_options.md)

## Examples

``` r
filter_options(
  exclude_financials = TRUE,
  exclude_utilities = TRUE,
  min_stock_price = 1,
  min_listing_age = 12
)
#> $exclude_financials
#> [1] TRUE
#> 
#> $exclude_utilities
#> [1] TRUE
#> 
#> $min_stock_price
#> [1] 1
#> 
#> $min_size_quantile
#> NULL
#> 
#> $min_listing_age
#> [1] 12
#> 
#> $exclude_negative_book_equity
#> [1] FALSE
#> 
#> $exclude_negative_earnings
#> [1] FALSE
#> 
#> attr(,"class")
#> [1] "tidyfinance_filter_options"
```
