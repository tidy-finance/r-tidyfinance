# Filter Sorting Data

Applies sample construction filters to a data frame before portfolio
sorting. Filters are applied in a fixed order: financials exclusion,
utilities exclusion, minimum stock price, minimum size quantile, minimum
listing age, positive book equity, and positive earnings. An
informational message is emitted for each filter that actually removes
at least one observation.

## Usage

``` r
filter_sorting_data(
  data,
  filter_options = NULL,
  data_options = NULL,
  quiet = FALSE
)
```

## Arguments

- data:

  A data frame containing the stock-level panel data to be filtered.

- filter_options:

  A list of class `tidyfinance_filter_options` created by
  [`filter_options()`](https://package.tidy-finance.org/reference/filter_options.md).
  If `NULL` (the default), the defaults from
  [`filter_options()`](https://package.tidy-finance.org/reference/filter_options.md)
  are used (i.e., no filters are applied). The arguments accepted by
  [`filter_options()`](https://package.tidy-finance.org/reference/filter_options.md)
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
    default) applies no size quantile filter. The cutoff is computed
    from NYSE stocks only; the `exchange` column (mapped via
    [`data_options()`](https://package.tidy-finance.org/reference/data_options.md))
    must be present in the data or an error is raised.

  - `min_listing_age` A single non-negative integer or numeric
    specifying the minimum number of months a stock must have been
    listed in CRSP. `NULL` (the default) applies no listing age filter.

  - `exclude_negative_book_equity` A logical indicating whether to
    exclude observations with non-positive book equity. Defaults to
    `FALSE`.

  - `exclude_negative_earnings` A logical indicating whether to exclude
    observations with non-positive earnings. Defaults to `FALSE`.

- data_options:

  A list of class `tidyfinance_data_options` (created via
  [`data_options()`](https://package.tidy-finance.org/reference/data_options.md))
  specifying column name mappings. The `siccd` element is used to
  specify the SIC code column, `price` is used to specify the (adjusted)
  price column, `mktcap_lag` is used to specify the market
  capitalization column, `date` is used to specify the date column,
  `listing_age` is used to specify the listing age column, `be` is used
  to specify the book equity column, and `earnings` is used to specify
  the earnings column. Uses
  [`data_options()`](https://package.tidy-finance.org/reference/data_options.md)
  default if `NULL`: `"siccd" = "siccd"`, `"price" = "prc_adj"`,
  `"exchange" = "exchange"` `"mktcap_lag" = "mktcap_lag"`,
  `"date" = "date"`, `"listing_age" = "listing_age"`, `"be" = "be"`, and
  `"earnings" = "ib"`.

- quiet:

  A logical indicating whether informational messages should be
  suppressed. Defaults to `FALSE`.

## Value

The filtered data frame, preserving the class and structure of the
input.

## See also

Other portfolio functions:
[`assign_portfolio()`](https://package.tidy-finance.org/reference/assign_portfolio.md),
[`breakpoint_options()`](https://package.tidy-finance.org/reference/breakpoint_options.md),
[`compute_breakpoints()`](https://package.tidy-finance.org/reference/compute_breakpoints.md),
[`compute_long_short_returns()`](https://package.tidy-finance.org/reference/compute_long_short_returns.md),
[`compute_portfolio_returns()`](https://package.tidy-finance.org/reference/compute_portfolio_returns.md),
[`data_options()`](https://package.tidy-finance.org/reference/data_options.md),
[`filter_options()`](https://package.tidy-finance.org/reference/filter_options.md),
[`implement_portfolio_sort()`](https://package.tidy-finance.org/reference/implement_portfolio_sort.md),
[`portfolio_sort_options()`](https://package.tidy-finance.org/reference/portfolio_sort_options.md)

## Examples

``` r
data <- data.frame(
  permno = 1:5,
  date = as.Date("2020-01-01"),
  siccd = c(6100, 2000, 4950, 3000, 6500),
  prc_adj = c(5, 0.5, 15, 20, 10)
)

data |>
  filter_sorting_data(
    filter_options = filter_options(
      exclude_financials = TRUE,
      min_stock_price = 1
    )
  )
#> Filter 'exclude_financials': removed 2 observations.
#> Filter 'min_stock_price': removed 1 observation.
#>   permno       date siccd prc_adj
#> 1      3 2020-01-01  4950      15
#> 2      4 2020-01-01  3000      20
```
