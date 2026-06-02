# Create Data Options

Creates a list of data options of class `tidyfinance_data_options` used
for TidyFinance-related functions. These options map the specific data
variables to the TidyFinance naming conventions, allowing functions to
flexibly work with different datasets by specifying the relevant column
names. Additional options can be passed through `...`.

## Usage

``` r
data_options(
  id = "permno",
  date = "date",
  exchange = "exchange",
  mktcap_lag = "mktcap_lag",
  ret_excess = "ret_excess",
  portfolio = "portfolio",
  siccd = "siccd",
  price = "prc_adj",
  listing_age = "listing_age",
  be = "be",
  earnings = "ib",
  ...
)
```

## Arguments

- id:

  A character string representing the entity variable (defaults to
  `"permno"`).

- date:

  A character string representing the date variable (defaults to
  `"date"`).

- exchange:

  A character string representing the exchange variable (defaults to
  `"exchange"`).

- mktcap_lag:

  A character string representing the market capitalization lag variable
  (defaults to `"mktcap_lag"`).

- ret_excess:

  A character string representing the excess return variable (defaults
  to `"ret_excess"`).

- portfolio:

  A character string representing the portfolio variable (defaults to
  `"portfolio"`).

- siccd:

  A character string representing the Standard Industrial Classification
  code variable (defaults to `"siccd"`).

- price:

  A character string representing the (adjusted) price variable
  (defaults to `"prc_adj"`).

- listing_age:

  A character string representing the listing age variable (defaults to
  `"listing_age"`).

- be:

  A character string representing the book equity variable (defaults to
  `"be"`).

- earnings:

  A character string representing the earnings variable (defaults to
  `"ib"`, the Compustat income before extraordinary items column).

- ...:

  Additional arguments to be included in the data options list.

## Value

A list of class `tidyfinance_data_options` containing the specified data
options.

## See also

Other portfolio functions:
[`assign_portfolio()`](https://package.tidy-finance.org/reference/assign_portfolio.md),
[`breakpoint_options()`](https://package.tidy-finance.org/reference/breakpoint_options.md),
[`compute_breakpoints()`](https://package.tidy-finance.org/reference/compute_breakpoints.md),
[`compute_long_short_returns()`](https://package.tidy-finance.org/reference/compute_long_short_returns.md),
[`compute_portfolio_returns()`](https://package.tidy-finance.org/reference/compute_portfolio_returns.md),
[`filter_options()`](https://package.tidy-finance.org/reference/filter_options.md),
[`filter_sorting_data()`](https://package.tidy-finance.org/reference/filter_sorting_data.md),
[`implement_portfolio_sort()`](https://package.tidy-finance.org/reference/implement_portfolio_sort.md),
[`portfolio_sort_options()`](https://package.tidy-finance.org/reference/portfolio_sort_options.md)

## Examples

``` r
data_options(
  id = "permno",
  date = "date",
  exchange = "exchange"
)
#> $id
#> [1] "permno"
#> 
#> $date
#> [1] "date"
#> 
#> $exchange
#> [1] "exchange"
#> 
#> $mktcap_lag
#> [1] "mktcap_lag"
#> 
#> $ret_excess
#> [1] "ret_excess"
#> 
#> $portfolio
#> [1] "portfolio"
#> 
#> $siccd
#> [1] "siccd"
#> 
#> $price
#> [1] "prc_adj"
#> 
#> $listing_age
#> [1] "listing_age"
#> 
#> $be
#> [1] "be"
#> 
#> $earnings
#> [1] "ib"
#> 
#> attr(,"class")
#> [1] "tidyfinance_data_options"
```
