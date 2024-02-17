
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tidyfinance

<!-- badges: start -->
<!-- badges: end -->

This repository contains an R package that collects helper functions for
developers and researchers familiar with [Tidy Financen with
R](https://www.tidy-finance.org/r/). The functions provide shortcuts to
selected issues that the book disccuses in detail.

## Installation

You can install the released version of `tidyfinance` from CRAN via:

``` r
install.packages("tidyfinance")
```

You can install the development version of `tidyfinance` from GitHub
via:

``` r
# install.packages("pak")
pak::pak("tidy-finance/r-tidyfinance")
```

## Usage

### Download Data

The main functionality of the `tidyfinance` package centers around data
download. You can download most of the data that we used in Tidy Finance
with R using the `download_data()` function or its children. For
instance, both functions give the same result:

``` r
download_data("factors_ff3_monthly", "2000-01-01", "2020-12-31")
download_data_factors_ff("factors_ff3_monthly", "2000-01-01", "2020-12-31")
```

You can also download data directly from WRDS (if you have set your
credentials via
`Sys.setenv(WRDS_USER = "your_username", WRDS_PASSWORD = "your_password")`),
e.g.,

``` r
download_data("wrds_compustat_annual", "2020-01-01", "2020-12-31")
```

If you want to fetch additional columns that are not included in our
default selection, then pass them as additional arguments in the
corresponding child function:

``` r
download_data_wrds_compustat("wrds_compustat_annual", "2020-01-01", "2020-12-31", acoxar, amc, aldo)
```

You can get a list of all currently supported types via
`list_supported_types()`. Please open an issue on GitHub to request
additional supported types.

### Other Helpers

We included functions to check out content from tidy-finance.org:

``` r
list_tidy_finance_chapters()
open_tidy_finance_website()
```

There are alos some simple helpers for regression analyses:

``` r
winsorize()
trim()
create_summary_statistics()
```

We have also included (experimental) functions that can be used for
different applications, but note that they might heavily change in
future package versions as we try to make them more general:

``` r
# For portfolio sorts
assign_portfolio()

# For beta estimation
estimate_model()
```
