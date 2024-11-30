
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tidyfinance

<!-- badges: start -->

[![](https://www.r-pkg.org/badges/version/tidyfinance)](https://cran.r-project.org/package=tidyfinance)
[![](http://cranlogs.r-pkg.org/badges/grand-total/tidyfinance)](https://cran.r-project.org/package=tidyfinance)
[![Project Status: Active - The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![](https://img.shields.io/badge/devel%20version-0.4.1.9004-blue.svg)](https://github.com/tidy-finance/r-tidyfinance)
[![R-CMD-check](https://github.com/tidy-finance/r-tidyfinance/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/tidy-finance/r-tidyfinance/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

This repository contains an R package that collects helper functions for
developers and researchers familiar with [Tidy Finance with
R](https://www.tidy-finance.org/r/index.html). The functions provide
shortcuts to selected issues that the book discusses in detail.

## Installation

You can install the released version of `tidyfinance` [from
CRAN](https://cran.r-project.org/package=tidyfinance) via:

``` r
install.packages("tidyfinance")
```

You can install the development version of `tidyfinance` from
[GitHub](https://github.com/tidy-finance/r-tidyfinance) via:

``` r
# install.packages("pak")
pak::pak("tidy-finance/r-tidyfinance")
```

## Usage

### Download Data

The main functionality of the `tidyfinance` package centers around data
download. You can download most of the data that we used in [Tidy
Finance with R](https://www.tidy-finance.org/r/index.html) using the
`download_data()` function or its children. For instance, both functions
give the same result:

``` r
download_data(
  type = "factors_ff_3_monthly", 
  start_date = "2000-01-01", 
  end_date = "2020-12-31"
)

download_data_factors_ff(
  type = "factors_ff_3_monthly", 
  start_date = "2000-01-01", 
  end_date = "2020-12-31"
)
```

You can also download data directly from
[WRDS](https://www.tidy-finance.org/r/wrds-crsp-and-compustat.html) (if
you have set your credentials via
`Sys.setenv(WRDS_USER = "your_username", WRDS_PASSWORD = "your_password")`),
e.g.,

``` r
download_data(
  type = "wrds_compustat_annual", 
  start_date = "2000-01-01", 
  end_date = "2020-12-31"
)
```

If you want to fetch additional columns that are not included in our
default selection, then pass them as additional arguments in the
corresponding child function:

``` r
download_data_wrds_compustat(
  type = "wrds_compustat_annual",  
  start_date = "2000-01-01", 
  end_date = "2020-12-31",
  additional_columns = c("acoxar", "amc", "aldo")
)
```

You can get a list of all currently supported types via
`list_supported_types()`. Please open an [issue on
GitHub](https://github.com/tidy-finance/r-tidyfinance/issues) to request
additional supported types.

### Other Helpers

We include functions to check out content from
[tidy-finance.org](https://www.tidy-finance.org/r/index.html):

``` r
list_tidy_finance_chapters()
open_tidy_finance_website()
```

There are also some simple helpers for regression analyses:

``` r
winsorize()
trim()
create_summary_statistics()
```

We also include (experimental) functions that can be used for different
applications, but note that they might heavily change in future package
versions as we try to make them more general:

``` r
# For portfolio sorts
?assign_portfolio()

# For model estimation
?estimate_model()

# For beta estimation
?estimate_betas()

# For beta estimation
?estimate_fama_macbeth()
```
