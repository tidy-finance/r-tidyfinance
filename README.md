
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tidyfinance

<!-- badges: start -->

[![](https://www.r-pkg.org/badges/version/tidyfinance)](https://cran.r-project.org/package=tidyfinance)
[![](http://cranlogs.r-pkg.org/badges/grand-total/tidyfinance)](https://cran.r-project.org/package=tidyfinance)
[![Project Status: Active - The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![](https://img.shields.io/badge/devel%20version-0.4.5.9002-blue.svg)](https://github.com/tidy-finance/r-tidyfinance)
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

    # install.packages("pak")
    pak::pak("tidy-finance/r-tidyfinance")

## Usage

Load the package:

``` r
library(tidyfinance)
#> Registered S3 method overwritten by 'future':
#>   method               from      
#>   all.equal.connection parallelly
```

## Download Open Source Data

The main functionality of the `tidyfinance` package centers around data
download. You can download most of the data that we used in [Tidy
Finance with R](https://www.tidy-finance.org/r/index.html) using the
`download_data()` function or its children.

The function always requires a `domain` argument and depending on the
domain typically also a `dataset`. For instance, to download monthly
Fama-French factors, you have to provide the dataset name according to
[Ken French’s Data
Library](https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/data_library.html):

``` r
download_data(
  domain = "factors_ff",
  dataset = "Fama/French 5 Factors (2x3) [Daily]",
  start_date = "2000-01-01",
  end_date = "2020-12-31"
)
#> # A tibble: 5,284 × 7
#>    date       mkt_excess     smb     hml     rmw     cma risk_free
#>    <date>          <dbl>   <dbl>   <dbl>   <dbl>   <dbl>     <dbl>
#>  1 2000-01-03    -0.0071 -0.0009 -0.0131 -0.0148 -0.007     0.0002
#>  2 2000-01-04    -0.0406  0.0034  0.0207  0.0053  0.0136    0.0002
#>  3 2000-01-05    -0.0009  0.0036 -0.0005  0.0045  0.0115    0.0002
#>  4 2000-01-06    -0.0074 -0.0004  0.0124  0.0064  0.0121    0.0002
#>  5 2000-01-07     0.0321 -0.0089 -0.0157 -0.0083 -0.01      0.0002
#>  6 2000-01-10     0.0175  0.0043 -0.0135 -0.022  -0.0024    0.0002
#>  7 2000-01-11    -0.0171  0.0033  0.0091  0.009   0.012     0.0002
#>  8 2000-01-12    -0.0069 -0.0022  0.0074  0.0042  0.0085    0.0002
#>  9 2000-01-13     0.0159  0.0047 -0.0084 -0.0172 -0.0103    0.0002
#> 10 2000-01-14     0.0114  0.0022 -0.0048 -0.0034 -0.006     0.0002
#> # ℹ 5,274 more rows
```

For [q factors](https://global-q.org/index.html), you provide the
relevant file name:

``` r
# download_data(
#   domain = "factors_q",
#   dataset = "q5_factors_monthly",
#   start_date = "2000-01-01",
#   end_date = "2020-12-31"
# )
```

To download the Welch and Goyal (2008) macroeconomic predictors for
monthly, quarterly, or annual frequency:

``` r
download_data(
  domain = "macro_predictors",
  dataset = "monthly",
  start_date = "2000-01-01",
  end_date = "2020-12-31"
)
#> # A tibble: 252 × 15
#>    date        rp_div    dp    dy    ep    de    svar    bm    ntis    tbl
#>    <date>       <dbl> <dbl> <dbl> <dbl> <dbl>   <dbl> <dbl>   <dbl>  <dbl>
#>  1 2000-01-01 -0.0244 -4.42 -4.48 -3.35 -1.08 0.00521 0.155 0.0254  0.0532
#>  2 2000-02-01  0.0866 -4.40 -4.42 -3.31 -1.09 0.00300 0.167 0.0274  0.0555
#>  3 2000-03-01 -0.0355 -4.49 -4.40 -3.38 -1.11 0.00668 0.150 0.0183  0.0569
#>  4 2000-04-01 -0.0269 -4.46 -4.49 -3.34 -1.12 0.00794 0.153 0.0119  0.0566
#>  5 2000-05-01  0.0194 -4.44 -4.46 -3.32 -1.13 0.00519 0.156 0.00918 0.0579
#>  6 2000-06-01 -0.0212 -4.47 -4.44 -3.33 -1.13 0.00236 0.157 0.00712 0.0569
#>  7 2000-07-01  0.0532 -4.46 -4.47 -3.30 -1.15 0.00207 0.156 0.00471 0.0596
#>  8 2000-08-01 -0.0595 -4.52 -4.46 -3.35 -1.17 0.00104 0.146 0.00467 0.0609
#>  9 2000-09-01 -0.0105 -4.48 -4.53 -3.29 -1.19 0.00160 0.154 0.00447 0.06  
#> 10 2000-10-01 -0.0876 -4.47 -4.48 -3.30 -1.17 0.00557 0.149 0.00374 0.0611
#> # ℹ 242 more rows
#> # ℹ 5 more variables: lty <dbl>, ltr <dbl>, tms <dbl>, dfy <dbl>, infl <dbl>
```

To download data from [Open Source Asset
Pricing](https://www.openassetpricing.com/) (OSAP):

``` r
download_data(
  domain = "osap",
  start_date = "2020-01-01",
  end_date = "2020-12-31"
)
#> # A tibble: 12 × 213
#>    date           am     aop abnormal_accruals accruals accruals_bm activism1
#>    <date>      <dbl>   <dbl>             <dbl>    <dbl>       <dbl>     <dbl>
#>  1 2020-01-31  -7.90 -2.55               1.56  -1.40           5.67        NA
#>  2 2020-02-28  -4.44 -1.45              -1.39  -1.11          -3.23        NA
#>  3 2020-03-31 -13.6   0.547             -3.61   1.74          -1.18        NA
#>  4 2020-04-30  -3.33 -1.13               7.78   4.53          -2.38        NA
#>  5 2020-05-29 -11.7   1.85               2.07  -0.361         -2.68        NA
#>  6 2020-06-30   1.65 -0.604              3.30   0.00851        4.00        NA
#>  7 2020-07-31  -3.66 -9.13              -2.09   3.00          -5.33        NA
#>  8 2020-08-31   2.65  1.91              -2.49   0.0904         2.23        NA
#>  9 2020-09-30  -3.66 -1.89              -2.35  -1.53          -4.60        NA
#> 10 2020-10-30   7.11  3.04              -0.682 -0.440          3.32        NA
#> 11 2020-11-30   7.39  2.02               2.61   2.42          19.3         NA
#> 12 2020-12-31   1.48  0.0180            -0.594  2.04          -2.92        NA
#> # ℹ 206 more variables: activism2 <dbl>, ad_exp <dbl>, age_ipo <dbl>,
#> #   analyst_revision <dbl>, analyst_value <dbl>, announcement_return <dbl>,
#> #   asset_growth <dbl>, bm <dbl>, bmdec <dbl>, bpebm <dbl>, beta <dbl>,
#> #   beta_fp <dbl>, beta_liquidity_ps <dbl>, beta_tail_risk <dbl>,
#> #   bid_ask_spread <dbl>, book_leverage <dbl>, brand_invest <dbl>,
#> #   cboper_prof <dbl>, cf <dbl>, cpvol_spread <dbl>, cash <dbl>,
#> #   cash_prod <dbl>, ch_asset_turnover <dbl>, ch_eq <dbl>, …
```

To download multiple series from the Federal Reserve Economic Data
(FRED):

``` r
download_data(
  domain = "fred",
  series = c("GDP", "CPIAUCNS"),
  start_date = "2020-01-01",
  end_date = "2020-12-31"
)
#> Warning: The `type` argument of `download_data()` is deprecated as of tidyfinance 0.5.0.
#> ℹ Column type should be replaced with domain and dataset. Use
#>   `list_supported_types()` to see the mapping.
#> This warning is displayed once every 8 hours.
#> Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
#> generated.
#> # A tibble: 16 × 3
#>    date        value series  
#>    <date>      <dbl> <chr>   
#>  1 2020-01-01 21751. GDP     
#>  2 2020-04-01 19958. GDP     
#>  3 2020-07-01 21704. GDP     
#>  4 2020-10-01 22087. GDP     
#>  5 2020-01-01   258. CPIAUCNS
#>  6 2020-02-01   259. CPIAUCNS
#>  7 2020-03-01   258. CPIAUCNS
#>  8 2020-04-01   256. CPIAUCNS
#>  9 2020-05-01   256. CPIAUCNS
#> 10 2020-06-01   258. CPIAUCNS
#> 11 2020-07-01   259. CPIAUCNS
#> 12 2020-08-01   260. CPIAUCNS
#> 13 2020-09-01   260. CPIAUCNS
#> 14 2020-10-01   260. CPIAUCNS
#> 15 2020-11-01   260. CPIAUCNS
#> 16 2020-12-01   260. CPIAUCNS
```

To download stock prices from Yahoo Finance:

``` r
download_data(
  domain = "stock_prices",
  symbols = c("AAPL", "MSFT"),
  start_date = "2020-01-01",
  end_date = "2020-12-31"
)
#> # A tibble: 504 × 8
#>    symbol date          volume  open   low  high close adjusted_close
#>    <chr>  <date>         <dbl> <dbl> <dbl> <dbl> <dbl>          <dbl>
#>  1 AAPL   2020-01-02 135480400  74.1  73.8  75.2  75.1           72.5
#>  2 AAPL   2020-01-03 146322800  74.3  74.1  75.1  74.4           71.8
#>  3 AAPL   2020-01-06 118387200  73.4  73.2  75.0  74.9           72.3
#>  4 AAPL   2020-01-07 108872000  75.0  74.4  75.2  74.6           72.0
#>  5 AAPL   2020-01-08 132079200  74.3  74.3  76.1  75.8           73.2
#>  6 AAPL   2020-01-09 170108400  76.8  76.6  77.6  77.4           74.7
#>  7 AAPL   2020-01-10 140644800  77.7  77.1  78.2  77.6           74.9
#>  8 AAPL   2020-01-13 121532000  77.9  77.8  79.3  79.2           76.5
#>  9 AAPL   2020-01-14 161954400  79.2  78.0  79.4  78.2           75.4
#> 10 AAPL   2020-01-15 121923600  78.0  77.4  78.9  77.8           75.1
#> # ℹ 494 more rows
```

You can also download high-frequency data for the S&P 500 that we host
on Hugging Face:

``` r
download_data(
  domain = "hf",
  dataset = "high_frequency_sp500",
  start_date = "2007-07-26",
  end_date = "2007-07-27"
)
#> # A tibble: 9,360 × 9
#>    ts                  midquote signed_volume trading_volume depth0_ask
#>    <dttm>                 <dbl>         <dbl>          <dbl>      <dbl>
#>  1 2007-07-26 09:30:05     150.             0      11565577.     13841.
#>  2 2007-07-26 09:30:10     150.             0       3017263.     10795.
#>  3 2007-07-26 09:30:15     150.             0       6108290.     12371.
#>  4 2007-07-26 09:30:20     150.         98630      21820258.     14069.
#>  5 2007-07-26 09:30:25     150.             0        931698      15283.
#>  6 2007-07-26 09:30:30     150.         -4400        661077       5307.
#>  7 2007-07-26 09:30:35     150.        -12000       2102820       3192.
#>  8 2007-07-26 09:30:40     150.         39927      12384621.      6234.
#>  9 2007-07-26 09:30:45     150.        -14671       3105370.      9255.
#> 10 2007-07-26 09:30:50     150.          5347       5518370.      5708.
#> # ℹ 9,350 more rows
#> # ℹ 4 more variables: depth0_bid <dbl>, depth5_ask <dbl>, depth5_bid <dbl>,
#> #   spread <dbl>
```

## Download WRDS Data

You can also download data directly from
[WRDS](https://www.tidy-finance.org/r/wrds-crsp-and-compustat.html) if
you have access to the underlying data and set your credentials via
`Sys.setenv(WRDS_USER = "your_username", WRDS_PASSWORD = "your_password")`
or the `set_wrds_credentials()` helper function.

To download monthly CRSP data:

``` r
download_data(
  domain = "wrds",
  dataset = "crsp_monthly",
  start_date = "2020-01-01",
  end_date = "2020-12-31"
)
#> # A tibble: 43,341 × 13
#>    permno date       calculation_date      ret   shrout    prc primaryexch siccd
#>     <int> <date>     <date>              <dbl>    <dbl>  <dbl> <chr>       <int>
#>  1  10026 2020-01-01 2020-01-31       -0.100     1.89e7 166.   Q            2052
#>  2  10028 2020-01-01 2020-01-31        0.607     2.69e7   2.17 A            5094
#>  3  10032 2020-01-01 2020-01-31       -0.0756    2.92e7  71.1  Q            3670
#>  4  10044 2020-01-01 2020-01-31       -0.0986    6   e6   8.32 Q            2060
#>  5  10051 2020-01-01 2020-01-31       -0.115     3.73e7  24.4  N            8093
#>  6  10104 2020-01-01 2020-01-31       -0.00561   3.21e9  52.4  N            7372
#>  7  10107 2020-01-01 2020-01-31        0.0795    7.61e9 170.   Q            7370
#>  8  10138 2020-01-01 2020-01-31        0.0959    2.35e8 134.   Q            6211
#>  9  10145 2020-01-01 2020-01-31       -0.0214    7.15e8 173.   N            5099
#> 10  10158 2020-01-01 2020-01-31        0.0966    2.90e7  19.2  N            8711
#> # ℹ 43,331 more rows
#> # ℹ 5 more variables: mktcap <dbl>, mktcap_lag <dbl>, exchange <chr>,
#> #   industry <chr>, ret_excess <dbl>
```

To download annual (or quaterly) Compustat data:

``` r
download_data(
  domain = "wrds",
  dataset = "compustat_annual",
  start_date = "2020-01-01",
  end_date = "2020-12-31"
)
#> # A tibble: 11,988 × 23
#>    gvkey  date       datadate        seq     ceq      at      lt  txditc    txdb
#>    <chr>  <date>     <date>        <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
#>  1 001004 2020-05-01 2020-05-31   903.    9.03e2 2.08e+3 1.18e+3 0       0      
#>  2 001019 2020-12-01 2020-12-31    13.5   1.35e1 4.06e+1 2.71e+1 3.61e-1 3.61e-1
#>  3 001045 2020-12-01 2020-12-31 -6867    -6.87e3 6.20e+4 6.89e+4 9   e+0 9   e+0
#>  4 001050 2020-12-01 2020-12-31   203.    2.03e2 4.19e+2 2.16e+2 6.97e+0 6.97e+0
#>  5 001062 2020-11-01 2020-11-30   464.    4.64e2 4.65e+2 8.04e-1 0       0      
#>  6 001075 2020-12-01 2020-12-31  5634.    5.63e3 2.00e+4 1.43e+4 2.33e+3 2.14e+3
#>  7 001076 2020-12-01 2020-12-31   986.    9.86e2 1.32e+3 3.31e+2 1.27e+2 1.27e+2
#>  8 001078 2020-12-01 2020-12-31 32784     3.28e4 7.25e+4 3.95e+4 1.41e+3 1.41e+3
#>  9 001084 2020-12-01 2020-12-31    -2.67 -2.67e0 6.92e-1 3.36e+0 0       0      
#> 10 001096 2020-12-01 2020-12-31  3372.    3.37e3 1.11e+4 6.69e+3 6.48e+2 6.48e+2
#> # ℹ 11,978 more rows
#> # ℹ 14 more variables: itcb <dbl>, pstkrv <dbl>, pstkl <dbl>, pstk <dbl>,
#> #   capx <dbl>, oancf <dbl>, sale <dbl>, cogs <dbl>, xint <dbl>, xsga <dbl>,
#> #   be <dbl>, op <dbl>, at_lag <dbl>, inv <dbl>
```

To download the CRSP-Compustat linking table:

``` r
download_data(
  domain = "wrds",
  dataset = "compustat_annual",
  start_date = "2020-01-01",
  end_date = "2020-12-31"
)
#> # A tibble: 11,988 × 23
#>    gvkey  date       datadate        seq     ceq      at      lt  txditc    txdb
#>    <chr>  <date>     <date>        <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
#>  1 001004 2020-05-01 2020-05-31   903.    9.03e2 2.08e+3 1.18e+3 0       0      
#>  2 001019 2020-12-01 2020-12-31    13.5   1.35e1 4.06e+1 2.71e+1 3.61e-1 3.61e-1
#>  3 001045 2020-12-01 2020-12-31 -6867    -6.87e3 6.20e+4 6.89e+4 9   e+0 9   e+0
#>  4 001050 2020-12-01 2020-12-31   203.    2.03e2 4.19e+2 2.16e+2 6.97e+0 6.97e+0
#>  5 001062 2020-11-01 2020-11-30   464.    4.64e2 4.65e+2 8.04e-1 0       0      
#>  6 001075 2020-12-01 2020-12-31  5634.    5.63e3 2.00e+4 1.43e+4 2.33e+3 2.14e+3
#>  7 001076 2020-12-01 2020-12-31   986.    9.86e2 1.32e+3 3.31e+2 1.27e+2 1.27e+2
#>  8 001078 2020-12-01 2020-12-31 32784     3.28e4 7.25e+4 3.95e+4 1.41e+3 1.41e+3
#>  9 001084 2020-12-01 2020-12-31    -2.67 -2.67e0 6.92e-1 3.36e+0 0       0      
#> 10 001096 2020-12-01 2020-12-31  3372.    3.37e3 1.11e+4 6.69e+3 6.48e+2 6.48e+2
#> # ℹ 11,978 more rows
#> # ℹ 14 more variables: itcb <dbl>, pstkrv <dbl>, pstkl <dbl>, pstk <dbl>,
#> #   capx <dbl>, oancf <dbl>, sale <dbl>, cogs <dbl>, xint <dbl>, xsga <dbl>,
#> #   be <dbl>, op <dbl>, at_lag <dbl>, inv <dbl>
```

To download Enhanced TRACE data for selected bonds:

``` r
download_data(
  domain = "wrds",
  dataset = "trace_enhanced",
  cusips = c("00101JAH9"),
  start_date = "2019-01-01",
  end_date = "2021-12-31"
)
#> # A tibble: 7,694 × 8
#>    cusip_id  trd_exctn_dt trd_exctn_tm rptd_pr entrd_vol_qt yld_pt rpt_side_cd
#>    <chr>     <date>       <time>         <dbl>        <dbl>  <dbl> <chr>      
#>  1 00101JAH9 2019-01-02   09:22:14        92.3         5000   6.12 S          
#>  2 00101JAH9 2019-01-02   09:22:14        92.3         5000   6.12 S          
#>  3 00101JAH9 2019-01-02   12:14:09        91        3314000   6.49 B          
#>  4 00101JAH9 2019-01-02   14:01:00        91.2         5000   6.43 S          
#>  5 00101JAH9 2019-01-02   14:01:00        91.3        24000   6.41 S          
#>  6 00101JAH9 2019-01-02   14:01:00        91.3         5000   6.41 S          
#>  7 00101JAH9 2019-01-02   14:01:03        91.2        24000   6.43 S          
#>  8 00101JAH9 2019-01-02   15:50:07        91.8       315000   6.28 S          
#>  9 00101JAH9 2019-01-03   09:17:16        91.5        50000   6.34 S          
#> 10 00101JAH9 2019-01-03   09:17:16        91.5        50000   6.34 S          
#> # ℹ 7,684 more rows
#> # ℹ 1 more variable: cntra_mp_id <chr>
```

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
