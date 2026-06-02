# Download and Process Data Based on Domain and Dataset

Downloads and processes data based on the specified domain (e.g.,
Fama-French factors, Global Q factors, or macro predictors), dataset,
and date range. This function checks if the specified domain is
supported and then delegates to the appropriate function for downloading
and processing the data.

## Usage

``` r
download_data(
  domain = NULL,
  dataset = NULL,
  start_date = NULL,
  end_date = NULL,
  type = deprecated(),
  ...
)
```

## Arguments

- domain:

  The domain of the dataset to download (e.g., "famafrench", "globalq",
  "macro_predictors", "wrds", "pseudo", "constituents", "fred",
  "stock_prices", "osap", "tidyfinance"). Use `"pseudo"` to obtain
  pseudo data with the same schema as `"wrds"` for testing or rendering
  without a WRDS subscription.

- dataset:

  Optional. The specific dataset to download within the domain.

- start_date:

  Optional. A character string or Date object in "YYYY-MM-DD" format
  specifying the start date for the data. If not provided, the full
  dataset or a subset is returned, depending on the dataset type.

- end_date:

  Optional. A character string or Date object in "YYYY-MM-DD" format
  specifying the end date for the data. If not provided, the full
  dataset or a subset is returned, depending on the dataset type.

- type:

  **\[deprecated\]** Use `domain` and `dataset` instead.

- ...:

  Additional arguments passed to specific download functions depending
  on the `domain`. For instance, if `domain` is `"constituents"`,
  arguments are passed to
  [`download_data_constituents()`](https://package.tidy-finance.org/reference/download_data_constituents.md).
  If `domain` is `"tidyfinance"` and `dataset` is `"factor_library"`,
  arguments are either filter inputs (e.g., `sorting_variable`,
  `rebalancing`, `fill_all`) or an explicit `ids` vector that bypasses
  the grid filter and downloads the specified portfolios directly via
  [`download_factor_library_ids()`](https://package.tidy-finance.org/reference/download_factor_library_ids.md);
  see
  [`download_data_huggingface()`](https://package.tidy-finance.org/reference/download_data_huggingface.md)
  for details.

## Value

A tibble with processed data, including dates and the relevant financial
metrics, filtered by the specified date range.

## See also

Other download functions:
[`download_data_constituents()`](https://package.tidy-finance.org/reference/download_data_constituents.md),
[`download_data_factors_ff()`](https://package.tidy-finance.org/reference/download_data_factors_ff.md),
[`download_data_factors_q()`](https://package.tidy-finance.org/reference/download_data_factors_q.md),
[`download_data_fred()`](https://package.tidy-finance.org/reference/download_data_fred.md),
[`download_data_huggingface()`](https://package.tidy-finance.org/reference/download_data_huggingface.md),
[`download_data_macro_predictors()`](https://package.tidy-finance.org/reference/download_data_macro_predictors.md),
[`download_data_osap()`](https://package.tidy-finance.org/reference/download_data_osap.md),
[`download_data_risk_free()`](https://package.tidy-finance.org/reference/download_data_risk_free.md),
[`download_data_stock_prices()`](https://package.tidy-finance.org/reference/download_data_stock_prices.md),
[`download_factor_library_grid()`](https://package.tidy-finance.org/reference/download_factor_library_grid.md),
[`download_factor_library_ids()`](https://package.tidy-finance.org/reference/download_factor_library_ids.md)

## Examples

``` r
# \donttest{
download_data(
  "famafrench",
  "Fama/French 5 Factors (2x3) [Daily]",
  "2000-01-01",
  "2020-12-31"
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
download_data("macro_predictors", "monthly", "2000-01-01", "2020-12-31")
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
download_data("constituents", index = "DAX")
#> # A tibble: 40 × 5
#>    symbol  name                               location    exchange      currency
#>    <chr>   <chr>                              <chr>       <chr>         <chr>   
#>  1 SIE.DE  SIEMENS N AG                       Deutschland Xetra         EUR     
#>  2 SAP.DE  SAP                                Deutschland Xetra         EUR     
#>  3 ALV.DE  ALLIANZ                            Deutschland Xetra         EUR     
#>  4 ENR.DE  SIEMENS ENERGY N AG                Deutschland Xetra         EUR     
#>  5 IFX.DE  INFINEON TECHNOLOGIES AG           Deutschland Xetra         EUR     
#>  6 AIR.BE  AIRBUS                             Frankreich  Boerse Berlin EUR     
#>  7 DTE.DE  DEUTSCHE TELEKOM N AG              Deutschland Xetra         EUR     
#>  8 RHM.DE  RHEINMETALL AG                     Deutschland Xetra         EUR     
#>  9 MUV2.DE MUENCHENER RUECKVERSICHERUNGS-GESE Deutschland Xetra         EUR     
#> 10 DBK.DE  DEUTSCHE BANK AG                   Deutschland Xetra         EUR     
#> # ℹ 30 more rows
download_data("fred", series = c("GDP", "CPIAUCNS"))
#> No `start_date` or `end_date` provided. Returning the full data set.
#> # A tibble: 1,677 × 3
#>    date       value series
#>    <date>     <dbl> <chr> 
#>  1 1947-01-01  243. GDP   
#>  2 1947-04-01  246. GDP   
#>  3 1947-07-01  250. GDP   
#>  4 1947-10-01  260. GDP   
#>  5 1948-01-01  266. GDP   
#>  6 1948-04-01  273. GDP   
#>  7 1948-07-01  279. GDP   
#>  8 1948-10-01  280. GDP   
#>  9 1949-01-01  275. GDP   
#> 10 1949-04-01  271. GDP   
#> # ℹ 1,667 more rows
download_data("stock_prices", symbols = c("AAPL", "MSFT"))
#> No `start_date` or `end_date` provided. Using the range 2024-06-02 to
#> 2025-06-02 to avoid downloading large amounts of data.
#> # A tibble: 498 × 8
#>    symbol date          volume  open   low  high close adjusted_close
#>    <chr>  <date>         <dbl> <dbl> <dbl> <dbl> <dbl>          <dbl>
#>  1 AAPL   2024-06-03  50080500  193.  193.  195.  194.           192.
#>  2 AAPL   2024-06-04  47471400  195.  193.  195.  194.           193.
#>  3 AAPL   2024-06-05  54156800  195.  195.  197.  196.           194.
#>  4 AAPL   2024-06-06  41181800  196.  194.  196.  194.           193.
#>  5 AAPL   2024-06-07  53103900  195.  194.  197.  197.           195.
#>  6 AAPL   2024-06-10  97010200  197.  192.  197.  193.           191.
#>  7 AAPL   2024-06-11 172373300  194.  194.  207.  207.           205.
#>  8 AAPL   2024-06-12 198134300  207.  207.  220.  213.           211.
#>  9 AAPL   2024-06-13  97862700  215.  212.  217.  214.           212.
#> 10 AAPL   2024-06-14  70122700  214.  211.  215.  212.           211.
#> # ℹ 488 more rows
download_data(
  "tidyfinance",
  "risk_free",
  "2020-01-01",
  "2020-12-31"
)
#> # A data frame: 12 × 2
#>    date       risk_free
#>    <date>         <dbl>
#>  1 2020-01-01 0.00129  
#>  2 2020-02-01 0.00121  
#>  3 2020-03-01 0.0000338
#>  4 2020-04-01 0.0000845
#>  5 2020-05-01 0.000110 
#>  6 2020-06-01 0.000110 
#>  7 2020-07-01 0.0000760
#>  8 2020-08-01 0.0000676
#>  9 2020-09-01 0.0000676
#> 10 2020-10-01 0.0000676
#> 11 2020-11-01 0.0000676
#> 12 2020-12-01 0.0000676
download_data(
  "tidyfinance",
  "high_frequency_sp500",
  "2007-07-26",
  "2007-07-27"
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
download_data(
  "tidyfinance",
  "factor_library",
  sorting_variable = "52w",
  rebalancing = "annual"
)
#> No `start_date` or `end_date` provided. Returning the full data set.
#> # A data frame: 774 × 17
#>        id date       ret_type      ret sorting_variable min_size_quantile
#>     <int> <date>     <chr>       <dbl> <chr>                        <dbl>
#>  1 286214 1960-07-01 vw        0.0257  52w                            0.2
#>  2 286214 1960-08-01 vw        0.0190  52w                            0.2
#>  3 286214 1960-09-01 vw        0.00840 52w                            0.2
#>  4 286214 1960-10-01 vw        0.0101  52w                            0.2
#>  5 286214 1960-11-01 vw        0.0300  52w                            0.2
#>  6 286214 1960-12-01 vw        0.0202  52w                            0.2
#>  7 286214 1961-01-01 vw        0.00680 52w                            0.2
#>  8 286214 1961-02-01 vw       -0.0127  52w                            0.2
#>  9 286214 1961-03-01 vw        0.00850 52w                            0.2
#> 10 286214 1961-04-01 vw        0.0487  52w                            0.2
#> # ℹ 764 more rows
#> # ℹ 11 more variables: exclude_financials <lgl>, exclude_utilities <lgl>,
#> #   exclude_negative_earnings <lgl>, sorting_variable_lag <chr>,
#> #   rebalancing <chr>, n_portfolios_main <chr>, sorting_method <chr>,
#> #   breakpoints_min_size_threshold <dbl>, n_portfolios_secondary <dbl>,
#> #   breakpoints_exchanges <chr>, weighting_scheme <chr>
download_data("tidyfinance", "factor_library", ids = c(1L, 2L, 3L))
#> No `start_date` or `end_date` provided. Returning the full data set.
#> # A data frame: 2,331 × 17
#>       id date       ret_type        ret sorting_variable min_size_quantile
#>    <int> <date>     <chr>         <dbl> <chr>                        <dbl>
#>  1     2 1960-04-01 vw         0.0158   52w                             NA
#>  2     1 1960-04-01 ew        -0.00398  52w                             NA
#>  3     3 1960-04-01 vw_capped -0.000927 52w                             NA
#>  4     2 1960-05-01 vw        -0.0347   52w                             NA
#>  5     1 1960-05-01 ew        -0.152    52w                             NA
#>  6     3 1960-05-01 vw_capped -0.0987   52w                             NA
#>  7     2 1960-06-01 vw         0.0328   52w                             NA
#>  8     1 1960-06-01 ew        -0.00606  52w                             NA
#>  9     3 1960-06-01 vw_capped  0.0325   52w                             NA
#> 10     2 1960-07-01 vw        -0.00215  52w                             NA
#> # ℹ 2,321 more rows
#> # ℹ 11 more variables: exclude_financials <lgl>, exclude_utilities <lgl>,
#> #   exclude_negative_earnings <lgl>, sorting_variable_lag <chr>,
#> #   rebalancing <chr>, n_portfolios_main <chr>, sorting_method <chr>,
#> #   breakpoints_min_size_threshold <dbl>, n_portfolios_secondary <dbl>,
#> #   breakpoints_exchanges <chr>, weighting_scheme <chr>
download_data("tidyfinance", "factor_library_grid")
#> # A data frame: 841,536 × 14
#>       id sorting_variable min_size_quantile exclude_financials exclude_utilities
#>    <int> <chr>                        <dbl> <lgl>              <lgl>            
#>  1     1 sv_52w                          NA TRUE               TRUE             
#>  2     2 sv_52w                          NA TRUE               TRUE             
#>  3     3 sv_52w                          NA TRUE               TRUE             
#>  4     4 sv_52w                          NA TRUE               TRUE             
#>  5     5 sv_52w                          NA TRUE               TRUE             
#>  6     6 sv_52w                          NA TRUE               TRUE             
#>  7     7 sv_52w                          NA TRUE               TRUE             
#>  8     8 sv_52w                          NA TRUE               TRUE             
#>  9     9 sv_52w                          NA TRUE               TRUE             
#> 10    10 sv_52w                          NA TRUE               TRUE             
#> # ℹ 841,526 more rows
#> # ℹ 9 more variables: exclude_negative_earnings <lgl>,
#> #   sorting_variable_lag <chr>, rebalancing <chr>, n_portfolios_main <dbl>,
#> #   sorting_method <chr>, breakpoints_min_size_threshold <dbl>,
#> #   n_portfolios_secondary <dbl>, breakpoints_exchanges <chr>,
#> #   weighting_scheme <chr>
# }
```
