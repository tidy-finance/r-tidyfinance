# Download and Process FRED-MD / FRED-QD (McCracken-Ng) Databases

Downloads a vintage of the FRED-MD (monthly) or FRED-QD (quarterly)
macroeconomic database - a curated, balanced panel of macro series
maintained by the Federal Reserve Bank of St. Louis - and returns it as
a wide tibble (one column per series), matching the layout of other
factor/predictor datasets such as
[`download_data_factors_ff()`](https://r.tidy-finance.org/reference/download_data_factors_ff.md).

## Usage

``` r
download_data_fred_md(
  database = "FRED-MD",
  transform = FALSE,
  vintage = "latest"
)
```

## Arguments

- database:

  Which database to download: `"FRED-MD"` (monthly) or `"FRED-QD"`
  (quarterly). The frequency is implied by the database.

- transform:

  Logical. If `TRUE`, apply each series' McCracken-Ng stationarity
  transform (tcode 1-7). If `FALSE` (the default), return raw levels.

- vintage:

  Which release(s) to download: `"latest"` (the default, the current
  vintage), a `"YYYY-MM"` label for one historical release (e.g.
  `"2020-03"`; recent ones are hosted individually, older ones are
  extracted from the St. Louis Fed vintage archives), or `"all"` for
  every archived vintage stacked (the full real-time panel).

## Value

A tibble `[date, <series...>]`. For a specific `vintage` or `"all"`, a
`vintage` column (the `"YYYY-MM"` release label) is inserted after
`date`.

## Details

Each series carries a McCracken-Ng stationarity transform code (tcode,
1-7); `transform = TRUE` applies it per series (all transforms are
causal, i.e. point-in-time safe). FRED-MD/QD publish a new vintage every
month, so a specific historical release can be requested via `vintage`,
or the entire real-time panel via `vintage = "all"`, for point-in-time
analysis that avoids look-ahead bias from data revisions. Recent
vintages are hosted individually; older ones are extracted from the St.
Louis Fed historical vintage archive ZIP files.

## References

McCracken, M. W., & Ng, S. (2016). FRED-MD: A monthly database for
macroeconomic research. *Journal of Business & Economic Statistics*,
34(4), 574-589.
[doi:10.1080/07350015.2015.1086655](https://doi.org/10.1080/07350015.2015.1086655)

McCracken, M. W., & Ng, S. (2021). FRED-QD: A quarterly database for
macroeconomic research. *Federal Reserve Bank of St. Louis Review*,
103(1), 1-44.
[doi:10.20955/r.103.1-44](https://doi.org/10.20955/r.103.1-44)

## See also

Other download functions:
[`download_data()`](https://r.tidy-finance.org/reference/download_data.md),
[`download_data_constituents()`](https://r.tidy-finance.org/reference/download_data_constituents.md),
[`download_data_factors_ff()`](https://r.tidy-finance.org/reference/download_data_factors_ff.md),
[`download_data_factors_q()`](https://r.tidy-finance.org/reference/download_data_factors_q.md),
[`download_data_fred()`](https://r.tidy-finance.org/reference/download_data_fred.md),
[`download_data_huggingface()`](https://r.tidy-finance.org/reference/download_data_huggingface.md),
[`download_data_jkp()`](https://r.tidy-finance.org/reference/download_data_jkp.md),
[`download_data_macro_predictors()`](https://r.tidy-finance.org/reference/download_data_macro_predictors.md),
[`download_data_osap()`](https://r.tidy-finance.org/reference/download_data_osap.md),
[`download_data_pastor_stambaugh()`](https://r.tidy-finance.org/reference/download_data_pastor_stambaugh.md),
[`download_data_risk_free()`](https://r.tidy-finance.org/reference/download_data_risk_free.md),
[`download_data_stambaugh_yuan()`](https://r.tidy-finance.org/reference/download_data_stambaugh_yuan.md),
[`download_data_stock_prices()`](https://r.tidy-finance.org/reference/download_data_stock_prices.md),
[`download_factor_library_grid()`](https://r.tidy-finance.org/reference/download_factor_library_grid.md),
[`download_factor_library_ids()`](https://r.tidy-finance.org/reference/download_factor_library_ids.md)

## Examples

``` r
# \donttest{
  download_data_fred_md("FRED-MD")
#> # A tibble: 801 × 127
#>    date         RPI W875RX1 DPCERA3M086SBEA CMRMTSPLx RETAILx INDPRO IPFPNSS
#>    <date>     <dbl>   <dbl>           <dbl>     <dbl>   <dbl>  <dbl>   <dbl>
#>  1 1959-01-01 2584.   2426             15.2   276677.  17689.   22.0    23.4
#>  2 1959-02-01 2594.   2435.            15.3   278714.  17819.   22.4    23.7
#>  3 1959-03-01 2610.   2453.            15.5   277775.  17968.   22.7    23.8
#>  4 1959-04-01 2627.   2470             15.4   283363.  17979.   23.2    24.2
#>  5 1959-05-01 2643.   2486.            15.6   285307.  18120.   23.5    24.4
#>  6 1959-06-01 2651.   2494.            15.7   285280.  18285.   23.6    24.6
#>  7 1959-07-01 2649.   2492             15.6   288768.  18279.   23.0    24.6
#>  8 1959-08-01 2634.   2478.            15.7   273993.  18395.   22.2    24.4
#>  9 1959-09-01 2636.   2478.            15.9   278039.  18155.   22.2    24.3
#> 10 1959-10-01 2640.   2481.            15.8   278490.  18288.   22.0    24.2
#> # ℹ 791 more rows
#> # ℹ 119 more variables: IPFINAL <dbl>, IPCONGD <dbl>, IPDCONGD <dbl>,
#> #   IPNCONGD <dbl>, IPBUSEQ <dbl>, IPMAT <dbl>, IPDMAT <dbl>, IPNMAT <dbl>,
#> #   IPMANSICS <dbl>, IPB51222S <dbl>, IPFUELS <dbl>, CUMFNS <dbl>, HWI <dbl>,
#> #   HWIURATIO <dbl>, CLF16OV <dbl>, CE16OV <dbl>, UNRATE <dbl>, UEMPMEAN <dbl>,
#> #   UEMPLT5 <dbl>, UEMP5TO14 <dbl>, UEMP15OV <dbl>, UEMP15T26 <dbl>,
#> #   UEMP27OV <dbl>, CLAIMSx <dbl>, PAYEMS <dbl>, USGOOD <dbl>, …
  download_data_fred_md("FRED-MD", transform = TRUE)
#> # A tibble: 801 × 127
#>    date             RPI   W875RX1 DPCERA3M086SBEA  CMRMTSPLx   RETAILx   INDPRO
#>    <date>         <dbl>     <dbl>           <dbl>      <dbl>     <dbl>    <dbl>
#>  1 1959-01-01 NA        NA               NA       NA         NA        NA      
#>  2 1959-02-01  0.00388   0.00362          0.0103   0.00734    0.00731   0.0194 
#>  3 1959-03-01  0.00646   0.00732          0.00940 -0.00337    0.00832   0.0143 
#>  4 1959-04-01  0.00651   0.00703         -0.00362  0.0199     0.000616  0.0211 
#>  5 1959-05-01  0.00580   0.00662          0.0120   0.00684    0.00780   0.0150 
#>  6 1959-06-01  0.00307   0.00301          0.00364 -0.0000968  0.00906   0.00114
#>  7 1959-07-01 -0.000580 -0.000762        -0.00339  0.0122    -0.000330 -0.0242 
#>  8 1959-08-01 -0.00565  -0.00575          0.00600 -0.0525     0.00636  -0.0345 
#>  9 1959-09-01  0.000763  0                0.0100   0.0147    -0.0132   -0.00121
#> 10 1959-10-01  0.00127   0.00117         -0.00683  0.00162    0.00729  -0.00729
#> # ℹ 791 more rows
#> # ℹ 120 more variables: IPFPNSS <dbl>, IPFINAL <dbl>, IPCONGD <dbl>,
#> #   IPDCONGD <dbl>, IPNCONGD <dbl>, IPBUSEQ <dbl>, IPMAT <dbl>, IPDMAT <dbl>,
#> #   IPNMAT <dbl>, IPMANSICS <dbl>, IPB51222S <dbl>, IPFUELS <dbl>,
#> #   CUMFNS <dbl>, HWI <dbl>, HWIURATIO <dbl>, CLF16OV <dbl>, CE16OV <dbl>,
#> #   UNRATE <dbl>, UEMPMEAN <dbl>, UEMPLT5 <dbl>, UEMP5TO14 <dbl>,
#> #   UEMP15OV <dbl>, UEMP15T26 <dbl>, UEMP27OV <dbl>, CLAIMSx <dbl>, …
  download_data_fred_md("FRED-MD", vintage = "2020-03")
#> # A tibble: 734 × 129
#>    date       vintage   RPI W875RX1 DPCERA3M086SBEA CMRMTSPLx RETAILx INDPRO
#>    <date>     <chr>   <dbl>   <dbl>           <dbl>     <dbl>   <dbl>  <dbl>
#>  1 1959-01-01 2020-03 2437.   2289.            17.3   292259.  18236.   22.6
#>  2 1959-02-01 2020-03 2447.   2297             17.5   294430.  18370.   23.1
#>  3 1959-03-01 2020-03 2463.   2314             17.6   293425.  18523.   23.4
#>  4 1959-04-01 2020-03 2479.   2330.            17.6   299332.  18534.   23.9
#>  5 1959-05-01 2020-03 2493.   2346.            17.8   301373.  18680.   24.3
#>  6 1959-06-01 2020-03 2501.   2353.            17.9   301365.  18850.   24.3
#>  7 1959-07-01 2020-03 2500.   2351             17.8   305035.  18844.   23.7
#>  8 1959-08-01 2020-03 2485.   2338.            17.9   289425.  18964.   22.9
#>  9 1959-09-01 2020-03 2487.   2338.            18.1   293715.  18716.   22.9
#> 10 1959-10-01 2020-03 2490.   2340.            18.0   294178.  18853.   22.7
#> # ℹ 724 more rows
#> # ℹ 121 more variables: IPFPNSS <dbl>, IPFINAL <dbl>, IPCONGD <dbl>,
#> #   IPDCONGD <dbl>, IPNCONGD <dbl>, IPBUSEQ <dbl>, IPMAT <dbl>, IPDMAT <dbl>,
#> #   IPNMAT <dbl>, IPMANSICS <dbl>, IPB51222S <dbl>, IPFUELS <dbl>,
#> #   CUMFNS <dbl>, HWI <dbl>, HWIURATIO <dbl>, CLF16OV <dbl>, CE16OV <dbl>,
#> #   UNRATE <dbl>, UEMPMEAN <dbl>, UEMPLT5 <dbl>, UEMP5TO14 <dbl>,
#> #   UEMP15OV <dbl>, UEMP15T26 <dbl>, UEMP27OV <dbl>, CLAIMSx <dbl>, …
  download_data_fred_md("FRED-MD", vintage = "all")
#> Error in vapply(series_cols, function(col) as.integer(round(as.numeric(raw[[col]][1]))),     integer(1)): values must be length 1,
#>  but FUN(X[[126]]) result is length 0
  download_data_fred_md("FRED-QD")
#> # A tibble: 267 × 246
#>    date       GDPC1 PCECC96 PCDGx PCESVx PCNDx GPDIC1  FPIx Y033RC1Q027SBEAx
#>    <date>     <dbl>   <dbl> <dbl>  <dbl> <dbl>  <dbl> <dbl>            <dbl>
#>  1 1959-03-01 3352.   2039.  68.7  1374.  689.   355.  357.             47.8
#>  2 1959-06-01 3428.   2071.  71.2  1395.  695.   382.  368.             49.2
#>  3 1959-09-01 3430.   2092.  72.6  1414.  697.   358.  372.             50.8
#>  4 1959-12-01 3440.   2094.  69.2  1431.  702.   369.  368.             50.7
#>  5 1960-03-01 3517.   2115.  71.4  1444.  704.   407.  380.             52.5
#>  6 1960-06-01 3498.   2141.  73.0  1460.  711.   369.  373.             53.4
#>  7 1960-09-01 3515.   2133.  72.4  1458.  707.   368.  365.             51.1
#>  8 1960-12-01 3470.   2135.  70.6  1472.  708.   327.  364.             49.3
#>  9 1961-03-01 3494.   2135.  66.7  1487.  712.   335.  361.             47.7
#> 10 1961-06-01 3553.   2166.  68.1  1508.  721.   359.  367.             50.0
#> # ℹ 257 more rows
#> # ℹ 237 more variables: PNFIx <dbl>, PRFIx <dbl>, A014RE1Q156NBEA <dbl>,
#> #   GCEC1 <dbl>, A823RL1Q225SBEA <dbl>, FGRECPTx <dbl>, SLCEx <dbl>,
#> #   EXPGSC1 <dbl>, IMPGSC1 <dbl>, DPIC96 <dbl>, OUTNFB <dbl>, OUTBS <dbl>,
#> #   OUTMS <dbl>, INDPRO <dbl>, IPFINAL <dbl>, IPCONGD <dbl>, IPMAT <dbl>,
#> #   IPDMAT <dbl>, IPNMAT <dbl>, IPDCONGD <dbl>, IPB51110SQ <dbl>,
#> #   IPNCONGD <dbl>, IPBUSEQ <dbl>, IPB51220SQ <dbl>, TCU <dbl>, CUMFNS <dbl>, …
# }
```
