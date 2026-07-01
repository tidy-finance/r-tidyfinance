# Download Constituent Data

Downloads and processes the constituent data for a specified financial
index. The data is fetched from a remote CSV file, filtered, and cleaned
to provide relevant information about constituents.

## Usage

``` r
download_data_constituents(index)
```

## Arguments

- index:

  A character string specifying the name of the financial index for
  which to download constituent data. The index must be one of the
  supported indexes listed by
  [`list_supported_indexes()`](https://r.tidy-finance.org/reference/list_supported_indexes.md).

## Value

A tibble with five columns:

- symbol:

  The ticker symbol of the equity constituent.

- name:

  The name of the equity constituent.

- location:

  The location where the company is based.

- exchange:

  The exchange where the equity is traded.

- currency:

  The currency in which the equity is traded, derived from the exchange.

The tibble is filtered to exclude non-equity entries, blacklisted
symbols, empty names, and any entries containing the index name or
"CASH".

## Details

The function retrieves the URL of the CSV file for the specified index
from ETF sites, then sends an HTTP GET request to download the CSV file,
and processes the CSV file to extract equity constituents.

The approach is inspired by `tidyquant::tq_index()`, which uses a
different wrapper around other ETFs.

## See also

Other download functions:
[`download_data()`](https://r.tidy-finance.org/reference/download_data.md),
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
  download_data_constituents("DAX")
#> # A tibble: 40 × 5
#>    symbol  name                               location    exchange      currency
#>    <chr>   <chr>                              <chr>       <chr>         <chr>   
#>  1 SIE.DE  SIEMENS N AG                       Deutschland Xetra         EUR     
#>  2 ALV.DE  ALLIANZ                            Deutschland Xetra         EUR     
#>  3 SAP.DE  SAP                                Deutschland Xetra         EUR     
#>  4 ENR.DE  SIEMENS ENERGY N AG                Deutschland Xetra         EUR     
#>  5 AIR.BE  AIRBUS                             Frankreich  Boerse Berlin EUR     
#>  6 IFX.DE  INFINEON TECHNOLOGIES AG           Deutschland Xetra         EUR     
#>  7 DTE.DE  DEUTSCHE TELEKOM N AG              Deutschland Xetra         EUR     
#>  8 MUV2.DE MUENCHENER RUECKVERSICHERUNGS-GESE Deutschland Xetra         EUR     
#>  9 DBK.DE  DEUTSCHE BANK AG                   Deutschland Xetra         EUR     
#> 10 DHL.DE  DEUTSCHE POST AG                   Deutschland Xetra         EUR     
#> # ℹ 30 more rows
# }
```
