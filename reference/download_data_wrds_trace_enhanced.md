# Download Enhanced TRACE Data from WRDS

Establishes a connection to the WRDS database to download the specified
CUSIPs trade messages from the Trade Reporting and Compliance Engine
(TRACE). The trade data is cleaned as suggested by Dick-Nielsen (2009,
2014).

## Usage

``` r
download_data_wrds_trace_enhanced(cusips, start_date = NULL, end_date = NULL)
```

## Arguments

- cusips:

  A character vector specifying the 9-digit CUSIPs to download.

- start_date:

  Optional. A character string or Date object in "YYYY-MM-DD" format
  specifying the start date for the data. If not provided, a subset of
  the dataset is returned.

- end_date:

  Optional. A character string or Date object in "YYYY-MM-DD" format
  specifying the end date for the data. If not provided, a subset of the
  dataset is returned.

## Value

A data frame containing the cleaned trade messages from TRACE for the
selected CUSIPs over the time window specified. Output variables include
identifying information (i.e., CUSIP, trade date/time) and
trade-specific information (i.e., price/yield, volume, counterparty, and
reporting side).

## References

Dick-Nielsen, J. (2009). Liquidity biases in TRACE. *Journal of Fixed
Income*, 19(2), 43-55.
[doi:10.3905/jfi.2009.19.2.043](https://doi.org/10.3905/jfi.2009.19.2.043)

Dick-Nielsen, J. (2014). How to clean enhanced TRACE data. Working
Paper. [doi:10.2139/ssrn.2337908](https://doi.org/10.2139/ssrn.2337908)

## See also

Other WRDS functions:
[`disconnect_connection()`](https://r.tidy-finance.org/reference/disconnect_connection.md),
[`download_data_wrds()`](https://r.tidy-finance.org/reference/download_data_wrds.md),
[`download_data_wrds_ccm_links()`](https://r.tidy-finance.org/reference/download_data_wrds_ccm_links.md),
[`download_data_wrds_compustat()`](https://r.tidy-finance.org/reference/download_data_wrds_compustat.md),
[`download_data_wrds_crsp()`](https://r.tidy-finance.org/reference/download_data_wrds_crsp.md),
[`download_data_wrds_fisd()`](https://r.tidy-finance.org/reference/download_data_wrds_fisd.md),
[`get_wrds_connection()`](https://r.tidy-finance.org/reference/get_wrds_connection.md),
[`set_wrds_credentials()`](https://r.tidy-finance.org/reference/set_wrds_credentials.md)

## Examples

``` r
if (FALSE) { # \dontrun{
download_data_wrds_trace_enhanced("00101JAH9", "2019-01-01", "2021-12-31")
} # }
```
