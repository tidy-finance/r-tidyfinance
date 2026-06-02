# Download Data from WRDS

Acts as a wrapper to download data from various WRDS datasets including
CRSP, Compustat, and CCM links based on the specified dataset. It is
designed to handle different datasets by redirecting to the appropriate
specific data download function.

## Usage

``` r
download_data_wrds(
  dataset = NULL,
  start_date = NULL,
  end_date = NULL,
  type = deprecated(),
  ...
)
```

## Arguments

- dataset:

  A string specifying the dataset to download. Supported values:
  "crsp_monthly", "crsp_daily" for CRSP data, "compustat_annual",
  "compustat_quarterly" for Compustat data, "ccm_links" for CCM links
  data, "fisd" for FISD data, or "trace_enhanced" for TRACE data.

- start_date:

  Optional. A character string or Date object in "YYYY-MM-DD" format
  specifying the start date for the data. If not provided, a subset of
  the dataset is returned.

- end_date:

  Optional. A character string or Date object in "YYYY-MM-DD" format
  specifying the end date for the data. If not provided, a subset of the
  dataset is returned.

- type:

  **\[deprecated\]** Use `dataset` instead.

- ...:

  Additional arguments passed to specific download functions depending
  on the `dataset`.

## Value

A data frame containing the requested data, with the structure and
contents depending on the specified `dataset`.

## See also

Other WRDS functions:
[`disconnect_connection()`](https://package.tidy-finance.org/reference/disconnect_connection.md),
[`download_data_wrds_ccm_links()`](https://package.tidy-finance.org/reference/download_data_wrds_ccm_links.md),
[`download_data_wrds_compustat()`](https://package.tidy-finance.org/reference/download_data_wrds_compustat.md),
[`download_data_wrds_crsp()`](https://package.tidy-finance.org/reference/download_data_wrds_crsp.md),
[`download_data_wrds_fisd()`](https://package.tidy-finance.org/reference/download_data_wrds_fisd.md),
[`download_data_wrds_trace_enhanced()`](https://package.tidy-finance.org/reference/download_data_wrds_trace_enhanced.md),
[`get_wrds_connection()`](https://package.tidy-finance.org/reference/get_wrds_connection.md),
[`set_wrds_credentials()`](https://package.tidy-finance.org/reference/set_wrds_credentials.md)

## Examples

``` r
if (FALSE) { # \dontrun{
  crsp_monthly <- download_data_wrds(
    "crsp_monthly", "2020-01-01", "2020-12-31"
  )
  compustat_annual <- download_data_wrds(
    "compustat_annual", "2020-01-01", "2020-12-31"
  )
  ccm_links <- download_data_wrds("ccm_links")
  fisd <- download_data_wrds("fisd")
  trace_enhanced <- download_data_wrds(
    "trace_enhanced", cusips = "00101JAH9"
  )
} # }
```
