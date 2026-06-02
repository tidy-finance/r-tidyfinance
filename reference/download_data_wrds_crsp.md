# Download Data from WRDS CRSP

Downloads and processes stock return data from the CRSP database for a
specified period. Users can choose between monthly and daily datasets.
The function also adjusts returns for delisting and calculates market
capitalization and excess returns over the risk-free rate.

## Usage

``` r
download_data_wrds_crsp(
  dataset = NULL,
  start_date = NULL,
  end_date = NULL,
  type = deprecated(),
  batch_size = 500,
  version = "v2",
  additional_columns = NULL,
  add_ccm_links = FALSE,
  adjust_volume = FALSE
)
```

## Arguments

- dataset:

  A string specifying the CRSP dataset to download: "crsp_monthly" or
  "crsp_daily".

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

- batch_size:

  An optional integer specifying the batch size for processing daily
  data, with a default of 500.

- version:

  An optional character specifying which CRSP version to use. "v2" (the
  default) uses the updated second version of CRSP, and "v1" downloads
  the legacy version of CRSP.

- additional_columns:

  Additional columns from the CRSP monthly or daily data as a character
  vector.

- add_ccm_links:

  A logical indicating whether CRSP-Compustat links should be added
  automatically using
  [`download_data_wrds_ccm_links()`](https://package.tidy-finance.org/reference/download_data_wrds_ccm_links.md).

- adjust_volume:

  A logical indicating whether daily CRSP trading volume data should be
  adjusted according to Gao & Ritter (2010).

## Value

A data frame containing CRSP stock returns, adjusted for delistings,
along with calculated market capitalization and excess returns over the
risk-free rate. The structure of the returned data frame depends on the
selected dataset.

## References

Gao, X., & Ritter, J. R. (2010). The marketing of seasoned equity
offerings. *Journal of Financial Economics*, 97(1), 33-52.
[doi:10.1016/j.jfineco.2010.03.007](https://doi.org/10.1016/j.jfineco.2010.03.007)

## See also

Other WRDS functions:
[`disconnect_connection()`](https://package.tidy-finance.org/reference/disconnect_connection.md),
[`download_data_wrds()`](https://package.tidy-finance.org/reference/download_data_wrds.md),
[`download_data_wrds_ccm_links()`](https://package.tidy-finance.org/reference/download_data_wrds_ccm_links.md),
[`download_data_wrds_compustat()`](https://package.tidy-finance.org/reference/download_data_wrds_compustat.md),
[`download_data_wrds_fisd()`](https://package.tidy-finance.org/reference/download_data_wrds_fisd.md),
[`download_data_wrds_trace_enhanced()`](https://package.tidy-finance.org/reference/download_data_wrds_trace_enhanced.md),
[`get_wrds_connection()`](https://package.tidy-finance.org/reference/get_wrds_connection.md),
[`set_wrds_credentials()`](https://package.tidy-finance.org/reference/set_wrds_credentials.md)

## Examples

``` r
if (FALSE) { # \dontrun{
crsp_monthly <- download_data_wrds_crsp(
  "crsp_monthly",
  "2020-11-01",
  "2020-12-31"
)
crsp_daily <- download_data_wrds_crsp(
  "crsp_daily",
  "2020-12-01",
  "2020-12-31"
)

# Add additional columns
download_data_wrds_crsp(
  "crsp_monthly",
  "2020-11-01",
  "2020-12-31",
  additional_columns = c("mthvol", "mthvolflg")
)
} # }
```
