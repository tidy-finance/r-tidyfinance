# Download Data from WRDS CRSP

Downloads and processes stock return data from the CRSP database for a
specified period. Users can choose between monthly and daily datasets.
The function also includes delisting returns (see the section on
delisting returns) and calculates market capitalization and excess
returns over the risk-free rate.

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
  adjust_volume = FALSE,
  quiet = FALSE
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
  [`download_data_wrds_ccm_links()`](https://r.tidy-finance.org/reference/download_data_wrds_ccm_links.md).

- adjust_volume:

  A logical indicating whether daily CRSP trading volume data should be
  adjusted according to Gao & Ritter (2010).

- quiet:

  A logical indicating whether to suppress the messages about the WRDS
  connection and the download progress. Defaults to `FALSE`.

## Value

A data frame containing CRSP stock returns, including delisting returns,
along with calculated market capitalization and excess returns over the
risk-free rate. The structure of the returned data frame depends on the
selected dataset.

## Delisting returns

How delisting returns enter the data depends on `version`:

- `"v2"` (CIZ): CRSP already includes the delisting return in the
  monthly (`mthret`) and daily (`dlyret`) returns of the month and the
  day in which a stock delists, so `ret` and `ret_excess` include it
  without further adjustment. The common-stock screens of the security
  history (`crsp.stksecurityinfohist`) are evaluated on the stock's last
  trading day for these observations, because the history holds no
  active record after it. Without this, the month or day that carries
  the delisting return would be dropped.

- `"v1"` (legacy SIZ), monthly: the delisting return `dlret` from
  `crsp.msedelist` replaces the return of the delisting month in
  `ret_adj`, which `ret_excess` is based on. If `dlret` is missing, the
  return is set to -30% for performance-related delistings (codes 500,
  520, 551-574, 580, and 584; Shumway, 1997), kept for code 100, and set
  to -100% otherwise. `ret` is the unadjusted return.

- `"v1"` (legacy SIZ), daily: the delisting return replaces the return
  on the delisting date, and later observations are dropped.

## References

Shumway, T. (1997). The delisting bias in CRSP data. *Journal of
Finance*, 52(1), 327-340.
[doi:10.1111/j.1540-6261.1997.tb03818.x](https://doi.org/10.1111/j.1540-6261.1997.tb03818.x)

Gao, X., & Ritter, J. R. (2010). The marketing of seasoned equity
offerings. *Journal of Financial Economics*, 97(1), 33-52.
[doi:10.1016/j.jfineco.2010.03.007](https://doi.org/10.1016/j.jfineco.2010.03.007)

## See also

Other WRDS functions:
[`disconnect_connection()`](https://r.tidy-finance.org/reference/disconnect_connection.md),
[`download_data_wrds()`](https://r.tidy-finance.org/reference/download_data_wrds.md),
[`download_data_wrds_ccm_links()`](https://r.tidy-finance.org/reference/download_data_wrds_ccm_links.md),
[`download_data_wrds_compustat()`](https://r.tidy-finance.org/reference/download_data_wrds_compustat.md),
[`download_data_wrds_fisd()`](https://r.tidy-finance.org/reference/download_data_wrds_fisd.md),
[`download_data_wrds_trace_enhanced()`](https://r.tidy-finance.org/reference/download_data_wrds_trace_enhanced.md),
[`get_wrds_connection()`](https://r.tidy-finance.org/reference/get_wrds_connection.md),
[`set_wrds_credentials()`](https://r.tidy-finance.org/reference/set_wrds_credentials.md)

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
