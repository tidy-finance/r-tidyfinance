# Download Data from WRDS Compustat

Downloads financial data from the WRDS Compustat database for a given
dataset, start date, and end date. It filters the data according to
industry format, data format, and consolidation level, and returns the
most current data for each reporting period. Additionally, the annual
data also includes the calculated book equity (be), operating
profitability (op), and investment (inv) for each company following Fama
& French (1993, 2015), as well as income before extraordinary items
(ib).

## Usage

``` r
download_data_wrds_compustat(
  dataset = NULL,
  start_date = NULL,
  end_date = NULL,
  type = deprecated(),
  additional_columns = NULL,
  only_usd = FALSE,
  only_us = deprecated()
)
```

## Arguments

- dataset:

  The dataset to download ("compustat_annual" or "compustat_quarterly").

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

- additional_columns:

  Additional columns from the Compustat table as a character vector.

- only_usd:

  A logical indicating whether only USD-denominated shares should be
  returned.

- only_us:

  **\[deprecated\]** Use `only_usd` instead.

## Value

A data frame with financial data for the specified period, including
variables for book equity (be), operating profitability (op), investment
(inv), and others.

## References

Fama, E. F., & French, K. R. (1993). Common risk factors in the returns
on stocks and bonds. *Journal of Financial Economics*, 33(1), 3-56.
[doi:10.1016/0304-405X(93)90023-5](https://doi.org/10.1016/0304-405X%2893%2990023-5)

Fama, E. F., & French, K. R. (2015). A five-factor asset pricing model.
*Journal of Financial Economics*, 116(1), 1-22.
[doi:10.1016/j.jfineco.2014.10.010](https://doi.org/10.1016/j.jfineco.2014.10.010)

## See also

Other WRDS functions:
[`disconnect_connection()`](https://r.tidy-finance.org/reference/disconnect_connection.md),
[`download_data_wrds()`](https://r.tidy-finance.org/reference/download_data_wrds.md),
[`download_data_wrds_ccm_links()`](https://r.tidy-finance.org/reference/download_data_wrds_ccm_links.md),
[`download_data_wrds_crsp()`](https://r.tidy-finance.org/reference/download_data_wrds_crsp.md),
[`download_data_wrds_fisd()`](https://r.tidy-finance.org/reference/download_data_wrds_fisd.md),
[`download_data_wrds_trace_enhanced()`](https://r.tidy-finance.org/reference/download_data_wrds_trace_enhanced.md),
[`get_wrds_connection()`](https://r.tidy-finance.org/reference/get_wrds_connection.md),
[`set_wrds_credentials()`](https://r.tidy-finance.org/reference/set_wrds_credentials.md)

## Examples

``` r
if (FALSE) { # \dontrun{
download_data_wrds_compustat("compustat_annual", "2020-01-01", "2020-12-31")
download_data_wrds_compustat(
  "compustat_quarterly",
  "2020-01-01",
  "2020-12-31"
)

  # Add additional columns
download_data_wrds_compustat(
  "compustat_annual",
  additional_columns = c("aodo", "aldo")
)
} # }
```
