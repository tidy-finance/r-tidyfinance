# Download CCM Links from WRDS

Downloads data from the WRDS CRSP/Compustat Merged (CCM) links database.
It allows users to specify the type of links (`linktype`) and the
primacy of the link (`linkprim`).

## Usage

``` r
download_data_wrds_ccm_links(linktype = c("LU", "LC"), linkprim = c("P", "C"))
```

## Arguments

- linktype:

  A character vector indicating the type of link to download. The
  default is `c("LU", "LC")`, where "LU" stands for "Link Up" and "LC"
  for "Link CRSP".

- linkprim:

  A character vector indicating the primacy of the link. Default is
  `c("P", "C")`, where "P" indicates primary and "C" indicates
  conditional links.

## Value

A data frame with the columns `permno`, `gvkey`, `linkdt`, and
`linkenddt`, where `linkenddt` is the end date of the link, and missing
end dates are replaced with today's date.

## See also

Other WRDS functions:
[`disconnect_connection()`](https://package.tidy-finance.org/reference/disconnect_connection.md),
[`download_data_wrds()`](https://package.tidy-finance.org/reference/download_data_wrds.md),
[`download_data_wrds_compustat()`](https://package.tidy-finance.org/reference/download_data_wrds_compustat.md),
[`download_data_wrds_crsp()`](https://package.tidy-finance.org/reference/download_data_wrds_crsp.md),
[`download_data_wrds_fisd()`](https://package.tidy-finance.org/reference/download_data_wrds_fisd.md),
[`download_data_wrds_trace_enhanced()`](https://package.tidy-finance.org/reference/download_data_wrds_trace_enhanced.md),
[`get_wrds_connection()`](https://package.tidy-finance.org/reference/get_wrds_connection.md),
[`set_wrds_credentials()`](https://package.tidy-finance.org/reference/set_wrds_credentials.md)

## Examples

``` r
if (FALSE) { # \dontrun{
  ccm_links <- download_data_wrds_ccm_links(linktype = "LU", linkprim = "P")
} # }
```
