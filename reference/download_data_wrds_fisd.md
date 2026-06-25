# Download Filtered FISD Data from WRDS

Establishes a connection to the WRDS database to download a filtered
subset of the FISD (Fixed Income Securities Database). The function
filters the `fisd_mergedissue` and `fisd_mergedissuer` tables based on
several criteria related to the securities, such as security level, bond
type, coupon type, and others, focusing on specific attributes that
denote the nature of the securities. It finally returns a data frame
with selected fields from the `fisd_mergedissue` table after joining it
with issuer information from the `fisd_mergedissuer` table for issuers
domiciled in the USA.

## Usage

``` r
download_data_wrds_fisd(additional_columns = NULL)
```

## Arguments

- additional_columns:

  Additional columns from the FISD table as a character vector.

## Value

A data frame containing a subset of FISD data with fields related to the
bond's characteristics and issuer information. This includes complete
CUSIP, maturity date, offering amount, offering date, dated date,
interest frequency, coupon, last interest date, issue ID, issuer ID, and
SIC code of the issuer.

## See also

Other WRDS functions:
[`disconnect_connection()`](https://r.tidy-finance.org/reference/disconnect_connection.md),
[`download_data_wrds()`](https://r.tidy-finance.org/reference/download_data_wrds.md),
[`download_data_wrds_ccm_links()`](https://r.tidy-finance.org/reference/download_data_wrds_ccm_links.md),
[`download_data_wrds_compustat()`](https://r.tidy-finance.org/reference/download_data_wrds_compustat.md),
[`download_data_wrds_crsp()`](https://r.tidy-finance.org/reference/download_data_wrds_crsp.md),
[`download_data_wrds_trace_enhanced()`](https://r.tidy-finance.org/reference/download_data_wrds_trace_enhanced.md),
[`get_wrds_connection()`](https://r.tidy-finance.org/reference/get_wrds_connection.md),
[`set_wrds_credentials()`](https://r.tidy-finance.org/reference/set_wrds_credentials.md)

## Examples

``` r
if (FALSE) { # \dontrun{
fisd <- download_data_wrds_fisd()
fisd_extended <- download_data_wrds_fisd(
  additional_columns = c("asset_backed", "defeased")
)
} # }
```
