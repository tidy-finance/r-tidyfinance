# Disconnect Database Connection

Safely disconnects an established database connection using the DBI
package.

## Usage

``` r
disconnect_connection(con)
```

## Arguments

- con:

  A database connection object created by DBI::dbConnect or any similar
  function that establishes a connection to a database.

## Value

`TRUE`, invisibly. Throws an error if the disconnection fails.

## See also

Other WRDS functions:
[`download_data_wrds()`](https://r.tidy-finance.org/reference/download_data_wrds.md),
[`download_data_wrds_ccm_links()`](https://r.tidy-finance.org/reference/download_data_wrds_ccm_links.md),
[`download_data_wrds_compustat()`](https://r.tidy-finance.org/reference/download_data_wrds_compustat.md),
[`download_data_wrds_crsp()`](https://r.tidy-finance.org/reference/download_data_wrds_crsp.md),
[`download_data_wrds_fisd()`](https://r.tidy-finance.org/reference/download_data_wrds_fisd.md),
[`download_data_wrds_trace_enhanced()`](https://r.tidy-finance.org/reference/download_data_wrds_trace_enhanced.md),
[`get_wrds_connection()`](https://r.tidy-finance.org/reference/get_wrds_connection.md),
[`set_wrds_credentials()`](https://r.tidy-finance.org/reference/set_wrds_credentials.md)

## Examples

``` r
if (FALSE) { # \dontrun{
con <- get_wrds_connection()
disconnect_connection(con)
} # }
```
