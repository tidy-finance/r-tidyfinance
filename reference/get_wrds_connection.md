# Establish a Connection to the WRDS Database

Establishes a connection to the Wharton Research Data Services (WRDS)
database using the `RPostgres` package. It requires that the `RPostgres`
package be installed and that valid WRDS credentials be set as
environment variables.

## Usage

``` r
get_wrds_connection()
```

## Value

An object of class `DBIConnection` representing the connection to the
WRDS database. This object can be used with other DBI-compliant
functions to interact with the database.

## Details

The function checks if the `RPostgres` package is installed before
attempting to establish a connection. It uses the host, dbname, port,
and sslmode as fixed parameters for the connection. Users must set their
WRDS username and password as environment variables `WRDS_USER` and
`WRDS_PASSWORD`, respectively, before using this function.

## See also

[`RPostgres::Postgres()`](https://rpostgres.r-dbi.org/reference/Postgres.html),
[`DBI::dbDisconnect()`](https://dbi.r-dbi.org/reference/dbDisconnect.html)
for more information on managing database connections.

Other WRDS functions:
[`disconnect_connection()`](https://r.tidy-finance.org/reference/disconnect_connection.md),
[`download_data_wrds()`](https://r.tidy-finance.org/reference/download_data_wrds.md),
[`download_data_wrds_ccm_links()`](https://r.tidy-finance.org/reference/download_data_wrds_ccm_links.md),
[`download_data_wrds_compustat()`](https://r.tidy-finance.org/reference/download_data_wrds_compustat.md),
[`download_data_wrds_crsp()`](https://r.tidy-finance.org/reference/download_data_wrds_crsp.md),
[`download_data_wrds_fisd()`](https://r.tidy-finance.org/reference/download_data_wrds_fisd.md),
[`download_data_wrds_trace_enhanced()`](https://r.tidy-finance.org/reference/download_data_wrds_trace_enhanced.md),
[`set_wrds_credentials()`](https://r.tidy-finance.org/reference/set_wrds_credentials.md)

## Examples

``` r
if (FALSE) { # \dontrun{
  # Before using this function, set your WRDS credentials:
  # Sys.setenv(WRDS_USER = "your_username", WRDS_PASSWORD = "your_password")

  # con <- get_wrds_connection()
  # Use `con` with DBI-compliant functions to interact with the WRDS database
  # Remember to disconnect after use:
  # disconnect_connection(con)
} # }
```
