# Set WRDS Credentials

Prompts the user to input their WRDS (Wharton Research Data Services)
username and password, and stores these credentials in a `.Renviron`
file. The user can choose to store the `.Renviron` file in either the
project directory or the home directory. If the `.Renviron` file already
contains WRDS credentials, the user will be asked if they want to
overwrite the existing credentials. Additionally, the user has the
option to add the `.Renviron` file to the .gitignore file to prevent it
from being tracked by version control.

## Usage

``` r
set_wrds_credentials()
```

## Value

Invisibly returns `TRUE`. Displays messages to the user based on their
input and actions taken.

## See also

Other WRDS functions:
[`disconnect_connection()`](https://package.tidy-finance.org/reference/disconnect_connection.md),
[`download_data_wrds()`](https://package.tidy-finance.org/reference/download_data_wrds.md),
[`download_data_wrds_ccm_links()`](https://package.tidy-finance.org/reference/download_data_wrds_ccm_links.md),
[`download_data_wrds_compustat()`](https://package.tidy-finance.org/reference/download_data_wrds_compustat.md),
[`download_data_wrds_crsp()`](https://package.tidy-finance.org/reference/download_data_wrds_crsp.md),
[`download_data_wrds_fisd()`](https://package.tidy-finance.org/reference/download_data_wrds_fisd.md),
[`download_data_wrds_trace_enhanced()`](https://package.tidy-finance.org/reference/download_data_wrds_trace_enhanced.md),
[`get_wrds_connection()`](https://package.tidy-finance.org/reference/get_wrds_connection.md)

## Examples

``` r
if (FALSE) { # \dontrun{
set_wrds_credentials()
} # }
```
