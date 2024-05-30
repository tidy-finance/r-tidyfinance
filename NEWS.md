# tidyfinance (development version)

# tidyfinance 0.2.0

## New features

* Added `vignettes/using-tidyfinance`
* Added `set_wrds_credentials()` function for a guided tour to store login data
* Added support for `"factors_ff_industry_*"` data types

## Bug fixes

* Removed `hml` and `smb` columns from `"wrds_crsp_monthly"` output
* Fixed stock filters for `"v2"` of `"wrds_crsp_*"` data types

## Improvements

* Relaxed package version requirements as much as possible with the current set of packages
* Split up the `download_data*` functions into multiple files for better maintenance

# tidyfinance 0.1.0

* Initial CRAN submission.
