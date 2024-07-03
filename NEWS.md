# tidyfinance 0.2.1

## New features

* Added `domain` and `as_vector` parameters to `list_supported_types()`

## Bug fixes

* Replaced `...` with `additional_columns` parameter and ensured that CRSP and Compustat types consider it correctly
* Removed `mkt_excess` column from type "wrds_crsp_monthly"

## Improvements

* Added `fixed = TRUE` to `grepl()` calls with fixed strings
* Switched to `NA_real_` instead of `as.double(NA)`
* Switched to `toString()` instead of `paste0()` with collapse
* Switched to `dplyr::between()` instead of unequal signs

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
