# Download and Process Damodaran Industry Data

Downloads industry averages from [Aswath Damodaran's data
library](https://pages.stern.nyu.edu/~adamodar/New_Home_Page/data.html).
Currently, the cost of capital (WACC) by industry is supported via
`dataset = "wacc"`. The source files are Excel workbooks, so the
function requires the readxl package.

## Usage

``` r
download_data_damodaran(dataset = "wacc", region = "us", year = NULL)
```

## Arguments

- dataset:

  The dataset to download. Currently only `"wacc"`.

- region:

  A character vector of regions: `"us"` (the default), `"europe"`,
  `"japan"`, `"emerging"`, `"global"`, `"china"`, `"india"`, or `"rest"`
  (Australia, New Zealand, and Canada).

- year:

  Optional. An integer vector of years. If not provided, the most recent
  data is returned. Years that are not available are skipped with a
  warning.

## Value

A tibble with one row per region, year, and industry and the columns
`region`, `year`, `industry`, `number_of_firms`, `beta`,
`cost_of_equity`, `equity_to_capital` (E/(D+E)), `sd_stock` (standard
deviation of stock returns), `cost_of_debt`, `tax_rate`,
`after_tax_cost_of_debt`, `debt_to_capital` (D/(D+E)), `wacc` (cost of
capital in US dollars), and `wacc_local` (cost of capital in local
currency, `NA` where the source file does not provide it). Rates are
decimals. The rows `"Total Market"` and, in recent files,
`"Total Market (without financials)"` hold the market-wide averages.

## Details

Damodaran updates the data each January with the data at the end of the
previous year. The `year` column refers to that previous year, e.g., the
file published in January 2026 has `year = 2025`. Earlier years are read
from the archive of the data library. The archive starts in 1998 for the
US, in 2013 for Europe, Japan, emerging markets, global, China, and
India, and in 2015 for Australia, New Zealand, and Canada. China and
India skip some years.

## See also

Other download functions:
[`download_data()`](https://r.tidy-finance.org/reference/download_data.md),
[`download_data_constituents()`](https://r.tidy-finance.org/reference/download_data_constituents.md),
[`download_data_factors_ff()`](https://r.tidy-finance.org/reference/download_data_factors_ff.md),
[`download_data_factors_q()`](https://r.tidy-finance.org/reference/download_data_factors_q.md),
[`download_data_fred()`](https://r.tidy-finance.org/reference/download_data_fred.md),
[`download_data_fred_md()`](https://r.tidy-finance.org/reference/download_data_fred_md.md),
[`download_data_huggingface()`](https://r.tidy-finance.org/reference/download_data_huggingface.md),
[`download_data_jkp()`](https://r.tidy-finance.org/reference/download_data_jkp.md),
[`download_data_macro_predictors()`](https://r.tidy-finance.org/reference/download_data_macro_predictors.md),
[`download_data_osap()`](https://r.tidy-finance.org/reference/download_data_osap.md),
[`download_data_pastor_stambaugh()`](https://r.tidy-finance.org/reference/download_data_pastor_stambaugh.md),
[`download_data_risk_free()`](https://r.tidy-finance.org/reference/download_data_risk_free.md),
[`download_data_stambaugh_yuan()`](https://r.tidy-finance.org/reference/download_data_stambaugh_yuan.md),
[`download_data_stock_prices()`](https://r.tidy-finance.org/reference/download_data_stock_prices.md),
[`download_factor_library_grid()`](https://r.tidy-finance.org/reference/download_factor_library_grid.md),
[`download_factor_library_ids()`](https://r.tidy-finance.org/reference/download_factor_library_ids.md)

## Examples

``` r
# \donttest{
  wacc <- download_data_damodaran("wacc")
  wacc_panel <- download_data_damodaran(
    "wacc", region = c("us", "europe"), year = 2020:2024
  )
# }
```
