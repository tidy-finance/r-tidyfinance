# Constructing a factor from scratch

Between a raw download of stock returns and a published asset-pricing
result sits a long chain of decisions: how to lag accounting data so a
portfolio formed today uses only information available today, where to
draw the breakpoints that separate “high” from “low” firms, whether to
weight stocks equally or by size, and when to rebalance. These choices
rarely appear in a paper’s tables, yet they move the numbers. The goal
of `tidyfinance` is to make each choice an explicit, documented argument
rather than a buried assumption.

This vignette walks through the whole chain once, end to end. We build a
single factor — the *asset growth* factor of Cooper, Gulen, and Schill
(2008), which sorts firms on the year-over-year change in their total
assets — starting from firm-level accounting and return panels and
finishing with a long-short return series and a cross-sectional
asset-pricing test. The vignette has six steps: we download the data,
construct and lag the signal, align it with monthly returns, sort stocks
into portfolios, customize that sort, and finally run a Fama-MacBeth
regression.

## Following along without a WRDS subscription

The real asset growth factor is built from CRSP (stock returns) and
Compustat (accounting statements), both distributed through Wharton
Research Data Services (WRDS) and both requiring an institutional
subscription. So that this vignette builds anywhere — on CRAN, on a
continuous-integration runner, on a laptop with no credentials and no
network — every download below uses `domain = "Pseudo Data"` instead of
`domain = "WRDS"`.

Pseudo data mirror the *schema* of the real WRDS panels exactly: the
same column names, types, and panel structure, and a calibrated mix of
exchanges and industries. Every line of cleaning, lagging, and sorting
code therefore runs unchanged. What pseudo data deliberately do **not**
carry is economic content. Returns are independent random draws, and the
asset growth signal is unrelated to subsequent returns by construction.
**Every number in this vignette is a simulation artifact** — not just
the cross-sectional premia and slopes, but the levels too (the uniformly
positive “excess” returns reflect a built-in drift in the generator, not
a market risk premium). Treat the code as the lesson and the numbers as
placeholders; to reproduce the published premium, swap `"Pseudo Data"`
for `"WRDS"` (after a one-time
[`set_wrds_credentials()`](https://r.tidy-finance.org/reference/set_wrds_credentials.md))
and rerun.

``` r

library(tidyfinance)
library(dplyr)
library(tidyr)
library(lubridate)
```

Two internal arguments govern the simulated panels: `seed` fixes the
random draws so the document is reproducible, and `n_assets` sets the
size of the cross-section. We deliberately leave both at their defaults
(`seed = 1234` and `n_assets = 1000`) instead of passing them
explicitly. The WRDS downloads do not accept these arguments, so keeping
them out of the
[`download_data()`](https://r.tidy-finance.org/reference/download_data.md)
calls is what lets you switch the domain at the end of this vignette
without touching anything else. The only parameters we set are the
sample dates, which mirror the empirical study.

``` r

date_start <- as.Date("1972-01-01")
date_end <- as.Date("2024-12-31")
```

## Step 1: Download the data

Constructing the factor needs two panels: monthly stock returns from
CRSP and annual accounting data from Compustat.
[`download_data()`](https://r.tidy-finance.org/reference/download_data.md)
is the single entry point for both; the `domain` and `dataset` arguments
pick the source.

Setting `add_ccm_links = TRUE` appends the CRSP-Compustat linking
identifier (`gvkey`) to the return panel, which we need to merge in
accounting data later.

``` r

crsp_monthly <- download_data(
  domain = "Pseudo Data",
  dataset = "crsp_monthly",
  start_date = date_start,
  end_date = date_end,
  add_ccm_links = TRUE
)
crsp_monthly
#> # A tibble: 636,000 × 15
#>    permno date       calculation_date      ret shrout   prc primaryexch siccd
#>     <int> <date>     <date>              <dbl>  <dbl> <dbl> <chr>       <int>
#>  1      1 1972-01-01 1972-01-31       -0.0361  12887. 201.  Q            1284
#>  2      1 1972-02-01 1972-02-29       -0.0193  26252. 997.  Q            1284
#>  3      1 1972-03-01 1972-03-31       -0.0700   5872. 294.  Q            1284
#>  4      1 1972-04-01 1972-04-30        0.0314  45175. 294.  Q            1284
#>  5      1 1972-05-01 1972-05-31       -0.00748 42106. 397.  Q            1284
#>  6      1 1972-06-01 1972-06-30       -0.132   21754. 801.  Q            1284
#>  7      1 1972-07-01 1972-07-31        0.00973 27680.  92.1 Q            1284
#>  8      1 1972-08-01 1972-08-31        0.0241  37520.  34.9 Q            1284
#>  9      1 1972-09-01 1972-09-30       -0.0371  27728. 366.  Q            1284
#> 10      1 1972-10-01 1972-10-31       -0.0859  11404.  66.9 Q            1284
#> # ℹ 635,990 more rows
#> # ℹ 7 more variables: listing_age <int>, mktcap <dbl>, mktcap_lag <dbl>,
#> #   exchange <chr>, industry <chr>, ret_excess <dbl>, gvkey <chr>
```

Each row is one stock-month, already cleaned: market capitalization
(`mktcap`) and its one-month lag (`mktcap_lag`) for value weighting, a
readable `exchange` label, the SIC industry code (`siccd`), and excess
returns (`ret_excess`) net of the risk-free rate. On real WRDS data
[`download_data()`](https://r.tidy-finance.org/reference/download_data.md)
also restricts the universe to common equity, folds in delisting
returns, and maps exchange and industry codes. Here those columns are
simulated, but the exchange and industry mix is calibrated to real CRSP
frequencies and `siccd` is drawn to match each firm’s industry, so the
exchange and industry filters we apply in Step 5 still drop the intended
firms.

Next we download annual Compustat statements. The `additional_columns`
argument pulls in any raw fields beyond the defaults; we ask for total
assets (`at`) — the input to the asset growth signal — and income before
extraordinary items (`ib`), which we use later as an earnings filter.

``` r

compustat_annual <- download_data(
  domain = "Pseudo Data",
  dataset = "compustat_annual",
  start_date = date_start,
  end_date = date_end,
  additional_columns = c("at", "ib"),
  only_usd = TRUE
) |>
  select(gvkey, date, at, ib)
compustat_annual
#> # A tibble: 53,000 × 4
#>    gvkey  date          at      ib
#>    <chr>  <date>     <dbl>   <dbl>
#>  1 010001 1972-12-01 154.  -11.4  
#>  2 010001 1973-12-01 145.   15.6  
#>  3 010001 1974-12-01 117.    7.28 
#>  4 010001 1975-12-01 138.   50.9  
#>  5 010001 1976-12-01  90.0   3.79 
#>  6 010001 1977-12-01  62.0   0.647
#>  7 010001 1978-12-01  95.2  18.6  
#>  8 010001 1979-12-01 107.    5.49 
#>  9 010001 1980-12-01  92.7   8.28 
#> 10 010001 1981-12-01 138.   37.9  
#> # ℹ 52,990 more rows
```

## Step 2: Construct and lag the signal

Asset growth for firm $`i`$ in year $`t`$ is the year-over-year
percentage change in total assets,

``` math
AG_{i,t} = \frac{AT_{i,t} - AT_{i,t-1}}{AT_{i,t-1}}.
```

Computing it means lining up each firm’s total assets with its own value
one year earlier.
[`add_lagged_columns()`](https://r.tidy-finance.org/reference/add_lagged_columns.md)
does exactly that: grouped by firm (`gvkey`), it attaches each firm’s
total assets from exactly one year before (`lag = years(1)`), matching
the lagged date directly. The same function also takes a `max_lag` to
instead search a window for the most recent value, which we use in the
next step. We then take the percentage change and set non-finite results
(from a zero or missing prior value) to `NA`.

Ratios of balance-sheet items produce extreme outliers, and a handful of
firms with explosive growth would otherwise dominate summary statistics
and regression slopes. We therefore winsorize the signal once, within
each statement date, and carry the winsorized variable through the rest
of the vignette:
[`winsorize()`](https://r.tidy-finance.org/reference/winsorize.md) pulls
the most extreme 1% on each tail back to the 1st and 99th percentiles.
If you would rather drop extreme observations than clamp them, the
package also provides
[`trim()`](https://r.tidy-finance.org/reference/trim.md), which removes
everything beyond the same cutoffs. Winsorizing matters for the
Fama-MacBeth regression in Step 6, where the magnitude of the signal
enters directly, but it does not affect the portfolio sorts in Steps 4
and 5: a sort depends only on how firms rank, and winsorizing compresses
the tails without reordering them.

``` r

sorting_variable_data <- compustat_annual |>
  add_lagged_columns(
    cols = "at",
    lag = years(1),
    by = "gvkey"
  ) |>
  mutate(
    asset_growth = (at - at_lag) / at_lag,
    asset_growth = if_else(is.finite(asset_growth), asset_growth, NA_real_)
  ) |>
  group_by(date) |>
  mutate(asset_growth = winsorize(asset_growth, cut = 0.01)) |>
  ungroup() |>
  select(gvkey, date, asset_growth, ib)
```

Before using the signal, it pays to look at it.
[`create_summary_statistics()`](https://r.tidy-finance.org/reference/create_summary_statistics.md)
returns the standard cross-sectional summary for one or more variables.

``` r

sorting_variable_data |>
  drop_na(asset_growth) |>
  create_summary_statistics(asset_growth, detail = TRUE) |>
  select(variable, n, mean, sd, q05, q50, q95) |>
  knitr::kable(
    digits = 3,
    caption = "Cross-sectional distribution of asset growth."
  )
```

| variable     |     n |  mean |    sd |    q05 |   q50 |  q95 |
|:-------------|------:|------:|------:|-------:|------:|-----:|
| asset_growth | 52000 | 0.098 | 0.328 | -0.356 | 0.051 | 0.73 |

Cross-sectional distribution of asset growth. {.table}

Even after winsorizing, the mean sits above the median: asset growth is
right-skewed, with a tail of fast-expanding firms pulling the average
up. That skew is a property of the constructed variable and so survives
in pseudo data — unlike any relationship between the signal and future
returns, which does not.

## Step 3: Align the signal with returns

The signal is annual; returns are monthly. To form portfolios we need
each stock-month paired with the *most recently observable* value of
asset growth — using only what an investor could have known at the
formation date. This is where look-ahead bias (slipping in information
that was not yet public) creeps in if you are not careful: the
accounting for fiscal year $`t`$ is not public the instant the year
ends.

[`join_lagged_values()`](https://r.tidy-finance.org/reference/join_lagged_values.md)
merges the annual signal onto the monthly panel through a configurable
lag window. Setting `ff_adjustment = TRUE` applies the Fama and French
(1993) timing convention: accounting data are assumed public roughly six
months after the fiscal year-end and held for the following twelve
months. Because the pseudo statements all carry a December fiscal
year-end, the 7-to-18-month window places each signal in the
July-to-June holding period that the convention prescribes. The window
is anchored on the statement date, so for real firms with other fiscal
year-ends the exact calendar months shift accordingly.

``` r

sorting_data <- crsp_monthly |>
  join_lagged_values(
    new_data = sorting_variable_data,
    id_keys = "gvkey",
    min_lag = months(7),
    max_lag = months(18),
    ff_adjustment = TRUE
  ) |>
  select(
    date,
    permno,
    exchange,
    siccd,
    ret_excess,
    mktcap_lag,
    asset_growth,
    ib
  ) |>
  filter(date >= date_start + months(12) + months(18))
```

The final
[`filter()`](https://dplyr.tidyverse.org/reference/filter.html) drops
the start of the sample, where the one-year growth calculation and the
lag window leave the signal mechanically missing. What remains is a
clean stock-month panel carrying excess returns, a value weight, and a
properly lagged signal.

## Step 4: Sort stocks into portfolios

A portfolio sort ranks stocks each period by the signal and splits them
into ordered groups; the spread between the extreme groups is the factor
return.
[`implement_portfolio_sort()`](https://r.tidy-finance.org/reference/implement_portfolio_sort.md)
runs the whole sort in one call. It takes the panel, the sorting
variable, the sorting method, and a `portfolio_sort_options` object that
bundles how breakpoints are formed.

For the baseline we sort into quintiles, computing breakpoints from
NYSE-listed stocks only — a standard convention that keeps the many
small NASDAQ firms from dominating the cutoffs — and rebalance every
July to match the Fama-French timing we used when lagging the signal.

``` r

options_baseline <- portfolio_sort_options(
  breakpoint_options_main = breakpoint_options(
    n_portfolios = 5,
    breakpoints_exchanges = "NYSE"
  )
)

portfolio_returns <- implement_portfolio_sort(
  data = sorting_data,
  sorting_variables = "asset_growth",
  sorting_method = "univariate",
  rebalancing_month = 7,
  portfolio_sort_options = options_baseline
)
portfolio_returns
#> # A tibble: 3,030 × 5
#>    portfolio date       ret_excess_vw ret_excess_ew ret_excess_vw_capped
#>        <int> <date>             <dbl>         <dbl>                <dbl>
#>  1         1 1974-07-01       0.00440      0.00268              0.00329 
#>  2         1 1974-08-01       0.0144       0.0155               0.0183  
#>  3         1 1974-09-01       0.0127       0.0174               0.0122  
#>  4         1 1974-10-01       0.00393     -0.00804             -0.000116
#>  5         1 1974-11-01       0.00896      0.00957              0.0118  
#>  6         1 1974-12-01       0.00973      0.00277              0.00876 
#>  7         1 1975-01-01       0.00745      0.0136               0.00862 
#>  8         1 1975-02-01       0.00381     -0.000778             0.00609 
#>  9         1 1975-03-01       0.00760      0.0111               0.00828 
#> 10         1 1975-04-01      -0.00240     -0.00264             -0.00322 
#> # ℹ 3,020 more rows
```

This baseline call named only two things; everything else came from a
documented default. That is the point: a default call reproduces
standard practice, and every construction choice that papers usually
leave implicit — an 80th-percentile cap for the capped value-weighted
return, no minimum portfolio size, the standard CRSP/Compustat column
names — is here an argument you can see and change. The output reports
three weighting schemes per portfolio-month: value-weighted
(`ret_excess_vw`), equal-weighted (`ret_excess_ew`), and capped
value-weighted (`ret_excess_vw_capped`).

The factor itself is the long-short spread.
[`compute_long_short_returns()`](https://r.tidy-finance.org/reference/compute_long_short_returns.md)
takes the difference between the extreme portfolios;
`direction = "bottom_minus_top"` goes long low-asset-growth firms and
short high-asset-growth firms, the sign of the documented premium.

``` r

factor_returns <- portfolio_returns |>
  compute_long_short_returns(direction = "bottom_minus_top")
```

``` r

portfolio_summary <- bind_rows(
  portfolio_returns |>
    mutate(portfolio = as.character(portfolio)),
  factor_returns |>
    mutate(portfolio = "Long-short")
) |>
  group_by(portfolio) |>
  summarize(
    mean_vw = mean(ret_excess_vw, na.rm = TRUE) * 12,
    mean_ew = mean(ret_excess_ew, na.rm = TRUE) * 12,
    .groups = "drop"
  )

portfolio_summary |>
  knitr::kable(
    digits = 4,
    col.names = c("Portfolio", "Value-weighted", "Equal-weighted"),
    caption = paste(
      "Annualized mean excess returns by asset growth quintile, plus the",
      "long-short spread. On real data the value-weighted mean declines",
      "across quintiles and the long-short row is a sizable premium; here",
      "the quintile ordering and the long-short value (and its sign) are",
      "sampling noise."
    )
  )
```

| Portfolio  | Value-weighted | Equal-weighted |
|:-----------|---------------:|---------------:|
| 1          |         0.0729 |         0.0718 |
| 2          |         0.0752 |         0.0755 |
| 3          |         0.0718 |         0.0717 |
| 4          |         0.0756 |         0.0764 |
| 5          |         0.0733 |         0.0734 |
| Long-short |        -0.0004 |        -0.0016 |

Annualized mean excess returns by asset growth quintile, plus the
long-short spread. On real data the value-weighted mean declines across
quintiles and the long-short row is a sizable premium; here the quintile
ordering and the long-short value (and its sign) are sampling noise.
{.table}

On real WRDS data the value-weighted means decline monotonically from
the low- to the high-asset-growth quintile, and the long-short spread
reproduces the negative premium of Cooper, Gulen, and Schill (2008). On
the pseudo panel there is no systematic decline: the differences across
quintiles, any apparent ordering, and the long-short spread (whatever
its sign on a given run) are all sampling noise, as they must be.

## Step 5: Customize the sort

The baseline took nearly every default. A realistic specification
overrides several at once, and `portfolio_sort_options` keeps them all
in one place. Here we exclude financial and utility firms and firms with
non-positive earnings, ignore the smallest 20% of NYSE firms when
setting breakpoints, and add a second sort on size: a *dependent
bivariate* sort that controls for size by first bucketing on it, then
sorting on asset growth within each bucket.

``` r

options_customized <- portfolio_sort_options(
  filter_options = filter_options(
    exclude_financials = TRUE,
    exclude_utilities = TRUE,
    exclude_negative_earnings = TRUE
  ),
  breakpoint_options_main = breakpoint_options(
    n_portfolios = 5,
    breakpoints_exchanges = "NYSE",
    breakpoints_min_size_threshold = 0.2
  ),
  breakpoint_options_secondary = breakpoint_options(
    n_portfolios = 2,
    breakpoints_exchanges = "NYSE"
  )
)

factor_returns_customized <- implement_portfolio_sort(
  data = sorting_data,
  sorting_variables = c("asset_growth", "mktcap_lag"),
  sorting_method = "bivariate-dependent",
  rebalancing_month = 7,
  portfolio_sort_options = options_customized,
  min_portfolio_size = 10,
  quiet = TRUE
) |>
  compute_long_short_returns(direction = "bottom_minus_top")
```

Moving from a univariate to a size-controlled bivariate sort was a
one-word change to `sorting_method`; the surrounding pipeline is
identical. That is the point of exposing construction choices as
arguments. The lower-level building blocks
([`compute_breakpoints()`](https://r.tidy-finance.org/reference/compute_breakpoints.md),
[`assign_portfolio()`](https://r.tidy-finance.org/reference/assign_portfolio.md),
[`compute_portfolio_returns()`](https://r.tidy-finance.org/reference/compute_portfolio_returns.md))
remain available when you need finer control over one stage.

## Step 6: Run an asset-pricing test

A long-short return is one verdict on whether a signal predicts the
cross-section of returns. A Fama-MacBeth regression (Fama and MacBeth
1973) gives a complementary one: it runs a cross-sectional regression of
returns on the signal period by period, then averages the slopes into a
risk premium with Newey-West standard errors (Newey and West 1987).
[`estimate_fama_macbeth()`](https://r.tidy-finance.org/reference/estimate_fama_macbeth.md)
does this from a panel and a model formula.

The signal is already winsorized from Step 2, so we only add log market
capitalization as a size control.

``` r

fm_data <- sorting_data |>
  drop_na(asset_growth, mktcap_lag, ret_excess) |>
  mutate(log_mktcap = log(mktcap_lag))

fm_results <- estimate_fama_macbeth(
  data = fm_data,
  model = "ret_excess ~ asset_growth + log_mktcap"
)

fm_results |>
  knitr::kable(
    digits = 4,
    caption = paste(
      "Fama-MacBeth regression of monthly excess returns on lagged asset",
      "growth and log market cap. On real data the asset growth slope is",
      "negative and significant; on pseudo data it is indistinguishable",
      "from zero. The large, 'significant' intercept is a mechanical",
      "artifact of the simulated returns' built-in positive drift, not an",
      "alpha."
    )
  )
```

| factor       | risk_premium |   n | standard_error | t_statistic |
|:-------------|-------------:|----:|---------------:|------------:|
| intercept    |       0.0068 | 606 |          1e-03 |      6.5564 |
| asset_growth |       0.0001 | 606 |          4e-04 |      0.1481 |
| log_mktcap   |      -0.0001 | 606 |          1e-04 |     -0.6700 |

Fama-MacBeth regression of monthly excess returns on lagged asset growth
and log market cap. On real data the asset growth slope is negative and
significant; on pseudo data it is indistinguishable from zero. The
large, ‘significant’ intercept is a mechanical artifact of the simulated
returns’ built-in positive drift, not an alpha. {.table}

On real data the slope on `asset_growth` is negative and significant,
matching the portfolio-sort evidence: high-asset-growth firms earn lower
subsequent returns. On the pseudo panel the slope is statistical noise.
The intercept row carries a large $`t`$-statistic here, but that
reflects only the generator’s built-in positive return drift, not a real
average return. Switching the standard-error treatment (`vcov = "iid"`)
or adding a regressor is a one-argument change.

## Where to go next

You have now run the complete `tidyfinance` workflow: download,
construct and lag the signal, align it with returns, sort, customize,
and test. Every stage exposed its decisions as arguments with sensible
defaults, so the baseline reproduces standard practice and any variation
is a small edit.

To reproduce the *published* asset growth premium, replace
`"Pseudo Data"` with `"WRDS"` in the two
[`download_data()`](https://r.tidy-finance.org/reference/download_data.md)
calls and run
[`set_wrds_credentials()`](https://r.tidy-finance.org/reference/set_wrds_credentials.md)
once beforehand; nothing else in the pipeline changes. For the
underlying methods and the empirical results on real data, see the book
by [Scheuch, Voigt, and Weiss (2023)](https://www.tidy-finance.org), and
consult
[`?download_data`](https://r.tidy-finance.org/reference/download_data.md),
[`?implement_portfolio_sort`](https://r.tidy-finance.org/reference/implement_portfolio_sort.md),
and
[`?estimate_fama_macbeth`](https://r.tidy-finance.org/reference/estimate_fama_macbeth.md)
for the full set of options at each stage.

## References

Cooper, Michael J., Huseyin Gulen, and Michael J. Schill. 2008. “Asset
Growth and the Cross-Section of Stock Returns.” *The Journal of Finance*
63 (4): 1609–1651.

Fama, Eugene F., and Kenneth R. French. 1993. “Common Risk Factors in
the Returns on Stocks and Bonds.” *Journal of Financial Economics* 33
(1): 3–56.

Fama, Eugene F., and James D. MacBeth. 1973. “Risk, Return, and
Equilibrium: Empirical Tests.” *Journal of Political Economy* 81 (3):
607–636.

Newey, Whitney K., and Kenneth D. West. 1987. “A Simple, Positive
Semi-Definite, Heteroskedasticity and Autocorrelation Consistent
Covariance Matrix.” *Econometrica* 55 (3): 703–708.

Scheuch, Christoph, Stefan Voigt, and Patrick Weiss. 2023. *Tidy Finance
with R*. Boca Raton, FL: Chapman & Hall/CRC.
<https://www.tidy-finance.org>.
