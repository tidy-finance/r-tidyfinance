# Changelog

## tidyfinance (development version)

## tidyfinance 1.0.0

CRAN release: 2026-10-26

### Breaking changes

- [`download_data_fred()`](https://r.tidy-finance.org/reference/download_data_fred.md)
  and `download_data("FRED", ...)` now return the columns `date`,
  `series`, `value` (identifiers first), as py-tidyfinance does, instead
  of `date`, `value`, `series`
  ([\#346](https://github.com/tidy-finance/r-tidyfinance/issues/346)).
- [`compute_rolling_value()`](https://r.tidy-finance.org/reference/compute_rolling_value.md)
  now returns one value per row of `data`, as documented: every row gets
  the value of its period’s window. Before, it returned one value per
  period, so it could not be used in
  [`mutate()`](https://dplyr.tidyverse.org/reference/mutate.html) when a
  period held more than one row, e.g. with `period = "quarter"` on
  monthly data
  ([\#348](https://github.com/tidy-finance/r-tidyfinance/issues/348)).
- `download_data("Goyal-Welch", ...)` now computes `rp_div` as the
  future log market return minus the log risk-free rate,
  `log(1 + Rfree)`, as in the book. Before, it subtracted the simple
  risk-free rate
  ([\#350](https://github.com/tidy-finance/r-tidyfinance/issues/350)).
- The capped value-weighted returns of
  [`compute_portfolio_returns()`](https://r.tidy-finance.org/reference/compute_portfolio_returns.md)
  and
  [`implement_portfolio_sort()`](https://r.tidy-finance.org/reference/implement_portfolio_sort.md)
  (`ret_excess_vw_capped`) now cap market capitalization at the
  `cap_weight` percentile of NYSE stocks on each date, following Jensen,
  Kelly, and Pedersen (2023), instead of the percentile of all stocks.
  The new `cap_weight_exchanges` argument selects the exchanges;
  `cap_weight_exchanges = NULL` restores the previous behaviour. Without
  an exchange column, or on dates without NYSE stocks, the cap falls
  back to all stocks with a message
  ([\#319](https://github.com/tidy-finance/r-tidyfinance/issues/319)).
- In dependent bivariate sorts, `breakpoints_min_size_threshold` of the
  main breakpoints is now applied with one minimum market cap per date,
  computed from the full cross-section, instead of a separate one within
  each secondary portfolio. Before, e.g., excluding the smallest 20% of
  NYSE stocks from the asset-growth breakpoints within size halves
  excluded the smallest 20% of each half
  ([\#319](https://github.com/tidy-finance/r-tidyfinance/issues/319)).
- With annual rebalancing (`rebalancing_month`),
  [`implement_portfolio_sort()`](https://r.tidy-finance.org/reference/implement_portfolio_sort.md)
  and
  [`filter_sorting_data()`](https://r.tidy-finance.org/reference/filter_sorting_data.md)
  now apply the screens of
  [`filter_options()`](https://r.tidy-finance.org/reference/filter_options.md)
  in the formation month only and keep every stock that passes them for
  the whole holding period (ex-ante screens). Before, the screens were
  applied in every month, so a stock that, e.g., fell below the price
  screen after formation lost its holding-month returns, which can bias
  factor returns. The previous behaviour is available with
  `filter_options(timing = "every_period")` and issues a warning.
  [`filter_sorting_data()`](https://r.tidy-finance.org/reference/filter_sorting_data.md)
  gains a `rebalancing_month` argument
  ([\#321](https://github.com/tidy-finance/r-tidyfinance/issues/321)).
- The default price column of
  [`data_options()`](https://r.tidy-finance.org/reference/data_options.md)
  is now `prc_lag`, the nominal price at the end of the previous month,
  instead of `prc_adj`, which the default CRSP v2 download does not
  return, so the `min_stock_price` screen failed on downloaded data.
  `download_data("WRDS", "crsp_monthly")` (v1 and v2) and the pseudo
  CRSP data now return `prc_lag`
  ([\#321](https://github.com/tidy-finance/r-tidyfinance/issues/321)).
- [`list_supported_types()`](https://r.tidy-finance.org/reference/list_supported_datasets.md)
  is now deprecated instead of soft-deprecated, so it warns on every
  call, not only when called from the global environment. The
  documentation of the machine-readable domain names of
  [`download_data()`](https://r.tidy-finance.org/reference/download_data.md)
  (e.g. `"famafrench"`, `"wrds"`) now calls them deprecated, as they
  already warned.

### Bug fixes

- `download_data("Fama-French", ...)` now parses all eight breakpoints
  datasets. Before, all but `ME Breakpoints` returned an empty tibble,
  and `ME Breakpoints` had the generic column names `v2`-`v22`. The
  columns are now named `date`, `count` (or `<=0` and `>0`), and the
  percentile bins `0-5`, …, `95-100`, as in py-tidyfinance. Annual
  breakpoints are dated at the end of the year
  ([\#347](https://github.com/tidy-finance/r-tidyfinance/issues/347)).
- [`list_tidy_finance_chapters()`](https://r.tidy-finance.org/reference/list_tidy_finance_chapters.md)
  and
  [`open_tidy_finance_website()`](https://r.tidy-finance.org/reference/open_tidy_finance_website.md)
  now follow the new website, which serves the chapters at
  `https://www.tidy-finance.org/chapters/`
  ([\#349](https://github.com/tidy-finance/r-tidyfinance/issues/349)).
- [`compute_portfolio_returns()`](https://r.tidy-finance.org/reference/compute_portfolio_returns.md)
  and
  [`implement_portfolio_sort()`](https://r.tidy-finance.org/reference/implement_portfolio_sort.md)
  now return the complete panel ordered by portfolio and date. Before,
  dates were ordered by their first appearance in `data`, so the row
  order depended on how the input was sorted
  ([\#344](https://github.com/tidy-finance/r-tidyfinance/issues/344)).
- [`add_lagged_columns()`](https://r.tidy-finance.org/reference/add_lagged_columns.md)
  now aborts when a `<col>_lag` column already exists in `data`. Before,
  it warned that it overwrites the column but returned `.x`/`.y` columns
  (exact lag) or failed with a cryptic error (lag window)
  ([\#342](https://github.com/tidy-finance/r-tidyfinance/issues/342)).
- `estimate_model(output = "tstats")` now names the t-statistic column
  `intercept` for an intercept-only model, instead of leaving it
  unnamed, so `$intercept` works as it does for models with regressors.
- [`estimate_model()`](https://r.tidy-finance.org/reference/estimate_model.md)
  no longer fails for a single-column data frame (for example
  `estimate_model(data.frame(y = 1:5), "y ~ 1")`); row subsetting
  previously dropped the one column to a vector before
  [`lm()`](https://rdrr.io/r/stats/lm.html).
- [`compute_long_short_returns()`](https://r.tidy-finance.org/reference/compute_long_short_returns.md)
  now reads the portfolio return columns (`ret_excess_vw`,
  `ret_excess_ew`, `ret_excess_vw_capped`) that
  [`compute_portfolio_returns()`](https://r.tidy-finance.org/reference/compute_portfolio_returns.md)
  produces, so the same custom `data_options` (for example a renamed
  `ret_excess` column) can be passed to both functions; previously
  [`compute_long_short_returns()`](https://r.tidy-finance.org/reference/compute_long_short_returns.md)
  failed with a cryptic tidyr error. Data without any of these columns
  now raises an informative error.
- [`compute_long_short_returns()`](https://r.tidy-finance.org/reference/compute_long_short_returns.md)
  now validates `direction`, which must be `"top_minus_bottom"` or
  `"bottom_minus_top"`; a misspelled value previously defaulted silently
  to top minus bottom.
- [`filter_options()`](https://r.tidy-finance.org/reference/filter_options.md),
  [`breakpoint_options()`](https://r.tidy-finance.org/reference/breakpoint_options.md),
  and
  [`data_options()`](https://r.tidy-finance.org/reference/data_options.md)
  now warn when an argument passed through `...` is within one edit of a
  known option name (for example `exclude_finacials`), which usually
  signals a typo; the argument is still kept, so genuine custom options
  keep working.
- The messages of
  [`compute_portfolio_returns()`](https://r.tidy-finance.org/reference/compute_portfolio_returns.md)
  for an empty panel and for missing portfolio returns now name the
  actual cause (a missing sorting value, no assigned stocks, a
  cross-section below `min_portfolio_size`, or a missing constituent
  return) instead of always citing “insufficient observations”.
- The help page of
  [`trim()`](https://r.tidy-finance.org/reference/trim.md) now states
  that the function sets the extreme values to `NA` and keeps the length
  of `x`; it previously said the vector gets shorter.
- The help pages of
  [`add_lagged_columns()`](https://r.tidy-finance.org/reference/add_lagged_columns.md)
  and
  [`join_lagged_values()`](https://r.tidy-finance.org/reference/join_lagged_values.md)
  now state that month- and year-based lags require first-of-month dates
  (as returned by
  [`download_data()`](https://r.tidy-finance.org/reference/download_data.md))
  and how to floor other dates.
- [`download_data_wrds_crsp()`](https://r.tidy-finance.org/reference/download_data_wrds_crsp.md)
  with `version = "v2"` (the default) now keeps the month and the day
  that carry a stock’s delisting return
  ([\#320](https://github.com/tidy-finance/r-tidyfinance/issues/320)).
  CIZ includes the delisting return in `mthret` and `dlyret`, but
  `stksecurityinfohist` holds no active record after a stock’s last
  trading day, so the common-stock filters were evaluated at month end
  (or on the delisting day) and dropped these observations. Their
  security information is now looked up on the last trading day
  (`securityenddt`). On CRSP data for 1960-2025, the fix restores 19,858
  stock-months with an average return of -6.9%, and the share of
  delistings with a delisting return whose month is in the data rises
  from 13% to over 99%.
- `ff_adjustment = TRUE` in
  [`join_lagged_values()`](https://r.tidy-finance.org/reference/join_lagged_values.md)
  and
  [`add_lagged_columns()`](https://r.tidy-finance.org/reference/add_lagged_columns.md)
  now implements the Fama and French (1993) timing: the value of a
  fiscal year ending in calendar year t is used from July of t + 1
  through June of t + 2, for every fiscal year-end. Before, it kept the
  last observation per year and then applied the `min_lag`/`max_lag`
  window from the fiscal year-end, so a firm with a June fiscal year-end
  updated in January instead of July. The lag arguments are now optional
  and ignored with `ff_adjustment = TRUE`
  ([\#323](https://github.com/tidy-finance/r-tidyfinance/issues/323)).
- `add_lagged_columns(ff_adjustment = TRUE)` no longer fails when `by`
  is set.
- [`get_wrds_connection()`](https://r.tidy-finance.org/reference/get_wrds_connection.md),
  and thus every WRDS download, now explains a failed connection: it
  lists the likely causes (wrong credentials, missing two-factor
  authentication, terms of use not accepted) and keeps the original
  error as the cause
  ([\#325](https://github.com/tidy-finance/r-tidyfinance/issues/325)).
- [`estimate_fama_macbeth()`](https://r.tidy-finance.org/reference/estimate_fama_macbeth.md)
  with `vcov = "iid"` no longer multiplies the t-statistic by the square
  root of the number of periods a second time. The standard error of the
  mean already accounts for it, so the t-statistics were inflated by a
  factor of sqrt(T)
  ([\#322](https://github.com/tidy-finance/r-tidyfinance/issues/322)).
- [`download_data_osap()`](https://r.tidy-finance.org/reference/download_data_osap.md)
  and `download_data("Open Source Asset Pricing")` no longer parse the
  HTML page that Google Drive returns instead of the data, e.g., when
  the download quota of the file is exceeded. They now return an empty
  tibble and say why, instead of a tibble whose column names are
  fragments of the page.
- [`download_data_osap()`](https://r.tidy-finance.org/reference/download_data_osap.md)
  downloads the October 2025 release (v2.0.0) of the long-short returns
  by default instead of the August 2024 release (v1.4.0).

### New features

- `download_data("Tidy Finance", ...)` gains two datasets of the factor
  library. `"factor_library_costs"` downloads the turnover, the trading
  cost, and the microcap share of each portfolio from January 1994, with
  the same filters and `ids` as `"factor_library"` and the grid joined
  in the same way. `"factor_library_persistence"` downloads how up to
  date each sorting variable is under each lag and rebalancing
  frequency, optionally for some `sorting_variable`s only.
- The factor library downloads can be pinned to a release. `revision`
  selects the commit of the Hugging Face dataset
  `tidy-finance/factor-library`, which holds the returns and the costs,
  and `grid_revision` the commit of `tidy-finance/factor-library-grid`,
  which holds the grid and the persistence. Both default to `"main"`.
  [`download_factor_library_ids()`](https://r.tidy-finance.org/reference/download_factor_library_ids.md)
  and
  [`download_factor_library_grid()`](https://r.tidy-finance.org/reference/download_factor_library_grid.md)
  gain the same arguments, and the grid is cached once per revision.
  `download_data("Tidy Finance", "factor_library_grid")` now also passes
  `refresh` on.
- The factor library downloads up to 16 files at once and 8 per second
  on average, also across calls, which keeps it below the limit of
  Hugging Face for anonymous users of 3,000 files per five minutes. When
  Hugging Face still refuses a file with HTTP 429, the download waits
  until the limit resets and tries again, twice at most, before it fails
  with a message that explains the limit. A revision that the dataset
  does not have now fails with a clear message. httr2 1.1.1 or later is
  required.
- [`compute_portfolio_returns()`](https://r.tidy-finance.org/reference/compute_portfolio_returns.md)
  and
  [`implement_portfolio_sort()`](https://r.tidy-finance.org/reference/implement_portfolio_sort.md)
  gain `holding_period` and `skip` for staggered rebalancing
  ([\#324](https://github.com/tidy-finance/r-tidyfinance/issues/324)):
  portfolios are formed every period, each cohort is held for
  `holding_period` periods starting `skip` periods after formation, and
  a portfolio’s return is the equal-weighted average of the overlapping
  cohorts held in a period, as in Jegadeesh and Titman (1993). Held
  stocks stay in their cohort for the whole holding period, and in
  [`implement_portfolio_sort()`](https://r.tidy-finance.org/reference/implement_portfolio_sort.md)
  the screens of
  [`filter_options()`](https://r.tidy-finance.org/reference/filter_options.md)
  only decide which stocks enter a new cohort. The vignette shows a
  six-month momentum strategy.
- WRDS downloads report their progress
  ([\#326](https://github.com/tidy-finance/r-tidyfinance/issues/326)).
  [`get_wrds_connection()`](https://r.tidy-finance.org/reference/get_wrds_connection.md)
  says when it connects (with a reminder to approve the Duo request) and
  when the connection succeeds. CRSP monthly, Compustat, CCM links,
  FISD, and TRACE downloads show a step that ends with the elapsed time;
  daily CRSP keeps its batch progress bar.
  [`get_wrds_connection()`](https://r.tidy-finance.org/reference/get_wrds_connection.md)
  and all `download_data_wrds_*()` functions gain `quiet = FALSE` to
  suppress these messages, which can also be passed through
  [`download_data()`](https://r.tidy-finance.org/reference/download_data.md).
  `download_data("Pseudo Data", ...)` accepts `quiet` too and then omits
  its notice, so the same call works for both domains.
- The help pages of
  [`get_wrds_connection()`](https://r.tidy-finance.org/reference/get_wrds_connection.md)
  and
  [`set_wrds_credentials()`](https://r.tidy-finance.org/reference/set_wrds_credentials.md),
  the README, and the vignette document what a WRDS account needs before
  the first connection: two-factor authentication (Duo) and acceptance
  of the WRDS terms of use
  ([\#325](https://github.com/tidy-finance/r-tidyfinance/issues/325)).
- Added
  [`download_data_damodaran()`](https://r.tidy-finance.org/reference/download_data_damodaran.md)
  and the `"Damodaran"` domain for
  [`download_data()`](https://r.tidy-finance.org/reference/download_data.md),
  which download the cost of capital (WACC) by industry from Aswath
  Damodaran’s data library, e.g. `download_data("Damodaran", "wacc")`.
  `region` selects the US (default), Europe, Japan, emerging markets,
  global, China, India, or Australia, New Zealand, and Canada, and
  `year` reads archived years (from 1998 for the US) into a panel.
  Reading the Excel files requires the suggested readxl package
  ([\#298](https://github.com/tidy-finance/r-tidyfinance/issues/298)).

## tidyfinance 0.9.0

CRAN release: 2026-10-05

### Breaking changes

- [`download_factor_library_ids()`](https://r.tidy-finance.org/reference/download_factor_library_ids.md)
  and `download_data("Tidy Finance", "factor_library")` read the new
  layout of the factor library on Hugging Face, where the returns hold
  only `id`, `date`, and `ret` in files of 1,000 consecutive IDs named
  after their range. Only the files that hold the requested IDs are
  downloaded, and the file listing of the dataset is no longer queried.
  The result no longer has a `ret_type` column; the weighting scheme is
  in the `weighting_scheme` column of the grid. Earlier versions of the
  package cannot read the new layout.
- The factor library now builds on the signals of Open Source Asset
  Pricing, so sorting variables carry their names, e.g. `"size"` instead
  of `"me"` and `"high52"` instead of `"52w"`. The examples and the
  documented grid values follow the new release, which adds the `"1m"`
  lag and `"capped VW"` weighting.

### New features

- [`download_data_constituents()`](https://r.tidy-finance.org/reference/download_data_constituents.md)
  gains a `path` argument that reads an iShares or BlackRock holdings
  CSV saved from the fund’s web page instead of downloading it,
  e.g. `download_data("Index Constituents", path = "holdings.csv")`.
  `index` is optional with `path`. Use it as a fallback when the
  provider moves its files
  ([\#311](https://github.com/tidy-finance/r-tidyfinance/issues/311)).
- Added
  [`download_data_fred_md()`](https://r.tidy-finance.org/reference/download_data_fred_md.md)
  and the `"FRED-MD"` / `"FRED-QD"` datasets for
  `download_data("FRED", ...)`, which download the McCracken and Ng
  (2016, 2021) curated monthly / quarterly macro panels as wide tables
  (one column per series). `transform = TRUE` applies each series’
  stationarity transform code (tcode). `vintage` selects the current
  release (default), a specific `"YYYY-MM"` release, or `"all"` - the
  full real-time panel across every archived vintage (recent vintages
  are hosted individually; older ones are read from the St. Louis Fed
  vintage archive ZIPs), enabling leak-free point-in-time analysis.

### Bug fixes

- [`download_data_constituents()`](https://r.tidy-finance.org/reference/download_data_constituents.md)
  works again for DAX, EURO STOXX 50, Dow Jones Industrial Average, S&P
  500, Nasdaq 100, FTSE 100, MSCI World, STOXX Europe 600, MDAX, TecDAX,
  MSCI Emerging Markets, and MSCI Europe. iShares removed the German
  retail pages the holdings files were downloaded from; they are now
  read from the Swiss professional pages. The header row of a holdings
  file is detected automatically instead of skipping a fixed number of
  rows, and a failed download now names the index and the URL and points
  to `path`
  ([\#311](https://github.com/tidy-finance/r-tidyfinance/issues/311)).
- [`download_factor_library_grid()`](https://r.tidy-finance.org/reference/download_factor_library_grid.md)
  caches the grid of the factor library for the rest of the session, so
  [`download_factor_library_ids()`](https://r.tidy-finance.org/reference/download_factor_library_ids.md)
  and `download_data("Tidy Finance", "factor_library", ...)` no longer
  download the grid of more than four million rows on every call
  (previously twice per
  [`download_data()`](https://r.tidy-finance.org/reference/download_data.md)
  call when filtering). Use `refresh = TRUE` to download it again.
- Passing a legacy identifier such as `"factors_ff_3_monthly"` as
  `dataset` now warns that this value is deprecated and names the
  dataset that replaces it, e.g. `"Fama/French 3 Factors"`. The warning
  previously claimed that the `type` argument was used, although the
  caller never passed it.
- [`estimate_fama_macbeth()`](https://r.tidy-finance.org/reference/estimate_fama_macbeth.md)
  now orders the cross-sections chronologically before aggregating them
  over time. Newey-West standard errors depend on the order of the time
  series, so the same data in a different row order previously returned
  different standard errors and t-statistics while the risk premia were
  unchanged. Results for chronologically sorted input are unaffected
  ([\#302](https://github.com/tidy-finance/r-tidyfinance/issues/302)).
- The documentation of `estimate_fama_macbeth(detail = TRUE)` now lists
  the `adj_r_squared` column, which the function has always returned in
  `summary_statistics` alongside `r_squared` and `n_obs`.
- [`download_data_stock_prices()`](https://r.tidy-finance.org/reference/download_data_stock_prices.md)
  now derives dates from the exchange time zone reported by Yahoo
  Finance instead of converting the timestamps in UTC. Timestamps refer
  to the market open in local time, so for exchanges ahead of UTC
  (e.g. `"^AXJO"`, `"^NZ50"`) every observation was previously dated one
  calendar day too early and could fall on a weekend.
- [`download_data_stock_prices()`](https://r.tidy-finance.org/reference/download_data_stock_prices.md)
  now returns a range that is inclusive of both `start_date` and
  `end_date` for every exchange, matching the other `download_data_*()`
  functions. The range was previously delegated to Yahoo Finance’s
  `period1` / `period2`, which are resolved in the exchange’s local time
  zone, so `end_date` was excluded for markets at or behind UTC but
  included for markets ahead of it.
- [`download_factor_library_grid()`](https://r.tidy-finance.org/reference/download_factor_library_grid.md),
  and with it `download_data("Tidy Finance", "factor_library")`, now
  reads `portfolio_sort_grid.parquet` by name instead of every Parquet
  file listed in the grid repo, which also holds one slice of the grid
  per sorting variable and the list of sorting variables. Reading the
  listing failed as soon as the repo held more than one Parquet file.
- [`download_factor_library_ids()`](https://r.tidy-finance.org/reference/download_factor_library_ids.md),
  and with it `download_data("Tidy Finance", "factor_library")`, no
  longer fails with “HTTP 404 Not Found” for IDs in a range of 1,000 IDs
  for which the factor library has no file because none of its portfolio
  sorts produced portfolios (e.g., IDs 833001 to 835000, which hold
  `"rdcap"` sorts with `min_size_quantile = 0.2`). Requested IDs without
  returns are absent from the result, as documented, and a warning now
  lists them; if none of the requested IDs has returns, the result is an
  empty tibble with all columns.

## tidyfinance 0.8.0

CRAN release: 2026-07-02

### New features

- Added
  [`download_data_pastor_stambaugh()`](https://r.tidy-finance.org/reference/download_data_pastor_stambaugh.md)
  and the `"Pastor-Stambaugh"` domain for
  [`download_data()`](https://r.tidy-finance.org/reference/download_data.md),
  which downloads the liquidity factors of Pastor and Stambaugh (2003)
  from [Robert Stambaugh’s data
  library](https://fnce.wharton.upenn.edu/profile/stambaug/#misc). The
  result carries the levels of aggregate liquidity, the non-traded
  liquidity factor (innovations), and the traded liquidity factor
  `LIQ_V`.
- Added
  [`download_data_stambaugh_yuan()`](https://r.tidy-finance.org/reference/download_data_stambaugh_yuan.md)
  and the `"Stambaugh-Yuan"` domain for
  [`download_data()`](https://r.tidy-finance.org/reference/download_data.md),
  which downloads the mispricing factors (`mgmt` and `perf`) of
  Stambaugh and Yuan (2017) from [Robert Stambaugh’s data
  library](https://fnce.wharton.upenn.edu/profile/stambaug/#misc). The
  `dataset` argument selects between `"monthly"` and `"daily"` data. The
  source files currently end in December 2016.
- Added
  [`download_data_jkp()`](https://r.tidy-finance.org/reference/download_data_jkp.md)
  and the `"Global Factor Data"` domain for
  [`download_data()`](https://r.tidy-finance.org/reference/download_data.md),
  which downloads data from [Global Factor
  Data](https://jkpfactors.com/data) (Jensen, Kelly, and Pedersen,
  2023). The `dataset` argument selects between factor returns
  (`"factors"`), the underlying long-short portfolios (`"portfolios"`),
  industry returns (`"industry"`), and the reference files
  `"nyse_cutoffs"` and `"return_cutoffs"`. The requested selection is
  validated against the library’s live availability manifest, and the
  helper
  [`list_supported_jkp_factors()`](https://r.tidy-finance.org/reference/list_supported_jkp_factors.md)
  lists the available regions and selectors.

### Improvements

- The `sorting_variable` column of the `factor_library_grid` dataset no
  longer carries a `"sv_"` prefix, so its values now match the
  `sorting_variable` argument of
  `download_data("Tidy Finance", "factor_library", ...)` (e.g. `"bm"`
  rather than `"sv_bm"`).
  `download_data("Tidy Finance", "factor_library_grid")` returns the
  bare values accordingly
  ([\#284](https://github.com/tidy-finance/r-tidyfinance/issues/284)).
- Added a `tidyfinance` vignette that walks through the complete
  factor-construction workflow: download, signal construction,
  fiscal-year lagging, portfolio sorting, and a Fama-MacBeth test. It
  builds entirely on `download_data(domain = "Pseudo Data")`, so it
  compiles without a WRDS subscription or network access. `knitr` and
  `rmarkdown` are added back to `Suggests`, and `VignetteBuilder: knitr`
  is restored to `DESCRIPTION`.
- `download_data("Open Source Asset Pricing")` now aligns the `date`
  column to the beginning of the month (the dataset previously returned
  end-of-month dates), matching the convention used by the other
  download functions. All predictor columns are monthly long-short
  returns expressed in percent and are now divided by 100 to return
  plain numeric (decimal) returns.

### Bug fixes

- `download_data_huggingface("factor_library", ...)` now treats an
  explicit `n_portfolios_secondary = NULL` as “remove the filter and
  return all values” (univariate and bivariate sorts alike), consistent
  with the documented behavior for every other column. Previously an
  explicit `NULL` was coerced to `NA`, restricting the result to
  univariate sorts.

## tidyfinance 0.7.0

CRAN release: 2026-06-25

### Improvements

- [`estimate_betas()`](https://r.tidy-finance.org/reference/estimate_betas.md)
  now uses a fast, vectorized closed-form approach based on rolling
  cumulants of the moment matrices instead of fitting one regression per
  stock and window. This removes the need for per-stock nesting and the
  optional `furrr` parallelization, so the `use_furrr` argument and the
  `furrr` dependency have been dropped. Estimates are numerically
  identical to the previous regression-based implementation. Windows
  with fewer than `min_obs` observations are now dropped from the output
  rather than returned with `NA` coefficients.
- The package website moved from `package.tidy-finance.org` to
  `r.tidy-finance.org`.
- [`download_data()`](https://r.tidy-finance.org/reference/download_data.md)
  now uses the human-readable domain names returned by
  [`list_supported_datasets()`](https://r.tidy-finance.org/reference/list_supported_datasets.md)
  (e.g., `"Fama-French"`, `"Global Q"`, `"WRDS"`, `"Tidy Finance"`). The
  `"pseudo"` and `"tidyfinance"` domains were renamed to `"Pseudo Data"`
  and `"Tidy Finance"`. The previous machine-readable domain names
  (e.g., `"famafrench"`, `"wrds"`, `"pseudo"`, `"tidyfinance"`) are
  soft-deprecated but still accepted.
- [`download_data_wrds_crsp()`](https://r.tidy-finance.org/reference/download_data_wrds_crsp.md)
  now errors informatively when `version = "v1"` is used with an
  `end_date` later than December 2024, reflecting the discontinuation of
  the CRSP legacy version at the end of 2024.
- Removed the “experimental” lifecycle badge from
  [`assign_portfolio()`](https://r.tidy-finance.org/reference/assign_portfolio.md),
  [`compute_breakpoints()`](https://r.tidy-finance.org/reference/compute_breakpoints.md),
  [`compute_rolling_value()`](https://r.tidy-finance.org/reference/compute_rolling_value.md),
  [`estimate_model()`](https://r.tidy-finance.org/reference/estimate_model.md),
  and
  [`join_lagged_values()`](https://r.tidy-finance.org/reference/join_lagged_values.md),
  which are now considered stable.

## tidyfinance 0.6.0

CRAN release: 2026-05-31

### New features

- Added `domain = "pseudo"` to
  [`download_data()`](https://r.tidy-finance.org/reference/download_data.md)
  for generating pseudo data with the same schema as the corresponding
  real domain. Supported datasets in this release: `"crsp_monthly"`,
  `"crsp_daily"`, `"compustat_annual"`, `"compustat_quarterly"`, and
  `"ccm_links"` (all mirroring `domain = "wrds"`). Internally, every
  `domain = "pseudo"` call funnels through `simulate_pseudo_data()`, the
  unexported router that dispatches to per-dataset generators.
  Per-dataset entry points
  ([`download_data_pseudo_crsp()`](https://r.tidy-finance.org/reference/download_data_pseudo_crsp.md),
  [`download_data_pseudo_compustat()`](https://r.tidy-finance.org/reference/download_data_pseudo_compustat.md),
  [`download_data_pseudo_ccm_links()`](https://r.tidy-finance.org/reference/download_data_pseudo_ccm_links.md))
  remain exported for direct use. All generators accept `n_assets` and
  `seed` arguments; identical `(seed, n_assets)` yields the same
  identifier universe across datasets, so pseudo CRSP and Compustat join
  cleanly via `add_ccm_links = TRUE` or `ccm_links`. Daily CRSP is
  generated on weekdays only.
- Added
  [`download_factor_library_grid()`](https://r.tidy-finance.org/reference/download_factor_library_grid.md)
  to fetch the `tidy-finance/factor-library-grid` dataset from Hugging
  Face. Also accessible via
  `download_data("tidyfinance", "factor_library_grid")`.

### Improvements

- Added `test-coverage.yaml` workflow and badge to README.
- Added tests to get coverage to 100% (excl.
  [`set_wrds_credentials()`](https://r.tidy-finance.org/reference/set_wrds_credentials.md)).
- Fama-French factor data is now downloaded and parsed internally via
  `httr2`, so `frenchdata` is no longer declared in `Imports`. The
  behavior of
  [`download_data_factors_ff()`](https://r.tidy-finance.org/reference/download_data_factors_ff.md)
  is unchanged.
- `download_data("tidyfinance", "factor_library", ...)` now honors the
  canonical `start_date` and `end_date` arguments, filtering the
  returned portfolio returns to the requested range. When both are
  omitted, the full history is returned and the standard “Returning the
  full data set” message is emitted (via
  [`validate_dates()`](https://r.tidy-finance.org/reference/validate_dates.md)).
  Previously these arguments were accepted but silently ignored for the
  factor library.
- Removed the `using-tidyfinance` and `dates-in-tidyfinance` vignettes.
  Both predated the current
  [`download_data()`](https://r.tidy-finance.org/reference/download_data.md)
  interface and are superseded by the package manuscript. `knitr` and
  `rmarkdown` are no longer declared in `Suggests`, and
  `VignetteBuilder` has been dropped from `DESCRIPTION`.
- `download_data("tidyfinance", "factor_library", ids = <vector>)` now
  delegates directly to
  [`download_factor_library_ids()`](https://r.tidy-finance.org/reference/download_factor_library_ids.md),
  bypassing the grid filter. Passing `ids` together with filter
  arguments raises an informative error.
- Renamed
  [`list_supported_types()`](https://r.tidy-finance.org/reference/list_supported_datasets.md)
  to
  [`list_supported_datasets()`](https://r.tidy-finance.org/reference/list_supported_datasets.md)
  ([\#242](https://github.com/tidy-finance/r-tidyfinance/issues/242)).
  The old name remains exported as a soft-deprecated alias that forwards
  to the new function. Internal helpers were renamed accordingly
  (e.g. `list_supported_types_ff()` -\>
  [`list_supported_datasets_ff()`](https://r.tidy-finance.org/reference/list_supported_datasets_ff.md)).
- [`download_data_constituents()`](https://r.tidy-finance.org/reference/download_data_constituents.md)
  now drops symbols equal to `"-"`.
- Renamed `only_us` parameter in
  [`download_data_wrds_compustat()`](https://r.tidy-finance.org/reference/download_data_wrds_compustat.md)
  to `only_usd` to reflect that the filter keeps USD-denominated shares
  only. The old name is deprecated and forwards to `only_usd` with a
  warning.
- Removed `arrow`, `glue`, and `stringr` dependencies and added
  `nanoparquet`.

## tidyfinance 0.5.0

CRAN release: 2026-05-12

### New features

- Added
  [`implement_portfolio_sort()`](https://r.tidy-finance.org/reference/implement_portfolio_sort.md)
  as a convenience wrapper that combines sample construction filtering
  and portfolio return computation into a single call.
- Added
  [`download_data_risk_free()`](https://r.tidy-finance.org/reference/download_data_risk_free.md)
  to download and process risk-free rate data from FRED, splicing TB3MS
  (pre-2001) with DTB4WK (from 2001 onwards) for monthly data, and using
  DTB3 for daily data. Also accessible via
  `download_data("tidyfinance", "risk_free")`.
- Updated
  [`download_data_wrds_crsp()`](https://r.tidy-finance.org/reference/download_data_wrds_crsp.md)
  to use
  [`download_data_risk_free()`](https://r.tidy-finance.org/reference/download_data_risk_free.md)
  (FRED-based) instead of the Kenneth French risk-free rate when
  computing excess returns.
- Added
  [`download_data_risk_free()`](https://r.tidy-finance.org/reference/download_data_risk_free.md).
- Added `only_us` parameter to
  [`download_data_wrds_compustat()`](https://r.tidy-finance.org/reference/download_data_wrds_compustat.md).
- Added new parameters for common CRSP transformation tasks
  (`add_ccm_links`, `adjust_volume`) to
  [`download_data_wrds_crsp()`](https://r.tidy-finance.org/reference/download_data_wrds_crsp.md).
- Added `prc_adj` to `"crsp_monthly"` version `"v1"`.
- Added `adjust_volume` parameter for `"crsp_daily"` version `"v1"` and
  `"v2"` to
  [`download_data_wrds_compustat()`](https://r.tidy-finance.org/reference/download_data_wrds_compustat.md).
- Added
  [`compute_rolling_value()`](https://r.tidy-finance.org/reference/compute_rolling_value.md).
- Added `output` parameter to
  [`estimate_model()`](https://r.tidy-finance.org/reference/estimate_model.md)
  to also return t-stats or residuals.
- Added
  [`join_lagged_values()`](https://r.tidy-finance.org/reference/join_lagged_values.md).
- Added more indexes to
  [`list_supported_indexes()`](https://r.tidy-finance.org/reference/list_supported_indexes.md).
- Added
  [`download_data_huggingface()`](https://r.tidy-finance.org/reference/download_data_huggingface.md)
  and
  [`get_available_huggingface_files()`](https://r.tidy-finance.org/reference/get_available_huggingface_files.md).
  and support for `type = "hf_high_frequency_sp500"`.
- Deprecated `type` parameter in favor of `domain` and `dataset`.
- Added `detail` parameter to
  [`estimate_fama_macbeth()`](https://r.tidy-finance.org/reference/estimate_fama_macbeth.md)
  to include average `n_obs`, `r_squared`, and `adj_r_squared`.
- Removed lower bound of excess returns in
  [`download_data_wrds_crsp()`](https://r.tidy-finance.org/reference/download_data_wrds_crsp.md).
- Removed `add_lag_columns()` in favor of
  [`add_lagged_columns()`](https://r.tidy-finance.org/reference/add_lagged_columns.md).
- Added domain `"tidyfinance"` with datasets `"high_frequency_sp500"`,
  `"factor_library"`, and `"risk_free"`.

### Improvements

- Removed `renv` due to lack of benefits.
- Moved optional dependencies to imports for improved user experience
  (except for `furrr`).

### Bug fixes

- Removed erroneous time zone adjustment in
  [`download_data_wrds_trace_enhanced()`](https://r.tidy-finance.org/reference/download_data_wrds_trace_enhanced.md)
  [\#133](https://github.com/tidy-finance/r-tidyfinance/issues/133).
- Replaced tabs in `list_supported_types_ff()` with underscores
  [\#134](https://github.com/tidy-finance/r-tidyfinance/issues/134).
- [`compute_portfolio_returns()`](https://r.tidy-finance.org/reference/compute_portfolio_returns.md)
  and
  [`implement_portfolio_sort()`](https://r.tidy-finance.org/reference/implement_portfolio_sort.md)
  now apply `min_portfolio_size` to the reported portfolio
  cross-section. For bivariate sorts this is the firm count per
  `(main_portfolio, date)` summed across secondary buckets, not per
  `(main, secondary, date)` cell as before. Previously, setting
  `min_portfolio_size` to the number of cells
  (e.g. `n_main * n_secondary`) silently voided every cell. Univariate
  behaviour is unchanged. The default has changed from `0L` to `1L`, so
  each reported portfolio is required to have at least one observation
  by default; pass `min_portfolio_size = 0L` to deactivate the check.
  The param documentation has also been corrected to reflect that small
  portfolios receive `NA` (not zero).
- [`compute_long_short_returns()`](https://r.tidy-finance.org/reference/compute_long_short_returns.md)
  no longer errors with `object 'top' not found` when the input panel
  contains only one distinct portfolio (e.g., because
  [`assign_portfolio()`](https://r.tidy-finance.org/reference/assign_portfolio.md)
  collapsed to a single bucket on a constant sorting variable). The
  long-short return is now `NA` on such dates, consistent with “no
  investment, no return”, instead of crashing.

## tidyfinance 0.4.5

CRAN release: 2026-01-08

### Bug fixes

- Updated download of FRED data due to API changes.

## tidyfinance 0.4.4

CRAN release: 2025-05-07

### Bug fixes

- Removed user agent sampling from `download_stock_prices()`because they
  were blocked.

## tidyfinance 0.4.3.

CRAN release: 2024-12-17

### Bug fixes

- `download_constituents()` and `download_stock_prices()` now also fail
  gracefully with informative messages instead of errors or warnings.
- `download_factors()` returns empty data frame with `date` column to
  ensure vignettes are built even if resources are unavailable.

### Improvements

- Unified `start_date` and `end_date` validation across applications.
- Updated tests of `download_*()` functions to cover unavailable or
  broken resources.

## tidyfinance 0.4.2

CRAN release: 2024-12-02

### New features

- Added experimental `add_lag_columns()` function that is more efficient
  than `lag_column()`

### Bug fixes

- `download_macro_predictors()`, `download_factors()`, and
  `download_osap()` now fail gracefully with informative messages
  instead of errors or warnings.

### Improvements

- Updated `ccmxpf_linktable` to the new WRDS default `ccmxpf_lnkhist`.
- Added support for “factors_q5_annual” in `download_factors_q()`
- Optimized
  [`winsorize()`](https://r.tidy-finance.org/reference/winsorize.md) by
  reducing quantile recalculations

## tidyfinance 0.4.1

CRAN release: 2024-09-04

### Bug fixes

- Added missing support of “wrds_trace_enhanced” and “wrds_fisd” support
  to
  [`download_data_wrds()`](https://r.tidy-finance.org/reference/download_data_wrds.md).
- Added intercept to
  [`estimate_model()`](https://r.tidy-finance.org/reference/estimate_model.md),
  [`estimate_betas()`](https://r.tidy-finance.org/reference/estimate_betas.md),
  and
  [`estimate_fama_macbeth()`](https://r.tidy-finance.org/reference/estimate_fama_macbeth.md).

### Improvements

- Renamed `download_data_wrds_clean_trace()` to
  [`download_data_wrds_trace_enhanced()`](https://r.tidy-finance.org/reference/download_data_wrds_trace_enhanced.md)
  for improved consistency.
- Added `vcov_options` parameter to
  [`estimate_fama_macbeth()`](https://r.tidy-finance.org/reference/estimate_fama_macbeth.md).

## tidyfinance 0.4.0

CRAN release: 2024-08-30

### New features

- Added
  [`list_supported_indexes()`](https://r.tidy-finance.org/reference/list_supported_indexes.md)
  and
  [`download_data_constituents()`](https://r.tidy-finance.org/reference/download_data_constituents.md)
  to download index constituents.
- Added
  [`estimate_betas()`](https://r.tidy-finance.org/reference/estimate_betas.md)
  to estimate risk factor betas.
- Added
  [`estimate_fama_macbeth()`](https://r.tidy-finance.org/reference/estimate_fama_macbeth.md)
  to estimate Fama-MacBeth models.
- Added
  [`download_data_constituents()`](https://r.tidy-finance.org/reference/download_data_constituents.md)
  to download index constituents.
- Added
  [`download_data_osap()`](https://r.tidy-finance.org/reference/download_data_osap.md)
  to download data from Open Source Asset Pricing.
- Added
  [`download_data_fred()`](https://r.tidy-finance.org/reference/download_data_fred.md)
  to download data from Federal Reserve Economic Data.
- Added
  [`compute_portfolio_returns()`](https://r.tidy-finance.org/reference/compute_portfolio_returns.md)
  to implement different portfolio sorting approaches.
- Added
  [`compute_long_short_returns()`](https://r.tidy-finance.org/reference/compute_long_short_returns.md)
  to quickly compute long-short portfolio returns.
- Added
  [`compute_breakpoints()`](https://r.tidy-finance.org/reference/compute_breakpoints.md)
  to make
  [`assign_portfolio()`](https://r.tidy-finance.org/reference/assign_portfolio.md)
  more flexible.
- Added
  [`breakpoint_options()`](https://r.tidy-finance.org/reference/breakpoint_options.md)
  and
  [`data_options()`](https://r.tidy-finance.org/reference/data_options.md)
  to provide more flexibility with respect to column names.

### Bug fixes

- Retained explicit missing values in `mktcap_lag` in monthly CRSP.

### Improvements

- Migrated to `cli` for error messages and warnings.
- Aligned documentation across functions.
- Switched to `NULL` for optional default values.
- Removed dependency from named placeholder that is only available from
  R 4.2 on.
- Removed `readxl` dependency from
  [`download_data_macro_predictors()`](https://r.tidy-finance.org/reference/download_data_macro_predictors.md).
- Removed redundant `check_if_package_installed()` function.
- Updated
  [`estimate_model()`](https://r.tidy-finance.org/reference/estimate_model.md)
  to support both
  [`estimate_betas()`](https://r.tidy-finance.org/reference/estimate_betas.md)
  and
  [`estimate_fama_macbeth()`](https://r.tidy-finance.org/reference/estimate_fama_macbeth.md).
- Updated
  [`assign_portfolio()`](https://r.tidy-finance.org/reference/assign_portfolio.md)
  to support
  [`compute_portfolio_returns()`](https://r.tidy-finance.org/reference/compute_portfolio_returns.md).
- Renamed `download_data_stocks()` to
  [`download_data_stock_prices()`](https://r.tidy-finance.org/reference/download_data_stock_prices.md)
  for better naming.

## tidyfinance 0.3.0

CRAN release: 2024-07-23

### New features

- Added support for all available Fama-French datasets (check via
  [`list_supported_types()`](https://r.tidy-finance.org/reference/list_supported_datasets.md)).
  All type names are created from a string cleaning algorithm and are
  hence more consistent. We kept implicit support for legacy type names
  to avoid breaking existing code.
- Added new function to download stock data from Yahoo Finance:
  `download_data_stocks()`.
- Added support for `wrds_compustat_quarterly`.

### Bug fixes

- CRSP monthly data always contains the historically accurate stock
  characteristics instead of the oft misleading most recent information.
- Consistently implemented the `additional_columns` option for CRSP and
  Compustat instead of having the error prone option to pass columns via
  `...`.
- Added replacement of `-999` by NA in Fama-French types, which was
  missing in the initial implementation.

### Improvements

- Refactored the column name cleaning procedure in
  `download_data_factors()` to support all available column names in the
  Fama-French universe.
- Made all `start_date` and `end_date` optional with a message to user
  which dates are used as defaults.
- Introduced automatic checks via GitHub Actions workflows.
- Synchronized `date` column and its references across WRDS types (see
  corresponding vignette for more information).
- Improved handling of imports with `tidyfinance-package.R` file.
- Reformatted DESCRIPTION and roxygen comments for more consistency with
  `tidyverse` style.

## tidyfinance 0.2.1

CRAN release: 2024-07-03

### New features

- Added `domain` and `as_vector` parameters to
  [`list_supported_types()`](https://r.tidy-finance.org/reference/list_supported_datasets.md)

### Bug fixes

- Replaced `...` with `additional_columns` parameter and ensured that
  CRSP and Compustat types consider it correctly
- Removed `mkt_excess` column from type “wrds_crsp_monthly”

### Improvements

- Added `fixed = TRUE` to [`grepl()`](https://rdrr.io/r/base/grep.html)
  calls with fixed strings
- Switched to `NA_real_` instead of `as.double(NA)`
- Switched to [`toString()`](https://rdrr.io/r/base/toString.html)
  instead of [`paste0()`](https://rdrr.io/r/base/paste.html) with
  collapse
- Switched to
  [`dplyr::between()`](https://dplyr.tidyverse.org/reference/between.html)
  instead of unequal signs

## tidyfinance 0.2.0

CRAN release: 2024-05-29

### New features

- Added `vignettes/using-tidyfinance`
- Added
  [`set_wrds_credentials()`](https://r.tidy-finance.org/reference/set_wrds_credentials.md)
  function for a guided tour to store login data
- Added support for `"factors_ff_industry_*"` data types

### Bug fixes

- Removed `hml` and `smb` columns from `"wrds_crsp_monthly"` output
- Fixed stock filters for `"v2"` of `"wrds_crsp_*"` data types

### Improvements

- Relaxed package version requirements as much as possible with the
  current set of packages
- Split up the `download_data*` functions into multiple files for better
  maintenance

## tidyfinance 0.1.0

CRAN release: 2024-03-05

- Initial CRAN submission.
