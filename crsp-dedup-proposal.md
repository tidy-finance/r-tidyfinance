# Proposal: De-duplicating `download_data_wrds_crsp()`

**Status:** Proposal / RFC — no behaviour change intended
**Target file:** `R/download_data_wrds_crsp.R` (currently 685 lines)
**Author:** codebase review

## 1. Problem

`download_data_wrds_crsp()` is a single 685-line function built from **four large,
mostly parallel blocks**:

| Block | Lines (approx.) | Source tables |
|-------|-----------------|---------------|
| `crsp_monthly`, `version = "v1"` | 122–262 (~140) | `crsp.msf`, `crsp.msenames`, `crsp.msedelist` |
| `crsp_monthly`, `version = "v2"` | 264–383 (~120) | `crsp.msf_v2`, `crsp.stksecurityinfohist` |
| `crsp_daily`, `version = "v1"`   | 386–514 (~128) | `crsp.dsf`, `crsp.msenames`, `crsp.msedelist` |
| `crsp_daily`, `version = "v2"`   | 515–649 (~134) | `crsp.dsf_v2`, `crsp.stksecurityinfohist` |

The v1/v2 pairs differ only in table names, a handful of column names, and two
narrow pieces of business logic (delisting adjustment, volume adjustment). Most
of the surrounding post-processing is **identical or near-identical**, which
makes the function hard to read and easy to break: a fix to the industry
mapping or the excess-return calculation currently has to be made in two or
four places.

## 2. Duplicated logic (verbatim or near-verbatim)

The following fragments are repeated across blocks and are good candidates for
extraction into small, individually testable internal helpers.

### 2.1 Industry classification — *identical*
Lines **211–226** (monthly v1) and **353–368** (monthly v2) are byte-for-byte
identical: an 11-branch `case_when()` mapping SIC code ranges to industry
labels.

### 2.2 `listing_age` from first CRSP date — *identical*
Lines **177–185** and **319–327**: the same
`pmax(as.integer(interval(first_crsp_date, date) %/% months(1)), 0L)`.

### 2.3 `mktcap_lag` — *identical*
Lines **193–198** and **335–340**: lag market cap by one month and self-join.

### 2.4 Excess return over the risk-free rate — *identical structure*
Lines **254–259** and **375–380** (and the daily equivalents at **474–479** /
**597–602**): join `download_data_risk_free()`, compute `ret_excess`, drop
`risk_free`. Differs only in the name of the return column (`ret_adj` vs `ret`).

### 2.5 Gao & Ritter (2010) volume adjustment — *near-identical*
Lines **481–505** (daily v1) and **604–640** (daily v2): the same three cutoff
dates (`2001-02-01`, `2002-01-01`, `2004-01-01`) and the same NASDAQ divisor
schedule (`2.0 / 1.8 / 1.6 / 1.0`). Differs only in the exchange predicate
(`exchcd == 3` vs `primaryexch == "Q"`) and the source price/volume columns.

### 2.6 Daily batch-download scaffolding — *near-identical*
Lines **419–514** (daily v1) and **554–649** (daily v2): the same
`ceiling(length(permnos) / batch_size)` batch count, `cli::cli_progress_bar()`,
the `for (j in 1:batches)` permno-slicing loop, and the closing
`bind_rows(crsp_daily_list)`. Only the inner per-batch query differs.

### 2.7 `adjust_volume` argument check — *near-identical*
Lines **387–397** and **516–531**: same shape, different required column names.

### 2.8 Exchange classification — *parallel*
Lines **201–208** (`exchcd`) and **342–350** (`primaryexch`): same 4-way shape,
different source column and codes.

## 3. Proposed helpers

All helpers are internal (`@noRd`), pure data-frame transforms, and side-effect
free (no DB access), which makes them easy to unit-test on small tibbles.

```r
# --- classification ---------------------------------------------------------

assign_crsp_industry <- function(data) {
  data |>
    mutate(industry = case_when(
      siccd >= 1    & siccd <= 999  ~ "Agriculture",
      siccd >= 1000 & siccd <= 1499 ~ "Mining",
      siccd >= 1500 & siccd <= 1799 ~ "Construction",
      siccd >= 2000 & siccd <= 3999 ~ "Manufacturing",
      siccd >= 4000 & siccd <= 4899 ~ "Transportation",
      siccd >= 4900 & siccd <= 4999 ~ "Utilities",
      siccd >= 5000 & siccd <= 5199 ~ "Wholesale",
      siccd >= 5200 & siccd <= 5999 ~ "Retail",
      siccd >= 6000 & siccd <= 6799 ~ "Finance",
      siccd >= 7000 & siccd <= 8999 ~ "Services",
      siccd >= 9000 & siccd <= 9999 ~ "Public",
      .default = "Missing"
    ))
}

assign_crsp_exchange <- function(data, version) {
  if (version == "v1") {
    mutate(data, exchange = case_when(
      exchcd %in% c(1, 31) ~ "NYSE",
      exchcd %in% c(2, 32) ~ "AMEX",
      exchcd %in% c(3, 33) ~ "NASDAQ",
      .default = "Other"
    ))
  } else {
    mutate(data, exchange = case_when(
      primaryexch == "N" ~ "NYSE",
      primaryexch == "A" ~ "AMEX",
      primaryexch == "Q" ~ "NASDAQ",
      .default = "Other"
    ))
  }
}

# --- shared monthly post-processing ----------------------------------------

add_listing_age <- function(data, first_crsp_date) {
  data |>
    left_join(first_crsp_date, by = "permno") |>
    mutate(listing_age = pmax(
      as.integer(lubridate::interval(first_crsp_date, date) %/% months(1)),
      0L
    )) |>
    select(-first_crsp_date)
}

add_mktcap_lag <- function(data) {
  mktcap_lag <- data |>
    mutate(date = date %m+% months(1)) |>
    select(permno, date, mktcap_lag = mktcap)
  left_join(data, mktcap_lag, join_by(permno, date))
}

add_excess_return <- function(data, ret_col, start_date, end_date,
                              frequency = "monthly") {
  risk_free <- download_data_risk_free(
    start_date = start_date, end_date = end_date, frequency = frequency
  )
  data |>
    left_join(risk_free, join_by(date)) |>
    mutate(ret_excess = .data[[ret_col]] - risk_free) |>
    select(-risk_free)
}

# --- shared volume adjustment ----------------------------------------------

adjust_volume_gao_ritter <- function(data, on_nasdaq) {
  # `on_nasdaq` is a logical vector marking NASDAQ rows
  gr1 <- as.Date("2001-02-01"); gr2 <- as.Date("2002-01-01")
  gr3 <- as.Date("2004-01-01")
  mutate(data, vol_adj = case_when(
    on_nasdaq & date <  gr1               ~ vol / 2.0,
    on_nasdaq & date >= gr1 & date < gr2  ~ vol / 1.8,
    on_nasdaq & date >= gr2 & date < gr3  ~ vol / 1.6,
    on_nasdaq & date >= gr3               ~ vol / 1.0,
    .default = vol
  ))
}
```

For the **daily batch scaffolding** (2.6), extract a driver that owns the loop,
the progress bar and the batching, and takes a per-batch callback that performs
the version-specific query + post-processing:

```r
download_crsp_daily_batched <- function(permnos, batch_size, process_batch) {
  batches <- ceiling(length(permnos) / batch_size)
  out <- vector("list", batches)
  cli::cli_progress_bar("Downloading batches", total = batches, clear = TRUE)
  for (j in seq_len(batches)) {
    idx <- ((j - 1) * batch_size + 1):min(j * batch_size, length(permnos))
    batch <- process_batch(permnos[idx])
    if (nrow(batch) > 0) out[[j]] <- batch
    cli::cli_progress_update()
  }
  bind_rows(out)
}
```

## 4. Recommended structure

Split the monolith into four thin internal functions behind the existing public
entry point, each delegating shared work to the helpers above:

```
download_data_wrds_crsp()        # arg handling + dispatch + add_ccm_links
├── download_crsp_monthly(con, version, ...)   # v1/v2 differences localised
└── download_crsp_daily(con,   version, ...)   # uses download_crsp_daily_batched()
```

`add_ccm_links` post-processing (lines 653–668) already sits *after* the
dispatch and stays in the entry point unchanged.

## 5. Expected impact

- **~685 → ~400 lines** in `download_data_wrds_crsp.R` (rough estimate).
- Industry / exchange / listing-age / mktcap-lag / excess-return / volume-adj
  logic each live in **one** place.
- The v1↔v2 differences become explicit and small (table names + a few column
  renames + two business-logic switches), instead of being buried in four large
  copies.
- No change to the public signature, returned columns, column order, or
  messages — this is a pure internal refactor.

## 6. Risks & how to de-risk

1. **Subtle v1/v2 column differences.** `mktcap` uses `abs(shrout * altprc)`
   (v1) vs `shrout * prc` (v2); the delisting `ret_adj` adjustment exists only
   in monthly v1; v2 daily derives `cfacpr = cumprod(dlyfacprc)` before the
   volume adjustment. These must stay version-local and **not** be folded into a
   shared helper.
2. **Column order / names are part of the contract.** Several downstream
   functions and tests rely on exact output columns. The refactor must preserve
   the final `select()`/`mutate()` ordering.
3. **Snapshot/error-message tests.** The `adjust_volume` argument-check messages
   are user-facing; keep the wording identical.

### Verification plan
- Run the existing `tests/testthat/test-download_data_wrds_crps.R` suite.
- Add focused unit tests for each new pure helper (`assign_crsp_industry()`,
  `adjust_volume_gao_ritter()`, etc.) on tiny hand-built tibbles — these need no
  WRDS connection and lock in behaviour before/after.
- Golden-master check: capture `download_data_wrds_crsp()` output for
  monthly/daily × v1/v2 on a fixed date range **before** the refactor and
  `expect_equal()` against it **after**. (Requires WRDS credentials; run locally
  in CI or by a maintainer.)

## 7. Suggested sequencing

1. Land the pure helpers (§3) + their unit tests, rewiring the existing blocks
   to call them — smallest, safest diff first.
2. Extract `download_crsp_daily_batched()` and route both daily branches
   through it.
3. Split into `download_crsp_monthly()` / `download_crsp_daily()` and slim the
   entry point to arg-handling + dispatch.

Each step is independently reviewable and leaves the test suite green.
