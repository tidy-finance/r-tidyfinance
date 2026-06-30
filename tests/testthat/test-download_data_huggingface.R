# Full-schema grid row used by download_factor_library_ids tests.
make_grid <- function(id = 1L) {
  tibble::tibble(
    id = id,
    sorting_variable = "me",
    sorting_variable_lag = "6m",
    sorting_method = "univariate",
    n_portfolios_main = 10L,
    min_size_quantile = 0.2,
    exclude_financials = FALSE,
    exclude_utilities = FALSE,
    exclude_negative_earnings = FALSE,
    rebalancing = "monthly",
    n_portfolios_secondary = NA_real_,
    breakpoints_exchanges = "NYSE",
    breakpoints_min_size_threshold = NA_real_,
    weighting_scheme = "VW"
  )
}

# Mocks the full httr2 chain + jsonlite::fromJSON for one page.
# page_df is passed straight through fromJSON so the rest of
# the pipeline (tibble(), filter(), select()) runs for real.
mock_httr2 <- function(page_df, link = NULL) {
  testthat::local_mocked_bindings(
    request = function(url) list(url = url),
    req_user_agent = function(req, ...) req,
    req_perform = function(req, ...) list(),
    resp_body_string = function(resp) "[]",
    resp_headers = function(resp) list(link = link),
    .package = "httr2",
    .env = parent.frame()
  )
  testthat::local_mocked_bindings(
    fromJSON = function(...) page_df,
    .package = "jsonlite",
    .env = parent.frame()
  )
}

# ── get_available_huggingface_files ─────────────────

test_that("single page: returns only parquet files", {
  page_df <- data.frame(
    type = c("file", "file", "directory"),
    path = c("data.parquet", "readme.txt", "subdir"),
    size = c(100L, 10L, 0L),
    stringsAsFactors = FALSE
  )
  mock_httr2(page_df)

  result <- get_available_huggingface_files("org", "ds")

  expect_equal(nrow(result), 1L)
  expect_named(result, c("path", "size", "url"))
  expect_equal(result$path, "data.parquet")
})

test_that("multi-page: paginates until rel=next link absent", {
  page_n <- 0L
  pages <- list(
    data.frame(
      type = "file",
      path = "a.parquet",
      size = 1L,
      stringsAsFactors = FALSE
    ),
    data.frame(
      type = "file",
      path = "b.parquet",
      size = 2L,
      stringsAsFactors = FALSE
    )
  )
  links <- list('<https://page2.example.com>; rel="next"', NULL)

  testthat::local_mocked_bindings(
    request = function(url) list(url = url),
    req_user_agent = function(req, ...) req,
    req_perform = function(req, ...) {
      page_n <<- page_n + 1L
      list()
    },
    resp_body_string = function(resp) "[]",
    resp_headers = function(resp) list(link = links[[page_n]]),
    .package = "httr2"
  )
  testthat::local_mocked_bindings(
    fromJSON = function(...) pages[[page_n]],
    .package = "jsonlite"
  )

  result <- get_available_huggingface_files("org", "ds")

  expect_equal(nrow(result), 2L)
})

# ── download_data_huggingface ────────────────────────

test_that("aborts when dataset is NULL", {
  expect_error(
    download_data_huggingface(dataset = NULL),
    class = "rlang_error"
  )
})

test_that("deprecated type arg warns and strips hf_ prefix", {
  withr::local_options(lifecycle_verbosity = "warning")
  testthat::local_mocked_bindings(
    download_factor_library_grid = function() {
      tibble::tibble(id = integer(0))
    }
  )

  expect_warning(
    download_data_huggingface(type = "hf_factor_library_grid"),
    regexp = "deprecated"
  )
})

test_that("legacy hf_ dataset value warns and strips prefix", {
  withr::local_options(lifecycle_verbosity = "warning")
  testthat::local_mocked_bindings(
    download_factor_library_grid = function() {
      tibble::tibble(id = integer(0))
    }
  )

  expect_warning(
    download_data_huggingface(
      dataset = "hf_factor_library_grid"
    ),
    regexp = "deprecated"
  )
})

test_that("aborts for unsupported dataset", {
  expect_error(
    download_data_huggingface(dataset = "unknown"),
    class = "rlang_error"
  )
})

test_that("factor_library_grid: delegates to helper", {
  mock_grid <- tibble::tibble(id = 1L)
  testthat::local_mocked_bindings(
    download_factor_library_grid = function() mock_grid
  )

  result <- download_data_huggingface("factor_library_grid")

  expect_equal(result, mock_grid)
})

test_that("high_frequency_sp500: filters by date and downloads", {
  available <- tibble::tibble(
    path = "date=2007-07-26/part.parquet",
    size = 100L,
    url = "https://example.com/part.parquet"
  )
  mock_trades <- tibble::tibble(price = 100.0)

  testthat::local_mocked_bindings(
    get_available_huggingface_files = function(...) available
  )
  testthat::local_mocked_bindings(
    read_parquet_url = function(...) mock_trades
  )

  result <- download_data_huggingface(
    "high_frequency_sp500",
    "2007-07-26",
    "2007-07-26"
  )

  expect_equal(result, mock_trades)
})

test_that("factor_library: delegates to inner helper", {
  mock_returns <- tibble::tibble(id = 1L, ret = 0.01)
  testthat::local_mocked_bindings(
    download_data_hugging_face_factor_library = function(...) {
      mock_returns
    }
  )

  result <- download_data_huggingface(
    "factor_library",
    sorting_variable = "me"
  )

  expect_equal(result, mock_returns)
})

test_that("factor_library: forwards start_date and end_date", {
  captured <- NULL
  testthat::local_mocked_bindings(
    download_data_hugging_face_factor_library = function(...) {
      captured <<- list(...)
      tibble::tibble(id = 1L)
    }
  )

  download_data_huggingface(
    "factor_library",
    sorting_variable = "me",
    start_date = "2020-01-01",
    end_date = "2020-12-31"
  )

  expect_equal(captured$start_date, "2020-01-01")
  expect_equal(captured$end_date, "2020-12-31")
  expect_equal(captured$sorting_variable, "me")
})

test_that("high_frequency_sp500: uses sample window when no dates", {
  available <- tibble::tibble(
    path = "date=2007-06-27/part.parquet",
    size = 100L,
    url = "https://example.com/part.parquet"
  )
  testthat::local_mocked_bindings(
    get_available_huggingface_files = function(...) available
  )
  testthat::local_mocked_bindings(
    read_parquet_url = function(...) tibble::tibble(price = 100.0)
  )

  # The default sample window (2007-06-27..2007-07-27) includes the only
  # available file, so a row is returned even though no dates were passed.
  result <- download_data_huggingface("high_frequency_sp500")

  expect_equal(nrow(result), 1L)
})

# ── filter_factor_library_grid ───────────────────────

test_that("aborts for unsupported filter name", {
  expect_error(
    filter_factor_library_grid(bad_col = "x"),
    class = "rlang_error"
  )
})

test_that("aborts: non-univariate sort without secondary n", {
  # error fires before download_factor_library_grid() is reached
  expect_error(
    filter_factor_library_grid(
      sorting_variable = "me",
      sorting_method = "sequential"
    ),
    class = "rlang_error"
  )
})

test_that("fill_all = FALSE: defaults applied, row filtered out", {
  # Row 2 differs only in min_size_quantile (0.4 vs default 0.2)
  # and should be dropped; all other columns match defaults.
  grid <- tibble::tibble(
    id = c(1L, 2L),
    sorting_variable = c("me", "me"),
    min_size_quantile = c(0.2, 0.4),
    exclude_financials = c(FALSE, FALSE),
    exclude_utilities = c(FALSE, FALSE),
    exclude_negative_earnings = c(FALSE, FALSE),
    sorting_variable_lag = c("6m", "6m"),
    rebalancing = c("monthly", "monthly"),
    n_portfolios_main = c(10L, 10L),
    sorting_method = c("univariate", "univariate"),
    n_portfolios_secondary = c(NA_real_, NA_real_),
    breakpoints_exchanges = c("NYSE", "NYSE"),
    breakpoints_min_size_threshold = c(NA_real_, NA_real_),
    weighting_scheme = c("VW", "VW")
  )
  testthat::local_mocked_bindings(
    download_factor_library_grid = function() grid
  )

  ids <- filter_factor_library_grid(sorting_variable = "me")

  expect_equal(ids, 1L)
})

test_that("fill_all = TRUE: only explicit filters applied", {
  # Both rows share all columns except sorting_variable and
  # weighting_scheme. With fill_all = TRUE, only the explicit
  # sorting_variable = "me" filter is applied; row 2 ("bm")
  # is dropped while row 1 ("me") passes regardless of the
  # differing weighting_scheme.
  grid <- tibble::tibble(
    id = c(1L, 2L),
    sorting_variable = c("me", "bm"),
    min_size_quantile = c(0.2, 0.2),
    exclude_financials = c(FALSE, FALSE),
    exclude_utilities = c(FALSE, FALSE),
    exclude_negative_earnings = c(FALSE, FALSE),
    sorting_variable_lag = c("6m", "6m"),
    rebalancing = c("monthly", "monthly"),
    n_portfolios_main = c(10L, 10L),
    sorting_method = c("univariate", "univariate"),
    n_portfolios_secondary = c(NA_real_, NA_real_),
    breakpoints_exchanges = c("NYSE", "NYSE"),
    breakpoints_min_size_threshold = c(NA_real_, NA_real_),
    weighting_scheme = c("EW", "VW")
  )
  testthat::local_mocked_bindings(
    download_factor_library_grid = function() grid
  )

  ids <- filter_factor_library_grid(
    sorting_variable = "me",
    fill_all = TRUE
  )

  expect_equal(ids, 1L)
})

test_that("explicit n_portfolios_secondary = NULL returns all values", {
  # Row 1 is a univariate sort (NA secondary); row 2 is a bivariate sort
  # with a non-NA secondary. Passing NULL should remove the filter and
  # return both, rather than restricting to the NA (univariate) row.
  grid <- tibble::tibble(
    id = c(1L, 2L),
    sorting_variable = c("sv_me", "sv_me"),
    min_size_quantile = c(0.2, 0.2),
    exclude_financials = c(FALSE, FALSE),
    exclude_utilities = c(FALSE, FALSE),
    exclude_negative_earnings = c(FALSE, FALSE),
    sorting_variable_lag = c("6m", "6m"),
    rebalancing = c("monthly", "monthly"),
    n_portfolios_main = c(10L, 10L),
    sorting_method = c("univariate", "univariate"),
    n_portfolios_secondary = c(NA_real_, 5),
    breakpoints_exchanges = c("NYSE", "NYSE"),
    breakpoints_min_size_threshold = c(NA_real_, NA_real_),
    weighting_scheme = c("VW", "VW")
  )
  testthat::local_mocked_bindings(
    download_factor_library_grid = function() grid
  )

  ids <- filter_factor_library_grid(
    sorting_variable = "me",
    n_portfolios_secondary = NULL
  )

  expect_equal(ids, c(1L, 2L))
})

# ── download_factor_library_grid ─────────────────────

test_that("pulls url from available files and reads parquet", {
  available <- tibble::tibble(
    path = "grid.parquet",
    size = 500L,
    url = "https://example.com/grid.parquet"
  )
  mock_grid <- tibble::tibble(id = 1L)

  testthat::local_mocked_bindings(
    get_available_huggingface_files = function(...) available
  )
  testthat::local_mocked_bindings(
    read_parquet_url = function(...) mock_grid
  )

  expect_equal(download_factor_library_grid(), mock_grid)
})

# ── download_factor_library_ids ──────────────────────

test_that("aborts when no grid rows match requested ids", {
  # make_grid(42L) has id = 42; requesting id = 999 yields an
  # empty inner_join, so relevant_urls is empty.
  testthat::local_mocked_bindings(
    download_factor_library_grid = function() make_grid(42L),
    get_available_huggingface_files = function(...) {
      tibble::tibble(
        path = character(0),
        size = numeric(0),
        url = character(0)
      )
    }
  )

  expect_error(
    download_factor_library_ids(999L),
    class = "rlang_error"
  )
})

test_that("aborts when ids have no matching parquet file", {
  # The available path "unrelated/data.parquet" does not match
  # the regex in tidyr::extract, so all key columns are NA and
  # the left_join leaves url = NA for the matched grid row.
  testthat::local_mocked_bindings(
    download_factor_library_grid = function() make_grid(1L),
    get_available_huggingface_files = function(...) {
      tibble::tibble(
        path = "unrelated/data.parquet",
        size = 100L,
        url = "https://example.com/unrelated/data.parquet"
      )
    }
  )

  expect_error(
    download_factor_library_ids(1L),
    class = "rlang_error"
  )
})

test_that("downloads returns and joins grid metadata", {
  fpath <- paste0(
    "sorting_variable=me/",
    "sorting_variable_lag=6m/",
    "sorting_method=univariate/",
    "n_portfolios_main=10/",
    "data.parquet"
  )
  mock_returns <- tibble::tibble(id = 1L, ret = 0.01)

  testthat::local_mocked_bindings(
    download_factor_library_grid = function() make_grid(1L),
    get_available_huggingface_files = function(...) {
      tibble::tibble(
        path = fpath,
        size = 100L,
        url = paste0("https://example.com/", fpath)
      )
    }
  )
  testthat::local_mocked_bindings(
    read_parquet_url = function(...) mock_returns
  )

  result <- download_factor_library_ids(1L)

  expect_true("ret" %in% names(result))
  expect_true("weighting_scheme" %in% names(result))
})

# ── download_data_hugging_face_factor_library ────────

test_that("aborts when ids and filter args are combined", {
  expect_error(
    download_data_hugging_face_factor_library(
      sorting_variable = "me",
      ids = 1L
    ),
    class = "rlang_error"
  )
})

test_that("with ids: delegates to download_factor_library_ids", {
  mock_result <- tibble::tibble(id = 1L, ret = 0.01)
  testthat::local_mocked_bindings(
    validate_dates = function(start_date, end_date) {
      list(start_date = NULL, end_date = NULL)
    },
    download_factor_library_ids = function(ids) mock_result
  )

  result <- download_data_hugging_face_factor_library(ids = 1L)

  expect_equal(result, mock_result)
})

test_that("without ids: resolves via grid then downloads", {
  mock_result <- tibble::tibble(id = 1L, ret = 0.01)
  testthat::local_mocked_bindings(
    validate_dates = function(start_date, end_date) {
      list(start_date = NULL, end_date = NULL)
    },
    filter_factor_library_grid = function(...) 1L,
    download_factor_library_ids = function(ids) mock_result
  )

  result <- download_data_hugging_face_factor_library(
    sorting_variable = "me"
  )

  expect_equal(result, mock_result)
})

test_that("filters returns to the requested date range", {
  mock_returns <- tibble::tibble(
    id = 1L,
    date = as.Date(c("2019-12-31", "2020-06-30", "2021-01-31")),
    ret = c(0.01, 0.02, 0.03)
  )
  testthat::local_mocked_bindings(
    validate_dates = function(start_date, end_date) {
      list(start_date = as.Date(start_date), end_date = as.Date(end_date))
    },
    download_factor_library_ids = function(ids) mock_returns
  )

  result <- download_data_hugging_face_factor_library(
    ids = 1L,
    start_date = "2020-01-01",
    end_date = "2020-12-31"
  )

  expect_equal(result$date, as.Date("2020-06-30"))
})

test_that("returns full history when dates omitted", {
  mock_returns <- tibble::tibble(
    id = 1L,
    date = as.Date(c("2019-12-31", "2020-06-30")),
    ret = c(0.01, 0.02)
  )
  testthat::local_mocked_bindings(
    validate_dates = function(start_date, end_date) {
      list(start_date = NULL, end_date = NULL)
    },
    download_factor_library_ids = function(ids) mock_returns
  )

  result <- download_data_hugging_face_factor_library(ids = 1L)

  expect_equal(result, mock_returns)
})

test_that("date filtering also applies on the grid-resolved path", {
  mock_returns <- tibble::tibble(
    id = 1L,
    date = as.Date(c("2018-01-31", "2020-06-30")),
    ret = c(0.01, 0.02)
  )
  testthat::local_mocked_bindings(
    validate_dates = function(start_date, end_date) {
      list(start_date = as.Date(start_date), end_date = as.Date(end_date))
    },
    filter_factor_library_grid = function(...) 1L,
    download_factor_library_ids = function(ids) mock_returns
  )

  result <- download_data_hugging_face_factor_library(
    sorting_variable = "me",
    start_date = "2020-01-01",
    end_date = "2020-12-31"
  )

  expect_equal(result$date, as.Date("2020-06-30"))
})
