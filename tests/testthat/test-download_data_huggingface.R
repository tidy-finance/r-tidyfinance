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

# Returns file with the columns of the factor library files on the Hub.
make_returns <- function(id, ret = 0.01) {
  tibble::tibble(id = id, date = as.Date("2020-01-01"), ret = ret)
}

# Costs file with the columns of the files in costs/ on the Hub.
make_costs <- function(id, cost = 0.001) {
  tibble::tibble(
    id = id,
    date = as.Date("2020-01-01"),
    turnover = 0.1,
    cost = cost,
    microcap_share = 0.2
  )
}

# Persistence file with the columns of the file on the Hub.
make_persistence <- function() {
  tibble::tibble(
    sorting_variable = c("bm", "bm", "size"),
    sorting_variable_lag = "6m",
    rebalancing = "monthly",
    date = as.Date(c("2019-12-01", "2020-06-01", "2020-06-01")),
    persistence = c(0.9, 0.8, 0.7)
  )
}

# Stands in for read_factor_library_files(), serving `files` by path. A path
# not in `files` is missing on the Hub, as for a range of ids in which no
# portfolio sort produced portfolios.
serve_files <- function(files) {
  function(dataset, revision, paths) {
    lapply(paths, \(path) files[[path]])
  }
}

# Empties the session cache of the factor library grid for one test and
# restores its previous content afterwards.
local_empty_factor_library_cache <- function(env = parent.frame()) {
  old <- factor_library_cache$grids
  factor_library_cache$grids <- list()
  withr::defer(factor_library_cache$grids <- old, envir = env)
}

# Body of a parquet file, as the Hub sends it.
parquet_body <- function(data) {
  file <- withr::local_tempfile(fileext = ".parquet")
  nanoparquet::write_parquet(data, file)
  readBin(file, "raw", file.size(file))
}

# Response of the Hub when it refuses a request because of its rate limit.
rate_limited <- function(seconds = 57) {
  httr2::response(
    429L,
    headers = list(RateLimit = paste0('"resolvers";r=0;t=', seconds))
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
    download_factor_library_grid = function(...) {
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
    download_factor_library_grid = function(...) {
      tibble::tibble(id = integer(0))
    }
  )

  expect_warning(
    download_data_huggingface(
      dataset = "hf_factor_library_grid"
    ),
    regexp = paste0(
      '`dataset = "hf_factor_library_grid"`.*',
      '`dataset = "factor_library_grid"`'
    )
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
    download_factor_library_grid = function(...) mock_grid
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
  expect_equal(captured$table, "returns")
})

test_that("factor_library_costs: delegates with the costs table", {
  captured <- NULL
  testthat::local_mocked_bindings(
    download_data_hugging_face_factor_library = function(...) {
      captured <<- list(...)
      tibble::tibble(id = 1L)
    }
  )

  download_data_huggingface(
    "factor_library_costs",
    sorting_variable = "bm",
    revision = "abc",
    grid_revision = "def"
  )

  expect_equal(captured$table, "costs")
  expect_equal(captured$sorting_variable, "bm")
  expect_equal(captured$revision, "abc")
  expect_equal(captured$grid_revision, "def")
})

test_that("factor_library_grid: forwards refresh and grid_revision", {
  captured <- NULL
  testthat::local_mocked_bindings(
    download_factor_library_grid = function(...) {
      captured <<- list(...)
      tibble::tibble(id = 1L)
    }
  )

  download_data_huggingface(
    "factor_library_grid",
    refresh = TRUE,
    grid_revision = "def"
  )

  expect_equal(captured, list(refresh = TRUE, grid_revision = "def"))
})

test_that("factor_library_persistence: delegates with dates", {
  captured <- NULL
  testthat::local_mocked_bindings(
    download_factor_library_persistence = function(...) {
      captured <<- list(...)
      make_persistence()
    }
  )

  result <- download_data_huggingface(
    "factor_library_persistence",
    start_date = "2020-01-01",
    end_date = "2020-12-31",
    sorting_variable = "bm",
    grid_revision = "def"
  )

  expect_equal(result, make_persistence())
  expect_equal(
    captured,
    list(
      sorting_variable = "bm",
      grid_revision = "def",
      start_date = "2020-01-01",
      end_date = "2020-12-31"
    )
  )
})

test_that("grid and persistence reject revision", {
  # The grid and the persistence are in another dataset than the returns, so
  # `revision` would silently pin nothing
  expect_error(
    download_data_huggingface("factor_library_grid", revision = "abc"),
    "grid_revision"
  )
  expect_error(
    download_data_huggingface("factor_library_persistence", revision = "abc"),
    "grid_revision"
  )
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
    download_factor_library_grid = function(...) grid
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
    download_factor_library_grid = function(...) grid
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
    sorting_variable = c("me", "me"),
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
    download_factor_library_grid = function(...) grid
  )

  ids <- filter_factor_library_grid(
    sorting_variable = "me",
    n_portfolios_secondary = NULL
  )

  expect_equal(ids, c(1L, 2L))
})

test_that("filters the grid of the requested revision", {
  revisions <- character(0)
  testthat::local_mocked_bindings(
    download_factor_library_grid = function(grid_revision) {
      revisions <<- c(revisions, grid_revision)
      make_grid(1L)
    }
  )

  filter_factor_library_grid(sorting_variable = "me", grid_revision = "def")

  expect_equal(revisions, "def")
})

# ── download_factor_library_grid ─────────────────────

test_that("reads the grid file by name", {
  mock_grid <- tibble::tibble(id = 1L)
  requested <- NULL
  local_empty_factor_library_cache()

  testthat::local_mocked_bindings(
    # The repo also holds slices of the grid, so its file listing must not
    # decide which file is read
    get_available_huggingface_files = function(...) {
      stop("The file listing of the grid repo must not be queried.")
    },
    read_factor_library_files = function(dataset, revision, paths) {
      requested <<- list(dataset = dataset, revision = revision, paths = paths)
      list(mock_grid)
    }
  )

  expect_equal(download_factor_library_grid(), mock_grid)
  expect_equal(
    requested,
    list(
      dataset = "factor-library-grid",
      revision = "main",
      paths = "portfolio_sort_grid.parquet"
    )
  )
})

test_that("caches the grid within the session", {
  local_empty_factor_library_cache()
  n_downloads <- 0L
  testthat::local_mocked_bindings(
    read_factor_library_files = function(...) {
      n_downloads <<- n_downloads + 1L
      list(make_grid(n_downloads))
    }
  )

  first <- download_factor_library_grid()
  second <- download_factor_library_grid()

  expect_equal(n_downloads, 1L)
  expect_identical(second, first)
})

test_that("caches one grid per revision", {
  local_empty_factor_library_cache()
  requested <- character(0)
  testthat::local_mocked_bindings(
    read_factor_library_files = function(dataset, revision, paths) {
      requested <<- c(requested, revision)
      list(make_grid(length(requested)))
    }
  )

  main <- download_factor_library_grid()
  pinned <- download_factor_library_grid(grid_revision = "abc")

  # Each revision is downloaded once and keeps its own grid
  expect_identical(download_factor_library_grid(), main)
  expect_identical(download_factor_library_grid(grid_revision = "abc"), pinned)
  expect_equal(requested, c("main", "abc"))
  expect_equal(main$id, 1L)
  expect_equal(pinned$id, 2L)
  expect_named(factor_library_cache$grids, c("main", "abc"))
})

test_that("refresh = TRUE downloads the grid again", {
  local_empty_factor_library_cache()
  n_downloads <- 0L
  testthat::local_mocked_bindings(
    read_factor_library_files = function(...) {
      n_downloads <<- n_downloads + 1L
      list(make_grid(n_downloads))
    }
  )

  download_factor_library_grid()
  pinned <- download_factor_library_grid(grid_revision = "abc")
  refreshed <- download_factor_library_grid(refresh = TRUE)

  expect_equal(n_downloads, 3L)
  expect_equal(refreshed$id, 3L)
  expect_identical(download_factor_library_grid(), refreshed)
  # Refreshing one revision keeps the grid of another
  expect_identical(download_factor_library_grid(grid_revision = "abc"), pinned)
})

test_that("does not cache a failed download", {
  local_empty_factor_library_cache()
  testthat::local_mocked_bindings(
    read_factor_library_files = function(...) cli::cli_abort("network down")
  )

  expect_error(download_factor_library_grid(), "network down")
  expect_null(factor_library_cache$grids[["main"]])
})

test_that("aborts when the revision has no grid", {
  local_empty_factor_library_cache()
  testthat::local_mocked_bindings(
    read_factor_library_files = serve_files(list())
  )

  expect_error(
    download_factor_library_grid(grid_revision = "abc"),
    "is not in revision"
  )
  expect_length(factor_library_cache$grids, 0)
})

test_that("aborts on an invalid refresh argument", {
  expect_error(download_factor_library_grid(refresh = NA), "refresh")
  expect_error(download_factor_library_grid(refresh = "yes"), "refresh")
})

test_that("aborts on an invalid revision", {
  expect_error(
    download_factor_library_grid(grid_revision = NA_character_),
    "grid_revision"
  )
  expect_error(
    download_factor_library_grid(grid_revision = c("a", "b")),
    "grid_revision"
  )
  expect_error(download_factor_library_grid(grid_revision = ""), "single")
  expect_error(download_factor_library_ids(1L, revision = 1), "revision")
})

test_that("download_factor_library_ids reuses the cached grid across calls", {
  local_empty_factor_library_cache()
  n_grid_downloads <- 0L
  testthat::local_mocked_bindings(
    read_factor_library_files = function(dataset, revision, paths) {
      if (identical(paths, "portfolio_sort_grid.parquet")) {
        n_grid_downloads <<- n_grid_downloads + 1L
        list(make_grid(c(1L, 2L)))
      } else {
        list(make_returns(c(1L, 2L)))
      }
    }
  )

  download_factor_library_ids(1L)
  download_factor_library_ids(2L)

  expect_equal(n_grid_downloads, 1L)
})

# ── download_factor_library_ids ──────────────────────

test_that("factor_library_file names the 1,000-id file of each id", {
  expect_equal(
    factor_library_file(c(1L, 1000L, 1001L, 4105728L)),
    c(
      "id_0000001-0001000.parquet",
      "id_0000001-0001000.parquet",
      "id_0001001-0002000.parquet",
      "id_4105001-4106000.parquet"
    )
  )
})

test_that("aborts when no grid rows match requested ids", {
  testthat::local_mocked_bindings(
    download_factor_library_grid = function(...) make_grid(42L)
  )

  expect_error(
    download_factor_library_ids(999L),
    class = "rlang_error"
  )
})

test_that("downloads the files that hold the ids and joins grid metadata", {
  files <- list(
    "id_0000001-0001000.parquet" = tibble::tibble(
      id = c(1L, 2L),
      date = as.Date("2020-01-01"),
      ret = c(0.01, 0.02)
    ),
    "id_0001001-0002000.parquet" = tibble::tibble(
      id = 1001L,
      date = as.Date("2020-01-01"),
      ret = 0.03
    )
  )
  requested <- NULL
  testthat::local_mocked_bindings(
    download_factor_library_grid = function(...) make_grid(c(1L, 1001L)),
    read_factor_library_files = function(dataset, revision, paths) {
      requested <<- list(dataset = dataset, revision = revision, paths = paths)
      serve_files(files)(dataset, revision, paths)
    }
  )

  expect_no_warning(result <- download_factor_library_ids(c(1L, 1001L)))

  expect_equal(
    requested,
    list(dataset = "factor-library", revision = "main", paths = names(files))
  )
  expect_equal(result$id, c(1L, 1001L))
  expect_equal(result$ret, c(0.01, 0.03))
  expect_true("weighting_scheme" %in% names(result))
})

test_that("reads the returns and the grid at their revisions", {
  revisions <- list()
  testthat::local_mocked_bindings(
    download_factor_library_grid = function(grid_revision) {
      revisions$grid <<- grid_revision
      make_grid(1L)
    },
    read_factor_library_files = function(dataset, revision, paths) {
      revisions$returns <<- revision
      list(make_returns(1L))
    }
  )

  download_factor_library_ids(1L, revision = "abc", grid_revision = "def")

  expect_equal(revisions, list(grid = "def", returns = "abc"))
})

test_that("skips a file missing on the Hub and warns about its ids", {
  # The Hub has no file for the range of id 1001, as when none of the
  # portfolio sorts in the range produced portfolios
  files <- list("id_0000001-0001000.parquet" = make_returns(1L))
  requested <- character(0)
  testthat::local_mocked_bindings(
    download_factor_library_grid = function(...) make_grid(c(1L, 1001L)),
    read_factor_library_files = function(dataset, revision, paths) {
      requested <<- paths
      serve_files(files)(dataset, revision, paths)
    }
  )

  expect_warning(
    result <- download_factor_library_ids(c(1L, 1001L)),
    "without returns .* absent from the result: 1001\\.$"
  )
  expect_equal(
    requested,
    c("id_0000001-0001000.parquet", "id_0001001-0002000.parquet")
  )
  expect_equal(result$id, 1L)
})

test_that("warns about ids without rows in the file that holds them", {
  # Of ids 1 to 8, only id 1 has rows; the file also holds id 9, which was
  # not requested
  files <- list(
    "id_0000001-0001000.parquet" = make_returns(c(1L, 9L), c(0.01, 0.09))
  )
  testthat::local_mocked_bindings(
    download_factor_library_grid = function(...) make_grid(1:9),
    read_factor_library_files = serve_files(files)
  )

  # The warning lists the first five ids and counts them all
  expect_warning(
    result <- download_factor_library_ids(1:8),
    "absent from the result: 2, 3, 4, 5, 6, \\.\\.\\. \\(7 IDs\\)\\.$"
  )
  expect_equal(result$id, 1L)
  expect_equal(result$ret, 0.01)
})

test_that("returns no rows but all columns when no id has returns", {
  testthat::local_mocked_bindings(
    download_factor_library_grid = function(...) make_grid(c(1L, 1001L)),
    read_factor_library_files = serve_files(list())
  )

  expect_warning(
    result <- download_factor_library_ids(c(1L, 1001L)),
    "absent from the result: 1, 1001\\.$"
  )
  expect_equal(nrow(result), 0L)
  expect_named(result, c("id", "date", "ret", names(make_grid())[-1]))
  expect_type(result$id, "integer")
  expect_s3_class(result$date, "Date")
  expect_type(result$ret, "double")
})

# ── download_factor_library_table (costs) ────────────

test_that("costs: reads the files of the same names in costs/", {
  files <- list(
    "costs/id_0000001-0001000.parquet" = make_costs(c(1L, 2L), c(0.01, 0.02)),
    "costs/id_0001001-0002000.parquet" = make_costs(1001L, 0.03)
  )
  requested <- NULL
  testthat::local_mocked_bindings(
    download_factor_library_grid = function(...) make_grid(c(1L, 1001L)),
    read_factor_library_files = function(dataset, revision, paths) {
      requested <<- list(dataset = dataset, revision = revision, paths = paths)
      serve_files(files)(dataset, revision, paths)
    }
  )

  expect_no_warning(
    result <- download_factor_library_table(
      c(1L, 1001L),
      "costs",
      revision = "abc",
      grid_revision = "def"
    )
  )

  expect_equal(
    requested,
    list(dataset = "factor-library", revision = "abc", paths = names(files))
  )
  expect_named(
    result,
    c(
      "id", "date", "turnover", "cost", "microcap_share",
      names(make_grid())[-1]
    )
  )
  expect_equal(result$id, c(1L, 1001L))
  expect_equal(result$cost, c(0.01, 0.03))
})

test_that("costs: warns about ids without costs", {
  testthat::local_mocked_bindings(
    download_factor_library_grid = function(...) make_grid(c(1L, 1001L)),
    read_factor_library_files = serve_files(list(
      "costs/id_0000001-0001000.parquet" = make_costs(1L)
    ))
  )

  expect_warning(
    result <- download_factor_library_table(c(1L, 1001L), "costs", "a", "b"),
    "without costs .* absent from the result: 1001\\.$"
  )
  expect_equal(result$id, 1L)
})

test_that("costs: returns no rows but all columns when no id has costs", {
  testthat::local_mocked_bindings(
    download_factor_library_grid = function(...) make_grid(1L),
    read_factor_library_files = serve_files(list())
  )

  expect_warning(
    result <- download_factor_library_table(1L, "costs", "a", "b"),
    "without costs"
  )
  expect_equal(nrow(result), 0L)
  expect_named(
    result,
    c(
      "id", "date", "turnover", "cost", "microcap_share",
      names(make_grid())[-1]
    )
  )
  expect_type(result$id, "integer")
  expect_s3_class(result$date, "Date")
  expect_type(result$microcap_share, "double")
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

test_that("with ids: delegates to download_factor_library_table", {
  mock_result <- tibble::tibble(id = 1L, ret = 0.01)
  testthat::local_mocked_bindings(
    validate_dates = function(start_date, end_date) {
      list(start_date = NULL, end_date = NULL)
    },
    download_factor_library_table = function(...) mock_result
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
    download_factor_library_table = function(...) mock_result
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
    download_factor_library_table = function(...) mock_returns
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
    download_factor_library_table = function(...) mock_returns
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
    download_factor_library_table = function(...) mock_returns
  )

  result <- download_data_hugging_face_factor_library(
    sorting_variable = "me",
    start_date = "2020-01-01",
    end_date = "2020-12-31"
  )

  expect_equal(result$date, as.Date("2020-06-30"))
})

test_that("forwards the table, the revisions, and the grid filters", {
  captured <- list()
  testthat::local_mocked_bindings(
    validate_dates = function(start_date, end_date) {
      list(start_date = NULL, end_date = NULL)
    },
    filter_factor_library_grid = function(..., fill_all, grid_revision) {
      captured$filter <<- list(..., fill_all = fill_all, grid = grid_revision)
      1L
    },
    download_factor_library_table = function(ids, table, revision,
                                             grid_revision) {
      captured$table <<- list(ids, table, revision, grid_revision)
      make_costs(ids)
    }
  )

  download_data_hugging_face_factor_library(
    sorting_variable = "bm",
    fill_all = TRUE,
    table = "costs",
    revision = "abc",
    grid_revision = "def"
  )

  expect_equal(
    captured$filter,
    list(sorting_variable = "bm", fill_all = TRUE, grid = "def")
  )
  expect_equal(captured$table, list(1L, "costs", "abc", "def"))
})

# ── download_factor_library_persistence ──────────────

test_that("persistence: reads the file of the grid dataset", {
  requested <- NULL
  testthat::local_mocked_bindings(
    read_factor_library_files = function(dataset, revision, paths) {
      requested <<- list(dataset = dataset, revision = revision, paths = paths)
      list(make_persistence())
    }
  )

  expect_message(
    result <- download_factor_library_persistence(grid_revision = "def"),
    "full data set"
  )

  expect_equal(
    requested,
    list(
      dataset = "factor-library-grid",
      revision = "def",
      paths = "sorting_variable_persistence.parquet"
    )
  )
  expect_equal(result, make_persistence())
})

test_that("persistence: filters by sorting variable and dates", {
  testthat::local_mocked_bindings(
    read_factor_library_files = function(...) list(make_persistence())
  )

  result <- download_factor_library_persistence(
    sorting_variable = c("bm", "size"),
    start_date = "2020-01-01",
    end_date = "2020-12-31"
  )

  expect_equal(result$sorting_variable, c("bm", "size"))
  expect_equal(result$persistence, c(0.8, 0.7))

  result <- suppressMessages(
    download_factor_library_persistence(sorting_variable = "bm")
  )
  expect_equal(result$persistence, c(0.9, 0.8))
})

test_that("persistence: aborts for unknown sorting variables", {
  testthat::local_mocked_bindings(
    read_factor_library_files = function(...) list(make_persistence())
  )

  expect_error(
    suppressMessages(
      download_factor_library_persistence(sorting_variable = c("bm", "xyz"))
    ),
    "xyz"
  )
})

test_that("persistence: aborts on an invalid revision", {
  expect_error(
    download_factor_library_persistence(grid_revision = NULL),
    "grid_revision"
  )
})

# ── Hugging Face files ───────────────────────────────

test_that("factor_library_url resolves paths at a revision", {
  expect_equal(
    factor_library_url(
      "factor-library",
      "e47bd85accd9b9b168ac7f478f12fe89ba5c2430",
      c("id_0000001-0001000.parquet", "costs/id_0000001-0001000.parquet")
    ),
    paste0(
      "https://huggingface.co/datasets/tidy-finance/factor-library/resolve/",
      "e47bd85accd9b9b168ac7f478f12fe89ba5c2430/",
      c("id_0000001-0001000.parquet", "costs/id_0000001-0001000.parquet")
    )
  )
  expect_equal(
    factor_library_url("factor-library-grid", "main", "grid.parquet"),
    paste0(
      "https://huggingface.co/datasets/tidy-finance/factor-library-grid/",
      "resolve/main/grid.parquet"
    )
  )
  # A branch with slashes is encoded as one segment
  expect_equal(
    factor_library_url("factor-library", "refs/pr/1", "README.md"),
    paste0(
      "https://huggingface.co/datasets/tidy-finance/factor-library/resolve/",
      "refs%2Fpr%2F1/README.md"
    )
  )
})

test_that("reads the files in order and skips missing files", {
  requested <- character(0)
  httr2::local_mocked_responses(function(req) {
    requested <<- c(requested, req$url)
    if (grepl("missing", req$url)) {
      httr2::response(404L, headers = list(`X-Error-Code` = "EntryNotFound"))
    } else {
      id <- as.integer(sub(".*/([0-9]+)\\.parquet$", "\\1", req$url))
      httr2::response(200L, body = parquet_body(make_returns(id)))
    }
  })

  result <- read_factor_library_files(
    "factor-library",
    "abc",
    c("2.parquet", "missing.parquet", "1.parquet")
  )

  expect_length(result, 3)
  expect_equal(result[[1]]$id, 2L)
  expect_null(result[[2]])
  expect_equal(result[[3]]$id, 1L)
  expect_s3_class(result[[1]]$date, "Date")
  expect_setequal(
    requested,
    factor_library_url(
      "factor-library",
      "abc",
      c("2.parquet", "missing.parquet", "1.parquet")
    )
  )
})

test_that("aborts on a revision that the dataset does not have", {
  httr2::local_mocked_responses(function(req) {
    httr2::response(404L, headers = list(`X-Error-Code` = "RevisionNotFound"))
  })

  expect_error(
    read_factor_library_files("factor-library", "nope", "a.parquet"),
    "Revision .*nope.* is not in"
  )
})

test_that("raises HTTP errors other than a missing file", {
  httr2::local_mocked_responses(function(req) httr2::response(503L))

  expect_error(
    read_factor_library_files("factor-library", "main", "a.parquet"),
    class = "httr2_http_503"
  )
})

test_that("retries files refused by the rate limit after it resets", {
  n_requests <- 0L
  waited <- numeric(0)
  httr2::local_mocked_responses(function(req) {
    n_requests <<- n_requests + 1L
    # The second of two files is refused once
    if (grepl("2\\.parquet$", req$url) && length(waited) == 0) {
      rate_limited(57)
    } else {
      httr2::response(200L, body = parquet_body(make_returns(1L)))
    }
  })
  testthat::local_mocked_bindings(
    wait_for_rate_limit = function(seconds) waited <<- c(waited, seconds)
  )

  result <- read_factor_library_files(
    "factor-library",
    "main",
    c("1.parquet", "2.parquet")
  )

  # Only the refused file is requested again, one second after the reset
  expect_equal(n_requests, 3L)
  expect_equal(waited, 58)
  expect_equal(result[[2]]$id, 1L)
})

test_that("aborts when the rate limit refuses a file three times", {
  waited <- numeric(0)
  httr2::local_mocked_responses(function(req) rate_limited(120))
  testthat::local_mocked_bindings(
    wait_for_rate_limit = function(seconds) waited <<- c(waited, seconds)
  )

  expect_error(
    read_factor_library_files("factor-library", "main", "a.parquet"),
    "3,000 files per five minutes"
  )
  expect_equal(waited, c(121, 121))
})

test_that("rate_limit_reset reads the RateLimit header", {
  expect_equal(rate_limit_reset(rate_limited(57)), 58)
  # Without the header, a full window of five minutes resets the limit
  expect_equal(rate_limit_reset(httr2::response(429L)), 301)
})

test_that("wait_for_rate_limit says how long it waits", {
  expect_message(wait_for_rate_limit(0), "Waiting 0 seconds")
})

test_that("downloads are throttled across calls", {
  skip_on_cran()
  httr2::local_mocked_responses(function(req) httr2::response(200L))
  urls <- paste0("https://huggingface.co/", seq_len(16), ".parquet")

  # 16 requests may start at once, then 8 per second, so the second call
  # waits for about two seconds
  started <- Sys.time()
  perform_factor_library_requests(urls)
  perform_factor_library_requests(urls)
  elapsed <- as.numeric(difftime(Sys.time(), started, units = "secs"))

  expect_gt(elapsed, 1)
})

test_that("requests do not retry inside httr2", {
  # httr2 retries HTTP 429 of parallel requests without limit, so a request
  # must return it to read_factor_library_files()
  httr2::local_mocked_responses(function(req) rate_limited())

  resps <- perform_factor_library_requests("https://huggingface.co/a.parquet")

  expect_s3_class(resps[[1]], "httr2_http_429")
})

# ── Release of 20 October 2026 ───────────────────────

test_that("reads the costs of the pinned release", {
  skip_on_cran()
  skip_if_offline("huggingface.co")

  costs <- read_factor_library_files(
    "factor-library",
    "e47bd85accd9b9b168ac7f478f12fe89ba5c2430",
    "costs/id_0000001-0001000.parquet"
  )[[1]]

  expect_named(costs, c("id", "date", "turnover", "cost", "microcap_share"))
  expect_type(costs$id, "integer")
  expect_s3_class(costs$date, "Date")
  expect_equal(min(costs$date), as.Date("1994-01-01"))
})

test_that("reads the persistence of the pinned release", {
  skip_on_cran()
  skip_if_offline("huggingface.co")

  persistence <- suppressMessages(download_factor_library_persistence(
    sorting_variable = "bm",
    grid_revision = "a5a74cdf1df0c2c3d1ce3d7bf6403a8386fbf99d"
  ))

  expect_named(
    persistence,
    c(
      "sorting_variable", "sorting_variable_lag", "rebalancing", "date",
      "persistence"
    )
  )
  expect_equal(unique(persistence$sorting_variable), "bm")
  expect_setequal(
    unique(persistence$sorting_variable_lag),
    c("1m", "3m", "6m", "ff")
  )
})
