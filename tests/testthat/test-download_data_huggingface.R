# Helpers ---------------------------------------------------------------

make_grid_parquet_file <- function(rows) {
  tmp <- tempfile(fileext = ".parquet")
  arrow::write_parquet(rows, tmp)
  tibble::tibble(path = "grid/part.parquet", size = file.size(tmp), url = tmp)
}

make_returns_parquet_file <- function(rows, sv, sv_lag) {
  tmp <- tempfile(fileext = ".parquet")
  arrow::write_parquet(rows, tmp)
  tibble::tibble(
    path = paste0(
      "sorting_variable=",
      sv,
      "/sorting_variable_lag=",
      sv_lag,
      "/part.parquet"
    ),
    size = file.size(tmp),
    url = tmp
  )
}

# Input validation (no network) -----------------------------------------

test_that(
  paste(
    "filter_grid() aborts with a clear message on unsupported filter names"
  ),
  {
    expect_error(
      download_data_huggingface("factor_library", not_a_column = "x"),
      regexp = "unsupported filter name"
    )
  }
)

# Empty / missing URL behaviour (mocked) --------------------------------

test_that(
  paste(
    "download_factor_library_returns_ids() aborts when no IDs match the grid"
  ),
  {
    grid_rows <- tibble::tibble(
      id = 1L,
      sorting_variable = "sv_me",
      sorting_variable_lag = "3m"
    )

    mock_files <- function(organization, dataset) {
      if (dataset == "factor-library-grid") {
        make_grid_parquet_file(grid_rows)
      } else {
        make_returns_parquet_file(tibble::tibble(id = 1L), "me", "3m")
      }
    }

    with_mocked_bindings(
      get_available_huggingface_files = mock_files,
      {
        expect_error(
          download_factor_library_ids(integer(0)),
          regexp = "No parquet files found for the requested portfolio IDs"
        )
      }
    )
  }
)

test_that(
  paste(
    "download_factor_library_returns_ids() aborts when a parquet URL",
    "is missing for a portfolio ID"
  ),
  {
    grid_rows <- tibble::tibble(
      id = 1L,
      sorting_variable = "sv_me",
      sorting_variable_lag = "3m"
    )

    mock_files <- function(organization, dataset) {
      if (dataset == "factor-library-grid") {
        make_grid_parquet_file(grid_rows)
      } else {
        tibble::tibble(
          path = character(0),
          size = numeric(0),
          url = character(0)
        )
      }
    }

    with_mocked_bindings(
      get_available_huggingface_files = mock_files,
      {
        expect_error(
          download_factor_library_ids(1L),
          regexp = "No parquet file found for 1 portfolio ID"
        )
      }
    )
  }
)

# NULL default / explicit NULL behaviour (mocked) ----------------------

test_that(
  paste(
    "filter_factor_library_grid(): omitted breakpoints_min_size defaults to",
    "NA (standard portfolio); explicit NULL removes the filter"
  ),
  {
    grid_rows <- tibble::tibble(
      id = c(1L, 2L),
      sorting_variable = c("sv_bm", "sv_bm"),
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
      breakpoints_min_size = c(NA_real_, 1e9),
      weighting_scheme = c("VW", "VW")
    )

    mock_files <- function(organization, dataset) {
      make_grid_parquet_file(grid_rows)
    }

    with_mocked_bindings(
      get_available_huggingface_files = mock_files,
      {
        ids_default <- filter_factor_library_grid(sorting_variable = "bm")
        ids_explicit_null <- filter_factor_library_grid(
          sorting_variable = "bm",
          breakpoints_min_size = NULL
        )

        expect_length(ids_default, 1L)
        expect_length(ids_explicit_null, 2L)
      }
    )
  }
)

# Successful download (mocked) ------------------------------------------

test_that(
  paste(
    "download_data_huggingface() returns a tibble for factor_library with",
    "matched IDs"
  ),
  {
    grid_rows <- tibble::tibble(
      id = 1L,
      sorting_variable = "sv_me",
      sorting_variable_lag = "3m"
    )
    returns_rows <- tibble::tibble(id = 1L, ret = 0.01)

    mock_files <- function(organization, dataset) {
      if (dataset == "factor-library-grid") {
        make_grid_parquet_file(grid_rows)
      } else {
        make_returns_parquet_file(returns_rows, "me", "3m")
      }
    }

    with_mocked_bindings(
      get_available_huggingface_files = mock_files,
      {
        result <- download_data_huggingface(
          "factor_library",
          sorting_variable = "me",
          fill_all = TRUE
        )
        expect_s3_class(result, "tbl_df")
        expect_true(nrow(result) > 0)
        expect_true("id" %in% colnames(result))
      }
    )
  }
)

# Live smoke test -------------------------------------------------------

test_that(
  paste(
    "download_data_huggingface() downloads factor_library data successfully"
  ),
  {
    skip_if_offline()
    skip_on_cran()

    result <- download_data_huggingface(
      "factor_library",
      sorting_variable = "me"
    )

    expect_s3_class(result, "tbl_df")
    expect_true(nrow(result) > 0)
    expect_true("id" %in% colnames(result))
  }
)
