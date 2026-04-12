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
