test_that("download_data_constituents works for a supported index", {
  skip_if_offline()
  skip_on_cran()
  index <- "Dow Jones Industrial Average"

  expect_silent({
    constituents_data <- download_data_constituents(index)
  })

  expect_true(all(c("symbol", "name") %in% colnames(constituents_data)))

  expect_true(all(!is.na(constituents_data$symbol)))
  expect_true(all(!is.na(constituents_data$name)))

})

test_that("download_data_constituents handles unavailable resource gracefully", {
  mock_list_supported_indexes <- function() {
    tibble::tibble(
      index = c("SAMPLE_INDEX"),
      url = c("http://nonexistent-url.example.com"),
      skip = c(0)
    )
  }

  with_mocked_bindings(
    list_supported_indexes = mock_list_supported_indexes,
    {
      expect_message(
        download_data_constituents(index = "SAMPLE_INDEX"),
        regex = "The resource may not be available, or the URL may have changed"
      )
    }
  )
})
