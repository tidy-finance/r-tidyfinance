test_that("download_data_constituents works for all supported indexes", {
  supported_indexes <- list_supported_indexes()

  for (j in 1:nrow(supported_indexes)) {
    index <- supported_indexes$index[j]

    expect_silent({
      constituents_data <- download_data_constituents(index)
    })

    # Test that the tibble has the expected columns
    expect_true(all(c("symbol", "name") %in% colnames(constituents_data)))

    # Test that there are no missing values in the required columns
    expect_true(all(!is.na(constituents_data$symbol)))
    expect_true(all(!is.na(constituents_data$name)))

    # Test that the tibble is not empty
    expect_gt(nrow(constituents_data), 0)
  }
})
