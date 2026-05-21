test_that("supported type passes silently", {
  supported <- list_supported_datasets(as_vector = TRUE)[[1]]
  expect_no_error(check_supported_type(supported))
})

test_that("unsupported type stops with an informative error", {
  expect_error(
    check_supported_type("not_a_valid_dataset_type"),
    "Unsupported"
  )
})
