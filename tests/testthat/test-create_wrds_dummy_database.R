test_that("create_wrds_dummy_database downloads database to the specified path", {
  skip_if_offline()
  skip_on_cran()
  temp_path <- tempfile(fileext = ".sqlite")
  create_wrds_dummy_database(path = temp_path)
  expect_true(file.exists(temp_path))
  unlink(temp_path)
})

test_that("create_wrds_dummy_database handles broken url", {
  skip_if_offline()
  skip_on_cran()
  temp_path <- tempfile(fileext = ".sqlite")
  expect_message(
    create_wrds_dummy_database(path = temp_path, url = "broken"),
    "The resource may not be available, or the URL may have changed."
  )
  unlink(temp_path)
})
