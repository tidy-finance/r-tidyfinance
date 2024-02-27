test_that("File is downloaded to the specified path", {
  temp_path <- tempfile(fileext = ".sqlite")
  create_wrds_dummy_database(path = temp_path)
  expect_true(file.exists(temp_path))
  unlink(temp_path)
})
