test_that("validate_column_name accepts a single string", {
  expect_no_error(validate_column_name("permno", "id", "entity"))
  expect_null(validate_column_name("permno", "id", "entity"))
})

test_that("validate_column_name rejects non-strings and non-scalars", {
  expect_error(validate_column_name(123, "id", "entity"), "id")
  expect_error(validate_column_name(c("a", "b"), "id", "entity"), "id")
  expect_error(validate_column_name(NULL, "id", "entity"), "id")
  expect_error(validate_column_name(TRUE, "id", "entity"), "id")
})

test_that("validate_flag accepts a single non-NA logical", {
  expect_no_error(validate_flag(TRUE, "flag"))
  expect_no_error(validate_flag(FALSE, "flag"))
})

test_that("validate_flag rejects non-logical, NA, and length != 1", {
  expect_error(validate_flag("yes", "flag"), "flag")
  expect_error(validate_flag(1, "flag"), "flag")
  expect_error(validate_flag(NA, "flag"), "flag")
  expect_error(validate_flag(c(TRUE, FALSE), "flag"), "flag")
})

test_that("validate_flag supports a custom message", {
  expect_error(
    validate_flag(NA, "flag", "{.arg flag} must be TRUE or FALSE."),
    "TRUE or FALSE"
  )
})

test_that("validate_optional_number passes for NULL", {
  expect_no_error(validate_optional_number(NULL, "msg"))
  expect_null(validate_optional_number(NULL, "msg"))
})

test_that("validate_optional_number enforces strict lower bound", {
  expect_no_error(
    validate_optional_number(0.5, "msg", min = 0, min_strict = TRUE)
  )
  expect_error(
    validate_optional_number(0, "msg", min = 0, min_strict = TRUE),
    "msg"
  )
})

test_that("validate_optional_number enforces an exclusive 0-1 range", {
  in_unit <- function(value) {
    validate_optional_number(
      value, "msg",
      min = 0, max = 1, min_strict = TRUE, max_strict = TRUE
    )
  }
  expect_no_error(in_unit(0.5))
  expect_error(in_unit(0), "msg")
  expect_error(in_unit(1), "msg")
})

test_that("validate_optional_number rejects NA, non-numeric, and length != 1", {
  expect_error(validate_optional_number(NA_real_, "msg"), "msg")
  expect_error(validate_optional_number("a", "msg"), "msg")
  expect_error(validate_optional_number(c(0.1, 0.2), "msg"), "msg")
})

test_that("validate_optional_number allows an inclusive non-negative bound", {
  expect_no_error(validate_optional_number(0, "msg", min = 0))
  expect_error(validate_optional_number(-1, "msg", min = 0), "msg")
})
