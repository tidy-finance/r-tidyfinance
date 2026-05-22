test_that("get_random_user_agent returns first mocked agent", {
  local_mocked_bindings(
    sample = function(x, size) {
      expect_identical(x, 20L)
      expect_identical(size, 1)
      1L
    },
    .package = "base"
  )

  user_agent <- get_random_user_agent()

  expect_type(user_agent, "character")
  expect_length(user_agent, 1)
  expect_match(user_agent, "Windows NT 10\\.0")
  expect_match(user_agent, "Chrome/122\\.0\\.6261\\.111")
})

test_that("get_random_user_agent returns last mocked agent", {
  local_mocked_bindings(
    sample = function(x, size) {
      expect_identical(x, 20L)
      expect_identical(size, 1)
      20L
    },
    .package = "base"
  )

  user_agent <- get_random_user_agent()

  expect_type(user_agent, "character")
  expect_length(user_agent, 1)
  expect_match(user_agent, "Firefox/125\\.0")
})
