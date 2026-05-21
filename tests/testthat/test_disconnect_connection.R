test_that("disconnect_connection returns TRUE invisibly on success", {
  local_mocked_bindings(
    dbDisconnect = function(con) invisible(TRUE),
    .package = "DBI"
  )
  result <- withVisible(disconnect_connection(NULL))
  expect_true(result$value)
  expect_false(result$visible)
})

test_that("disconnect_connection propagates errors from DBI", {
  local_mocked_bindings(
    dbDisconnect = function(con) stop("disconnection failed"),
    .package = "DBI"
  )
  expect_error(
    disconnect_connection(NULL),
    "disconnection failed"
  )
})
