test_that("get_wrds_connection informs when credentials are missing", {
  withr::local_envvar(WRDS_USER = "", WRDS_PASSWORD = "")
  local_mocked_bindings(
    Postgres = function() "pg_driver",
    .package = "RPostgres"
  )
  local_mocked_bindings(
    dbConnect = function(...) structure(list(), class = "DBIConnection"),
    .package = "DBI"
  )
  expect_message(
    get_wrds_connection(),
    "WRDS credentials"
  )
})

local_wrds_mocks <- function(db_connect, env = parent.frame()) {
  withr::local_envvar(
    WRDS_USER = "user",
    WRDS_PASSWORD = "pass",
    .local_envir = env
  )
  testthat::local_mocked_bindings(
    Postgres = function() "pg_driver",
    .package = "RPostgres",
    .env = env
  )
  testthat::local_mocked_bindings(
    dbConnect = db_connect,
    .package = "DBI",
    .env = env
  )
}

test_that("get_wrds_connection reports connecting and success", {
  fake_con <- structure(list(), class = "DBIConnection")
  local_wrds_mocks(function(...) fake_con)
  rlang::local_options(cli.dynamic = FALSE)
  msgs <- cli::ansi_strip(paste(
    testthat::capture_messages(result <- get_wrds_connection()),
    collapse = ""
  ))
  expect_match(msgs, "Connecting to WRDS as \"user\"")
  expect_match(msgs, "Approve the Duo request")
  expect_match(msgs, "Connected to WRDS as \"user\"")
  expect_identical(result, fake_con)
})

test_that("get_wrds_connection is silent with quiet = TRUE", {
  fake_con <- structure(list(), class = "DBIConnection")
  local_wrds_mocks(function(...) fake_con)
  expect_no_message(result <- get_wrds_connection(quiet = TRUE))
  expect_identical(result, fake_con)

  withr::local_envvar(WRDS_USER = "", WRDS_PASSWORD = "")
  expect_no_message(get_wrds_connection(quiet = TRUE))
})

test_that("get_wrds_connection validates quiet", {
  expect_error(get_wrds_connection(quiet = NA), "quiet")
})

test_that("get_wrds_connection explains failed connections", {
  local_wrds_mocks(function(...) stop("connection refused"))
  err <- expect_error(
    get_wrds_connection(quiet = TRUE),
    "Could not connect to WRDS"
  )
  expect_match(conditionMessage(err$parent), "connection refused")
  msg <- cli::ansi_strip(conditionMessage(err))
  expect_match(msg, "two-factor authentication")
  expect_match(msg, "terms of use")
  expect_snapshot(get_wrds_connection(quiet = TRUE), error = TRUE)
})

test_that("get_wrds_connection marks the connection step as failed", {
  local_wrds_mocks(function(...) stop("connection refused"))
  rlang::local_options(cli.dynamic = FALSE)
  msgs <- character()
  withCallingHandlers(
    try(get_wrds_connection(), silent = TRUE),
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  expect_match(
    cli::ansi_strip(paste(msgs, collapse = "")),
    "Connection to WRDS failed"
  )
})
