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

test_that(
  paste0(
    "get_wrds_connection returns connection silently when credentials are set"
  ),
  {
    withr::local_envvar(WRDS_USER = "user", WRDS_PASSWORD = "pass")
    fake_con <- structure(list(), class = "DBIConnection")
    local_mocked_bindings(
      Postgres = function() "pg_driver",
      .package = "RPostgres"
    )
    local_mocked_bindings(
      dbConnect = function(...) fake_con,
      .package = "DBI"
    )
    expect_no_message(result <- get_wrds_connection())
    expect_identical(result, fake_con)
  }
)

test_that("get_wrds_connection explains failed connections", {
  withr::local_envvar(WRDS_USER = "user", WRDS_PASSWORD = "pass")
  local_mocked_bindings(
    Postgres = function() "pg_driver",
    .package = "RPostgres"
  )
  local_mocked_bindings(
    dbConnect = function(...) stop("connection refused"),
    .package = "DBI"
  )
  err <- expect_error(get_wrds_connection(), "Could not connect to WRDS")
  expect_match(conditionMessage(err$parent), "connection refused")
  msg <- cli::ansi_strip(conditionMessage(err))
  expect_match(msg, "two-factor authentication")
  expect_match(msg, "terms of use")
  expect_snapshot(get_wrds_connection(), error = TRUE)
})
