# get_wrds_connection explains failed connections

    Code
      get_wrds_connection()
    Condition
      Error in `get_wrds_connection()`:
      ! Could not connect to WRDS.
      i Check that `WRDS_USER` and `WRDS_PASSWORD` are correct, e.g., with `set_wrds_credentials()`.
      i WRDS requires two-factor authentication (Duo): enroll your account and approve the push notification when you connect.
      i Log in to <https://wrds-www.wharton.upenn.edu> once and accept the terms of use if WRDS asks you to.
      i See `?tidyfinance::get_wrds_connection` for details.
      Caused by error in `DBI::dbConnect()`:
      ! connection refused

