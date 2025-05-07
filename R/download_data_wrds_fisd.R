#' Download Filtered FISD Data from WRDS
#'
#' Establishes a connection to the WRDS database to download a filtered subset
#' of the FISD (Fixed Income Securities Database). The function filters the
#' `fisd_mergedissue` and `fisd_mergedissuer` tables based on several criteria
#' related to the securities, such as security level, bond type, coupon type,
#' and others, focusing on specific attributes that denote the nature of the
#' securities. It finally returns a data frame with selected fields from the
#' `fisd_mergedissue` table after joining it with issuer information from the
#' `fisd_mergedissuer` table for issuers domiciled in the USA.
#'
#' @param additional_columns Additional columns from the FISD table
#'   as a character vector.
#'
#' @returns A data frame containing a subset of FISD data with fields related to
#'   the bond's characteristics and issuer information. This includes complete
#'   CUSIP, maturity date, offering amount, offering date, dated date, interest
#'   frequency, coupon, last interest date, issue ID, issuer ID, SIC code of the
#'   issuer.
#'
#' @export
#' @examples
#' \dontrun{
#'   fisd <- download_data_wrds_fisd()
#'   fisd_extended <- download_data_wrds_fisd(additional_columns = c("asset_backed", "defeased"))
#' }
download_data_wrds_fisd <- function(additional_columns = NULL) {
  rlang::check_installed(
    "dbplyr",
    reason = "to download type fisdmergedissue."
  )

  con <- get_wrds_connection()

  fisd_mergedissue_db <- tbl(con, I("fisd.fisd_mergedissue"))

  fisd <- fisd_mergedissue_db |>
    filter(
      security_level == "SEN",
      slob == "N" | is.na(slob),
      is.na(security_pledge),
      asset_backed == "N" | is.na(asset_backed),
      defeased == "N" | is.na(defeased),
      is.na(defeased_date),
      bond_type %in% c("CDEB", "CMTN", "CMTZ", "CZ", "USBN"),
      pay_in_kind != "Y" | is.na(pay_in_kind),
      is.na(pay_in_kind_exp_date),
      yankee == "N" | is.na(yankee),
      canadian == "N" | is.na(canadian),
      foreign_currency == "N",
      coupon_type %in% c("F", "Z"),
      is.na(fix_frequency),
      coupon_change_indicator == "N",
      interest_frequency %in% c("0", "1", "2", "4", "12"),
      rule_144a == "N",
      private_placement == "N" | is.na(private_placement),
      defaulted == "N",
      is.na(filing_date),
      is.na(settlement),
      convertible == "N",
      is.na(exchange),
      putable == "N" | is.na(putable),
      unit_deal == "N" | is.na(unit_deal),
      exchangeable == "N" | is.na(exchangeable),
      perpetual == "N",
      preferred_security == "N" | is.na(preferred_security)
    ) |>
    select(
      complete_cusip,
      maturity,
      offering_amt,
      offering_date,
      dated_date,
      interest_frequency,
      coupon,
      last_interest_date,
      issue_id,
      issuer_id,
      all_of(additional_columns)
    ) |>
    collect()

  fisd_mergedissuer_db <- tbl(con, I("fisd.fisd_mergedissuer"))

  fisd_issuer <- fisd_mergedissuer_db |>
    filter(country_domicile == "USA") |>
    select(issuer_id, sic_code) |>
    collect()

  disconnection_connection(con)

  fisd <- fisd |>
    inner_join(fisd_issuer, join_by(issuer_id))

  fisd
}
