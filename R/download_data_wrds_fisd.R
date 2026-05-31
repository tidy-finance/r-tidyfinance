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
#' @returns A data frame containing a subset of FISD data with fields related
#'   to the bond's characteristics and issuer information. This includes
#'   complete CUSIP, maturity date, offering amount, offering date, dated date,
#'   interest frequency, coupon, last interest date, issue ID, issuer ID, and
#'   SIC code of the issuer.
#'
#' @family WRDS functions
#' @export
#'
#' @examples
#' \dontrun{
#' fisd <- download_data_wrds_fisd()
#' fisd_extended <- download_data_wrds_fisd(
#'   additional_columns = c("asset_backed", "defeased")
#' )
#' }
download_data_wrds_fisd <- function(additional_columns = NULL) {
  con <- get_wrds_connection()

  fisd_mergedissue_db <- tbl(con, I("fisd.fisd_mergedissue"))

  fisd <- fisd_mergedissue_db |>
    filter(
      .data[["security_level"]] == "SEN",
      .data[["slob"]] == "N" | is.na(.data[["slob"]]),
      is.na(.data[["security_pledge"]]),
      .data[["asset_backed"]] == "N" | is.na(.data[["asset_backed"]]),
      .data[["defeased"]] == "N" | is.na(.data[["defeased"]]),
      is.na(.data[["defeased_date"]]),
      .data[["bond_type"]] %in% c("CDEB", "CMTN", "CMTZ", "CZ", "USBN"),
      .data[["pay_in_kind"]] != "Y" | is.na(.data[["pay_in_kind"]]),
      is.na(.data[["pay_in_kind_exp_date"]]),
      .data[["yankee"]] == "N" | is.na(.data[["yankee"]]),
      .data[["canadian"]] == "N" | is.na(.data[["canadian"]]),
      .data[["foreign_currency"]] == "N",
      .data[["coupon_type"]] %in% c("F", "Z"),
      is.na(.data[["fix_frequency"]]),
      .data[["coupon_change_indicator"]] == "N",
      .data[["interest_frequency"]] %in% c("0", "1", "2", "4", "12"),
      .data[["rule_144a"]] == "N",
      .data[["private_placement"]] == "N" |
        is.na(.data[["private_placement"]]),
      .data[["defaulted"]] == "N",
      is.na(.data[["filing_date"]]),
      is.na(.data[["settlement"]]),
      .data[["convertible"]] == "N",
      is.na(.data[["exchange"]]),
      .data[["putable"]] == "N" | is.na(.data[["putable"]]),
      .data[["unit_deal"]] == "N" | is.na(.data[["unit_deal"]]),
      .data[["exchangeable"]] == "N" | is.na(.data[["exchangeable"]]),
      .data[["perpetual"]] == "N",
      .data[["preferred_security"]] == "N" |
        is.na(.data[["preferred_security"]])
    ) |>
    select(
      "complete_cusip",
      "maturity",
      "offering_amt",
      "offering_date",
      "dated_date",
      "interest_frequency",
      "coupon",
      "last_interest_date",
      "issue_id",
      "issuer_id",
      all_of(additional_columns)
    ) |>
    collect()

  fisd_mergedissuer_db <- tbl(con, I("fisd.fisd_mergedissuer"))

  fisd_issuer <- fisd_mergedissuer_db |>
    filter(.data[["country_domicile"]] == "USA") |>
    select("issuer_id", "sic_code") |>
    collect()

  disconnect_connection(con)

  fisd <- fisd |>
    inner_join(fisd_issuer, by = "issuer_id")

  fisd
}
