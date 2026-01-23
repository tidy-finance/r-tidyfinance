#' Download CCM Links from WRDS
#'
#' This function downloads data from the WRDS CRSP/Compustat Merged (CCM) links
#' database. It allows users to specify the type of links (`linktype`) and the
#' primacy of the link (`linkprim`).
#'
#' @param linktype A character vector indicating the type of link to download.
#'   The default is `c("LU", "LC")`, where "LU" stands for "Link Up" and "LC"
#'   for "Link CRSP".
#' @param linkprim A character vector indicating the primacy of the link.
#'   Default is `c("P", "C")`, where "P" indicates primary and "C" indicates
#'   conditional links.
#'
#' @returns A data frame with the columns `permno`, `gvkey`, `linkdt`, and
#'   `linkenddt`, where `linkenddt` is the end date of the link, and missing end
#'   dates are replaced with today's date.
#'
#' @export
#' @examples
#' \dontrun{
#'   ccm_links <- download_data_wrds_ccm_links(linktype = "LU", linkprim = "P")
#' }
#'
download_data_wrds_ccm_links <- function(
  linktype = c("LU", "LC"),
  linkprim = c("P", "C")
) {
  con <- get_wrds_connection()

  ccm_linking_table_db <- tbl(con, I("crsp.ccmxpf_lnkhist"))

  ccm_links <- ccm_linking_table_db |>
    filter(
      linktype %in% c("LU", "LC") & linkprim %in% c("P", "C")
    ) |>
    select(permno = lpermno, gvkey, linkdt, linkenddt) |>
    collect() |>
    mutate(linkenddt = tidyr::replace_na(linkenddt, today()))

  disconnection_connection(con)

  ccm_links
}
