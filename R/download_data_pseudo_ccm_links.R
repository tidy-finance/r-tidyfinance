#' Generate Pseudo CCM Links
#'
#' Returns a pseudo CRSP-Compustat linking table with the same column
#' layout as [download_data_wrds_ccm_links()]. Every pseudo `permno` is
#' linked to its corresponding `gvkey` for the full sample horizon.
#'
#' @param n_assets Integer. Number of pseudo firms in the universe.
#'   Defaults to `1000`.
#' @param seed Integer. Random seed controlling the pseudo identifier
#'   universe; defaults to `1234`.
#' @param linktype Accepted for API compatibility with
#'   [download_data_wrds_ccm_links()]; ignored for pseudo data.
#' @param linkprim Accepted for API compatibility with
#'   [download_data_wrds_ccm_links()]; ignored for pseudo data.
#'
#' @returns A tibble with columns `permno`, `gvkey`, `linkdt`, and `linkenddt`,
#'   one row per pseudo firm.
#'
#' @family pseudo functions
#' @export
#'
#' @examples
#' download_data_pseudo_ccm_links(n_assets = 10)
download_data_pseudo_ccm_links <- function(
  n_assets = 1000L,
  seed = 1234L,
  linktype = c("LU", "LC"),
  linkprim = c("P", "C")
) {
  identifiers <- simulate_pseudo_identifiers(n_assets = n_assets, seed = seed)

  identifiers |>
    mutate(
      linkdt = as.Date("1925-12-31"),
      linkenddt = as.Date("2099-12-31")
    ) |>
    select("permno", "gvkey", "linkdt", "linkenddt")
}
