#' Open Tidy Finance Website or Specific Chapter in Browser
#'
#' Opens the main Tidy Finance website or a specific chapter within the site
#' in the user's default web browser. If a chapter is specified, the function
#' constructs the URL to access the chapter directly.
#'
#' @param chapter Optional. A character string specifying the chapter to open.
#' If NULL (the default), the function opens the main page of the Tidy Finance
#' website. If a chapter name is provided (e.g., "beta-estimation"), the
#' function opens the corresponding chapter page (e.g., "beta-estimation.html").
#'
#' @examples
#' open_tidy_finance_website()
#' open_tidy_finance_website("beta-estimation")
#'
#' @export
#'
#' @return Invisible NULL. The function is called for its side effect of opening a web page.
open_tidy_finance_website <- function(chapter = NULL) {
  base_url <- "https://www.tidy-finance.org/r/"
  if (!is.null(chapter)) {
    final_url <- paste0(base_url, chapter, ".html")
  } else {
    final_url <- base_url
  }
  browseURL(final_url)
  invisible(NULL)
}

#' List Chapters of Tidy Finance
#'
#' Returns a character vector containing the names of the chapters available in
#' the Tidy Finance resource. This function provides a quick reference to the
#' various topics covered.
#'
#' @examples
#' list_tidy_finance_chapters()
#'
#' @export
#'
#' @return A character vector where each element is the name of a chapter
#' available in the Tidy Finance resource. These names correspond to specific
#' chapter in Tidy Finance with R.
list_tidy_finance_chapters <- function() {
  c(
    "setting-up-your-environment",
    "introduction-to-tidy-finance",
    "accessing-and-managing-financial-data",
    "wrds-crsp-and-compustat",
    "trace-and-fisd",
    "other-data-providers",
    "beta-estimation",
    "univariate-portfolio-sorts",
    "size-sorts-and-p-hacking",
    "value-and-bivariate-sorts",
    "replicating-fama-and-french-factors",
    "fama-macbeth-regressions",
    "fixed-effects-and-clustered-standard-errors",
    "difference-in-differences",
    "factor-selection-via-machine-learning",
    "option-pricing-via-machine-learning",
    "parametric-portfolio-policies",
    "constrained-optimization-and-backtesting",
    "wrds-dummy-data",
    "cover-and-logo-design",
    "clean-enhanced-trace-with-r",
    "proofs",
    "hex-sticker",
    "changelog"
  )
}
