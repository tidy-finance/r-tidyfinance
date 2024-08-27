#' List Chapters of Tidy Finance
#'
#' Returns a character vector containing the names of the chapters available in
#' the Tidy Finance resource. This function provides a quick reference to the
#' various topics covered.
#'
#' @returns A character vector where each element is the name of a chapter
#'   available in the Tidy Finance resource. These names correspond to specific
#'   chapters in Tidy Finance with R.
#'
#' @export
#'
#' @examples
#' list_tidy_finance_chapters()
#'
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
