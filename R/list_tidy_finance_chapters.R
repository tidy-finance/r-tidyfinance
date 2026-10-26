#' List Chapters of Tidy Finance
#'
#' Returns a character vector containing the names of the chapters available in
#' the Tidy Finance resource. It provides a quick reference to the
#' various topics covered.
#'
#' @returns A character vector where each element is the name of a chapter
#'   available in the Tidy Finance resource, in the order of the website. These
#'   names correspond to the chapter pages at
#'   `https://www.tidy-finance.org/chapters/`.
#'
#' @family utility functions
#' @export
#'
#' @examples
#' list_tidy_finance_chapters()
#'
list_tidy_finance_chapters <- function() {
  c(
    "setting-up-your-environment",
    "the-tidyfinance-package",
    "working-with-stock-returns",
    "modern-portfolio-theory",
    "capital-asset-pricing-model",
    "financial-statement-analysis",
    "discounted-cash-flow-analysis",
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
    "clean-enhanced-trace",
    "proofs",
    "colophon"
  )
}
