#' Open Tidy Finance Website or Specific Chapter in Browser
#'
#' Opens the main Tidy Finance website or a specific chapter within the site in
#' the user's default web browser. If a chapter is specified, the function
#' constructs the URL to access the chapter directly.
#'
#' @param chapter An optional character string specifying the chapter to open.
#'   If NULL (the default), the function opens the main page of Tidy Finance with R.
#'   If a chapter name is provided (e.g., "beta-estimation"), the
#'   function opens the corresponding chapter's page (e.g.,
#'   "beta-estimation.html").
#'
#' @examples
#' open_tidy_finance_website()
#' open_tidy_finance_website("beta-estimation")
#'
#' @importFrom utils browseURL
#'
#' @export
#'
#' @return Invisible NULL. The function is called for its side effect of opening
#'   a web page.
open_tidy_finance_website <- function(chapter = NULL) {
  base_url <- "https://www.tidy-finance.org/r/"
  if (!is.null(chapter)) {
    final_url <- paste0(base_url, chapter, ".html")
  } else {
    final_url <- base_url
  }
  utils::browseURL(final_url)
  invisible(NULL)
}
