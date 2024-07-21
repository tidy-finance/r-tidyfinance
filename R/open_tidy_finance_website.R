#' Open Tidy Finance Website or Specific Chapter in Browser
#'
#' Opens the main Tidy Finance website or a specific chapter within the site in
#' the user's default web browser. If a chapter is specified, the function
#' constructs the URL to access the chapter directly.
#'
#' @param chapter An optional character string specifying the chapter to open.
#'   If `NULL` (the default), the function opens the main page of Tidy Finance with R.
#'   If a chapter name is provided (e.g., "beta-estimation"), the
#'   function opens the corresponding chapter's page (e.g.,
#'   "beta-estimation.html"). If the chapter name does not exist, then the
#'   function opens the main page.
#'
#' @returns Invisible `NULL`. The function is called for its side effect of opening
#'   a web page.
#'
#' @export
#' @examples
#' open_tidy_finance_website()
#' open_tidy_finance_website("beta-estimation")
open_tidy_finance_website <- function(chapter = NULL) {
  base_url <- "https://www.tidy-finance.org/r/"
  if (!is.null(chapter)) {
    tidy_finance_chapters <- list_tidy_finance_chapters()
    if (chapter %in% tidy_finance_chapters) {
      final_url <- paste0(base_url, chapter, ".html")
    } else {
      final_url <- base_url
    }
  } else {
    final_url <- base_url
  }
  utils::browseURL(final_url)
  invisible(NULL)
}
