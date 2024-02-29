#' Create WRDS Dummy Database
#'
#' Downloads the WRDS dummy database from the respective Tidy Finance GitHub
#' repository and saves it to the specified path. If the file already exists,
#' the user is prompted before it is replaced.
#'
#' @param path The file path where the SQLite database should be saved. If not
#'   provided, the default path is "data/tidy_finance_r.sqlite".
#'
#' @return Invisible NULL. Side effect: downloads a file to the specified path.
#'
#' @examples
#' create_wrds_dummy_database(path = "path/to/your/directory/tidy_finance_r.sqlite")
#'
#' @export
create_wrds_dummy_database <- function(path) {

  if(missing(path)) {
    stop("Please provide a file path for the SQLite database. We recommend 'data/tidy_finance_r.sqlite'.")
  }

  if(file.exists(path)) {
    response <- readline(prompt = "The database file already exists at this path. Do you want to replace it? (Y/n): ")
    if(tolower(response) != "y") {
      message("Operation aborted by the user.")
      return(invisible(NULL))
    }
  }

  url <- "https://github.com/tidy-finance/website/tree/main/blog/tidy-finance-dummy-data/data/tidy_finance.sqlite"
  download.file(
    url = url,
    destfile = path,
    quiet = TRUE
  )

  message(paste0("Downloaded WRDS dummy database to ", path, "."))

  return(invisible(NULL))
}
