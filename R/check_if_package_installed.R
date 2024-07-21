#' Check If a Package is Installed
#'
#' Checks if a specified package is installed and available for use. If the
#' package is not installed, the function stops execution and prompts the user
#' to install the package.
#'
#' @param package The name of the package to check.
#' @param type The type for which the package needs to be available.
#'
#' @return Invisible `TRUE` if the package is installed; otherwise, it stops
#'   execution with an error message advising the installation of the package.
#'   Since the function is designed to stop if the package is not found, it does
#'   not explicitly return a value upon successful completion.
check_if_package_installed <- function(package, type) {
  rlang::check_installed(
    package,
    reason = paste0("to download type '", type, "'")
  )
}
