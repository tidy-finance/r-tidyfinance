#' Set WRDS Credentials
#'
#' This function prompts the user to input their WRDS (Wharton Research Data Services)
#' username and password, and stores these credentials in a .Renviron file. The user can
#' choose to store the .Renviron file in either the project directory or the home directory.
#' If the .Renviron file already contains WRDS credentials, the user will be asked if they
#' want to overwrite the existing credentials. Additionally, the user has the option
#' to add the .Renviron file to the .gitignore file to prevent it from being tracked
#' by version control.
#'
#' @returns Invisibly returns TRUE. Displays messages to the user based on their input and actions taken.
#'
#' @export
#' @examples
#' \dontrun{
#' set_wrds_credentials()
#' }
set_wrds_credentials <- function() {
  wrds_user <- readline(prompt = "Enter your WRDS username: ")
  wrds_password <- readline(prompt = "Enter your WRDS password: ")
  location_choice <- readline(
    prompt = "Where do you want to store the .Renviron file? Enter 'project' for project directory or 'home' for home directory: "
  )

  if (tolower(location_choice) == "project") {
    renviron_path <- file.path(getwd(), ".Renviron")
    gitignore_path <- file.path(getwd(), ".gitignore")
  } else if (tolower(location_choice) == "home") {
    renviron_path <- file.path(Sys.getenv("HOME"), ".Renviron")
    gitignore_path <- file.path(Sys.getenv("HOME"), ".gitignore")
  } else {
    cli::cli_inform(
      "Invalid choice. Please start again and enter {.str project} or {.str home}."
    )
    return(invisible(TRUE))
  }

  env_lines <- if (file.exists(renviron_path)) readLines(renviron_path) else
    character()

  wrds_user_exists <- any(grepl("^WRDS_USER=", env_lines))
  wrds_password_exists <- any(grepl("^WRDS_PASSWORD=", env_lines))

  if (wrds_user_exists || wrds_password_exists) {
    overwrite_choice <- readline(
      prompt = "Credentials already exist. Do you want to overwrite them? Enter 'yes' or 'no': "
    )
    if (tolower(overwrite_choice) != "yes") {
      cli::cli_inform(
        "Aborted. Credentials already exist and were not overwritten."
      )
      return(invisible(TRUE))
    }
  }

  if (file.exists(gitignore_path)) {
    add_gitignore <- readline(
      prompt = "Do you want to add .Renviron to .gitignore? It is highly recommended! Enter 'yes' or 'no': "
    )
    if (tolower(add_gitignore) == "yes") {
      gitignore_lines <- readLines(gitignore_path)
      if (!any(grepl("^\\.Renviron$", gitignore_lines))) {
        gitignore_lines <- c(gitignore_lines, ".Renviron")
        writeLines(gitignore_lines, gitignore_path)
        cli::cli_inform("{.file .Renviron} added to {.file .gitignore}.")
      }
    } else if (tolower(add_gitignore) == "no") {
      cli::cli_inform("{.file .Renviron} NOT added to {.file .gitignore}.")
    } else {
      cli::cli_inform(
        "Invalid choice. Please start again and enter 'yes' or 'no'."
      )
      return(invisible(TRUE))
    }
  }

  env_lines <- env_lines[!grepl("^WRDS_USER=", env_lines)]
  env_lines <- env_lines[!grepl("^WRDS_PASSWORD=", env_lines)]

  env_lines <- c(
    env_lines,
    sprintf("WRDS_USER=%s", wrds_user),
    sprintf("WRDS_PASSWORD=%s", wrds_password)
  )

  writeLines(env_lines, renviron_path)

  readRenviron(renviron_path)

  cli::cli_inform(
    "WRDS credentials have been set and saved in {.file .Renviron} in your {.path {location_choice}} directory."
  )
}
