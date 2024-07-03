#' Check if a Dataset Type is Supported
#'
#' This function checks if a given dataset type is supported by verifying
#' against a list of all supported dataset types from different domains. If the
#' specified type is not supported, it stops execution and returns an error
#' message listing all supported types.
#'
#' @param type The dataset type to check for support.
#'
#' @return Does not return a value; instead, it either passes silently if the
#'   type is supported or stops execution with an error message if the type is
#'   unsupported.
#'
check_supported_type <- function(type) {
  supported_types <- list_supported_types()
  if (!any(type %in% supported_types$type)) {
    stop("Unsupported type specified. Call the function list_supported_types() to get all supported types.")
  }
}
