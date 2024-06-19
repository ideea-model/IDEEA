# scenario-management utils ####

#' Set directory for IDEEA scenarios
#'
#' @param path character path to the directory with/for IDEEA scenarios
#'
#' @return
#' @export
#'
#' @examples
set_ideea_scenarios <- function(path = NULL) {
  # browser()
  if (!is.null(path) && path != "") {
    if (!dir.exists(path)) {
      stop(paste0('The path "', path, '" does not exist.'), call. = FALSE)
    }
    if (!grepl("\\/$", path)) {
      path <- paste0(path, "/")
    }
  }
  options(IDEEA.scenarios = path)
  invisible(TRUE)
}

#' Get current IDEEA-scenarios directory
#'
#' @return
#' @export
#'
#' @examples
ideea_scenarios <- function() {
  options(IDEEA.scenarios)
}


#' Add model, calendar, and other details to a name IDEEA scenario
#'
#' @param name character, basic name of IDEEA scenario
#' @param model_name character, model name
#' @param calendar_name character, calendar name
#' @param suffix character, additional string to add to the name
#' @param sep character, a separator between identifiers, "_" by default
#'
#' @return
#' @export
#'
#' @examples
ideea_scenario_name <- function(
    name,
    model_name = NULL,
    calendar_name = NULL,
    horizon_name = NULL,
    suffix = NULL,
    sep = "_"
  ) {

  if (!is.null(model_name)) {
    name <- paste(name, model_name, sep = sep)
  }

  if (!is.null(calendar_name)) {
    name <- paste(name, calendar_name, sep = sep)
  }

  if (!is.null(horizon_name)) {
    name <- paste(name, horizon_name, sep = sep)
  }

  if (!is.null(suffix)) {
    name <- paste(name, suffix, sep = sep)
  }

  return(name)
}
