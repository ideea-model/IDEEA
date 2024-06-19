#' Save, load, edit, or remove IDEEA global options
#'
#' @param load logical, if TRUE load the global options from the file
#' @param edit logical, if TRUE open the file in the IDE or default text editor
#' @param remove logical, if TRUE remove the global options file
#'
#' @return on the first run creates file in '~/.ideea.R' with commented settings and opens the file for editing. On the second run, sources the file to load the settings. Returns TRUE invisibly if successful, or FALSE if not. The function is called with the loading of IDEEA package and sources the '~/.ideea.R' file if previously created.
#' @export
#'
#' @examples
ideea_global_options <- function(edit = FALSE, load = !edit, remove = FALSE) {
  fl <- "~/.ideea.R" # hardwired ideea options file in the system home folder
  if (!file.exists(fl)) {
    message("Creating IDEEA global options: '", fl, "'")
    success <- file.create(fl)
    if (!success) {
      message("Cannot create file '", fl, "'")
      return(invisible(success))
    }
    write_lines("message('Loading IDEEA global options.')", fl, append = T)
    write_lines("message(\"(use 'ideea_global_options(edit = TRUE)' to edit)\"", fl, append = T)
    write_lines('', fl, append = T)
    write_lines("# IDEEA external dataset", fl, append = T)
    write_lines("# set_ideea_extra('...')", fl, append = T)
    write_lines('', fl, append = T)
    write_lines("# IDEEA solver options", fl, append = T)
    write_lines("# energyRt::set_gams_path('C:/GAMS/...')", fl, append = T)
    write_lines("# energyRt::set_gdxlib_path('C:/GAMS/...')", fl, append = T)
    # write_lines("# energyRt::set_gams_path('C:/GAMS/...')", fl, append = T)
    write_lines("# energyRt::set_glpk_path()", fl, append = T)
    write_lines("# energyRt::set_julia_path()", fl, append = T)
    write_lines("# energyRt::set_python_path()", fl, append = T)
    write_lines("# energyRt::set_default_solver(solver_options$julia_highs_barrier)",
                fl, append = T)
    write_lines(" ", fl, append = T)
    write_lines("# IDEEA external data folder (IDEEA.extra):", fl, append = T)
    write_lines("options(IDEEA.extra = '...')", fl, append = T)
    write_lines(" ", fl, append = T)
    file.edit(fl)
    return(invisible(TRUE))
  } else if (remove) {
    message('Deleting IDEEA global options: "', fl, '"')
    success <- file.remove(fl)
    if (!success) message("Cannot remove file '", fl,"'")
    return(invisible(success))
  }
  # open to edit (works in RStudio IDE)
  if (edit) {
    file.edit(fl)
  }
  # load global options/defaults
  if (load) {
    source(fl)
  }
  invisible(TRUE)
}


