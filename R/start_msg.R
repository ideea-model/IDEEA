# The script below is sourced when the package is loaded
# It prints the IDEEA package version and
# loads defaults if configured and saved with 'ideea_global_options()' function
.onAttach <- function(...) {
  # start message
  packageStartupMessage(
    "\n",
    glue::glue('IDEEA version: {packageVersion("IDEEA")} ({packageDate("IDEEA")})'),
    '\nhttps://github.com/ideea-model/IDEEA\n'
  )
  # load global settings if exist
  if (file.exists("~/.ideea.R")) {
    try({
      IDEEA::ideea_global_options(load = TRUE)
    })
  }
}

