.onAttach <- function(...) {
  packageStartupMessage(
"\n",
glue::glue('IDEEA version: {packageVersion("IDEEA")} ({packageDate("IDEEA")})'),
'\nhttps://github.com/ideea-model/IDEEA\n'
)}

