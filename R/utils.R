#' Drop the numeric part of a string (aka vintage in names of technologies)
#'
#' @param x A character vector
#'
#' @return
#' @export
#'
#' @examples
#' names(ideea_techs$ECOASUB)
#' drop_vintage(names(ideea_techs$ECOASUB))
#'
drop_vintage <- function(x) {
  sub("_[0-9]+$", "", x)
}

#' Drop the numeric part of a names in `process` column of a data frame
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
#' dat <- data.frame(scenario = "BASE", process = names(ideea_techs$ENUC))
#' dat
#' dat |> drop_process_vintage()
drop_process_vintage <- function(x) {
  if (!"process" %in% names(x)) {
    return(x)
  }
  x |> mutate(process = drop_vintage(process))
}

# theme_ideea <- theme_bw() +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# cluster-version identifier to add to names of capacity factors
# to distinguish alternative versions of clusters
# win_cl_mask <-
#   glue(
#     "CL", params$win_cl_max, # max allowed number of clusters
#     "_TOL", prettyNum(params$win_cl_tol) # loss/tolerance
#   ) %>%
#   str_replace_all("\\.", "")
#
# sol_cl_mask <-
#   glue(
#     "CL", params$sol_cl_max, # max allowed number of clusters
#     "_TOL", prettyNum(params$sol_cl_tol) # loss/tolerance
#   ) %>%
#   str_replace_all("\\.", "")


