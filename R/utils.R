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

#' Add a column with names of offshore regions
#'
#' @param x data frame with required two columns: `offshore` and `regN`
#' @param regN character name of the column with region names
#' @param regN_off character name of the column with offshore region names,
#' that will be added to the data frame, default is `{regN}_off`
#' @param offshore character name of the column with offshore indicator,
#' default is 'offshore'
#'
#' @return data frame with added column `regN_off`
#' @export
#'
#' @examples
#'
add_reg_off <- function(x, regN,
                        regN_off = paste0(regN, "_off"),
                        offshore = "offshore") {
  # browser()
  REG_N <- as.character(regN)
  REG_N_OFF <- as.character(regN_off)
  OFFSHORE <- as.character(offshore)
  mutate(x,
    {{ REG_N_OFF }} := if_else(get(OFFSHORE),
                               paste0(get(REG_N), "_off"),
                               get(REG_N)),
    .after = REG_N
  )
}
