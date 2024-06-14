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


#' Title
#'
#' @param tol
#'
#' @return
#' @export
#'
#' @examples
ideea_cl_mask <- function(tol) {
  # , wind = T, solar = !wind
    stopifnot(tol <= 1 & tol >= 0)
    if (tol == 1) return(paste0("TOL", 99))
    if (tol < .01) FORMAT = "f" else FORMAT = "d"
    filemask <- glue("TOL", formatC(tol * 100, width = 2, flag = "0",
                                    format = FORMAT))
    # if ()
    # if (!is.null(year))
    # if (!is.null(dir))
    filemask
}

#' Combine name with cluster number
#'
#' @param name
#' @param cluster
#' @param ndigits
#'
#' @return
#' @export
#'
#' @examples
name_with_cluster <- function(name, cluster, ndigits = 2) {
  paste0(
    name,
    formatC(cluster,
            digits = ndigits, width = ndigits,
            flag = "0", mode = "integer"
    )
  )
}
if (F) {
  name_with_cluster("", 0)
  name_with_cluster("SOL", 1)
  name_with_cluster("WIN_CL", 99)
}

#' Extract cluster string or number from a name
#'
#' @param name
#' @param digits
#' @param cluster_flag
#'
#' @return
#' @export
#'
#' @examples
get_cluster <- function(name, digits = 2, cluster_flag = "CL") {
  # pattern1 <- paste0("(?:CL)?(\\d{", digits,"})(?!\\d)")
  pattern1 <- paste0("(?:", cluster_flag,")?(?<!\\d)(\\d{", digits,"})(?!\\d)")
  pattern2 <- paste0("\\d{", digits,"}")
  # browser()
  str_extract(name, pattern1) |> str_extract(pattern2)
}

if (F) {
  get_cluster(c("ESOL_2030_47", "ESOL_21_2030"))
  get_cluster(c("ESOL_2030_471", "ESOL_21_2030"), digits = 2)
  get_cluster(c("ESOL_CL2030_471", "ESOL_21_2030"), digits = 2)
  get_cluster(c("ESOL_CL2030_471", "ESOL_21_2030"), digits = 3)
  get_cluster(c("ESOL_2030_47", "ESOL_21_2030"), digits = 4)
  get_cluster(c("ESOL_2030_CL47", "ESOL_21_2030"))
  get_cluster(c("ESOL_V2030_CL47", "ESOL_CL21_V2030",
                "ESOL_2030_47", "ESOL_21_2030"))
  get_cluster(c("ESOL_2030_471", "ESOL_21_2030", "ESOL_V2030_CL47",
                "ESOL_CL21_V2030"))
}

