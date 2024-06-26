# functions and methods for IDEEA-electricity module

## Clusters

#' Cluster
#'
#' @param tol
#'
#' @return
#' @export
#'
#' @examples
ideea_cl_mask <- function(tol, width = 2) {
  # , wind = T, solar = !wind
  stopifnot(tol <= 1 & tol >= 0)
  if (tol == 1) return(paste0("TOL", 99))
  if (tol < .01) FORMAT = "f" else FORMAT = "d"
  filemask <- glue("TOL", formatC(tol * 100, width = width, flag = "0",
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

#' Drop cluster identifier from a name
#'
#' @param name
#' @param digits
#' @param cluster_flag
#'
#' @return
#' @export
#'
#' @examples
drop_cluster <- function(name, digits = 2, cluster_flag = "CL") {
  pattern1 <- paste0("_(?:", cluster_flag,")?(?<!\\d)(\\d{", digits,"})(?!\\d)")
  # pattern2 <- paste0("\\d{", digits,"}")
  # browser()
  str_replace(name, pattern1, "")
}

#' Drop cluster identifier from a process name
#'
#' @param x
#' @param digits
#' @param cluster_flag
#' @export
drop_process_cluster <- function(x, digits = 2, cluster_flag = "CL") {
  if (!"process" %in% names(x)) {
    return(x)
  }
  x |> mutate(process = drop_cluster(process, digits = digits,
                                     cluster_flag = cluster_flag))
}


if (F) {
  drop_cluster(c("ESOL_V2030_CL47", "ESOL_CL21_V2030"), digits = 2)
  drop_cluster(c("ESOL_CL2030_471", "ESOL_21_2030"), digits = 3)
  drop_cluster(c("ESOL_V2030_CL47", "ESOL_CL21_V2030",
                 "ESOL_2030_47", "ESOL_21_2030"))
  drop_cluster(c("ESOL_2030_471", "ESOL_21_2030", "ESOL_V2030_CL47",
                 "ESOL_CL21_V2030"))

}
