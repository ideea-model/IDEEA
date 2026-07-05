# functions and methods for IDEEA-electricity module

## Clusters

#' Cluster
#'
#' @param tol numeric in `[0, 1]`, the cluster loss tolerance.
#' @param width integer, zero-padded width of the tolerance in the mask (default 2).
#'
#' @return A character file-mask string such as `"TOL05"`.
#' @export
#'
#' @examples
#' ideea_cl_mask(0.05)
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
#' @param name character (vector), base name to append the cluster number to.
#' @param cluster integer, cluster number to append.
#' @param ndigits integer, zero-padded width of the cluster number (default 2).
#'
#' @return A character vector: `name` with the zero-padded cluster number appended.
#' @export
#'
#' @examples
#' name_with_cluster("SOL", 1)
#' name_with_cluster("WIN_CL", 99)
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
#' @param name character vector of names to extract the cluster number from.
#' @param digits integer, number of digits in the cluster number (default 2).
#' @param cluster_flag character prefix that may precede the cluster number (default "CL").
#'
#' @return A character vector of the extracted cluster numbers (`NA` where none found).
#' @export
#'
#' @examples
#' get_cluster(c("ESOL_2030_47", "ESOL_21_2030"))
#' get_cluster("ESOL_CL2030_471", digits = 3)
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
#' @param name character vector of names.
#' @param digits integer, number of digits in the cluster number (default 2).
#' @param cluster_flag character prefix that may precede the cluster number (default "CL").
#'
#' @return The names with the cluster identifier removed.
#' @export
#'
#' @examples
#' drop_cluster(c("ESOL_V2030_CL47", "ESOL_CL21_V2030"))
drop_cluster <- function(name, digits = 2, cluster_flag = "CL") {
  pattern1 <- paste0("_(?:", cluster_flag,")?(?<!\\d)(\\d{", digits,"})(?!\\d)")
  # pattern2 <- paste0("\\d{", digits,"}")
  # browser()
  stringr::str_replace(name, pattern1, "")
}

#' Drop cluster identifier from a process name
#'
#' @param x A data.frame with a `process` column.
#' @param digits integer, number of digits in the cluster number (default 2).
#' @param cluster_flag character prefix that may precede the cluster number (default "CL").
#' @return `x` with the cluster identifier dropped from its `process` column.
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
