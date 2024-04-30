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
