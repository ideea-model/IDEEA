# A set of functions to create gif-files of IDEEA inputs and outputs


#' Create GIF animation of capacity factors by cluster and time-slice
#'
#' @param x data.frame of capacity factors, typically from get_ideea_cf()
#' @param ideea_cl_sf sf, cluster shapefile, typically from get_ideea_cl_sf()
#' @param ideea_sf sf, ideea shapefile, typically from get_ideea_map()
#' @param cf_name character, name of capacity factor column in x (e.g. "wcf_100m", "scf_tl", etc.)
#' @param slice character vector, time-slices to udpate in the GIF
#' @param timestamp.stamp logical, if TRUE, add timestamp to the plot
#' @param fill_scale_lims numeric vector of length two, limits of the fill scale
#' @param fps numeric, frames per second, default is 12
#' @param gif.width numeric, width of the GIF, default is 576
#' @param gif.height numeric, height of the GIF, default is 576
#' @param filename character, name of the GIF file
#'
#' @return
#' A GIF animation of capacity factors by cluster and time-slice saved in the working directory
#' @export
#'
#' @examples
#' # do not run
#' resource <- "win"; cf_name <- "wcf_100m"
#' resource <- "sol"; cf_name <- "scf_tl"
#' nreg <- 5
#' tol <- 0.01
#'
#' ideea_sf <- get_ideea_map(nreg = nreg, offshore = T, islands = T)
#' ideea_cl_sf <- get_ideea_cl_sf(resource = resource, tol = tol)
#'
#' ideea_cl_sf$cluster |> unique()
#' plot(ideea_cl_sf["cluster"])
#'
#' x <- get_ideea_cf(resource, tol = tol)
#'
#' slices_1day_per_month <-
#'   ideea_modules$electricity$reg7_base$partial_calendar_1day_per_month@timetable$slice
#'
#' ideea_gif_cf(x, ideea_cl_sf, ideea_sf, cf_name = cf_name,
#'              slice = slices_1day_per_month,
#'              filename = glue("tmp/{resource}.gif"))
ideea_gif_cf <- function(
    x,
    ideea_cl_sf,
    ideea_sf = NULL,
    cf_name = names(x)[grepl("cf_"), names(x)][1],
    slice = unique(x$slice)[1:24],
    timestamp.stamp = TRUE,
    # return_data = FALSE,
    fill_scale_lims = range(x[[cf_name]]),
    fps = 12,
    gif.width = 576, gif.height = 576,
    filename = "ideea_cf.gif"
    ) {

  verbose <- TRUE
  nframes <- length(slice)

  animation::saveGIF({
    if (verbose) cat("frame:")
    for (i in 1:nframes) {
      if (verbose) cat(format(i, width = nchar(nframes) + 1))
      # ii <- x[[timestamp.variable]] == frames[i]
      # arg$x <- x[ii,]
      arg <- list(
        x = x,
        ideea_cl_sf = ideea_cl_sf,
        ideea_sf = ideea_sf,
        cf_name = cf_name,
        slice = slice[i],
        timestamp.stamp = timestamp.stamp,
        return_data = FALSE,
        fill_scale_lims = fill_scale_lims
        )
      a <- do.call(ideea_snapshot_cf, arg, quote = FALSE)
      # a <- rlang::exec(.fn = FUN, !!!arg)
      if (i == nframes) {
        if (verbose) cat(" -> creating GIF\n")
      } else {
        if (verbose) cat(rep("\b", nchar(nframes) + 1), sep = "")
      }
      if (!is.null(a)) print(a)
    }
  },
  interval = 1/fps, ani.width = gif.width, ani.height = gif.height,
  movie.name = filename
  )
}


if (F) {
  # solar
  resource <- "sol"; cf_name <- "scf_tl"; tol <- 0.01
  nreg <- 5

  ideea_sf <- get_ideea_map(nreg = nreg, offshore = T, islands = T)
  ideea_cl_sf <- get_ideea_cl_sf(resource = resource, tol = tol)
  ideea_cl_sf$cluster |> unique()
  plot(ideea_cl_sf["cluster"])

  x <- get_ideea_cf(resource, tol = tol)

  slices_1day_per_month <-
    ideea_modules$electricity$reg7_base$partial_calendar_1day_per_month@timetable$slice

  ideea_gif_cf(x, ideea_cl_sf, ideea_sf, cf_name = cf_name,
               slice = slices_1day_per_month,
               fps = 5,
               filename = glue("{resource}.gif"))

  # wind
  resource <- "win"; cf_name <- "wcf_100m"; tol <- 0.05
  ideea_cl_sf <- get_ideea_cl_sf(resource = resource, tol = tol)
  ideea_cl_sf$cluster |> unique()
  plot(ideea_cl_sf["cluster"])

  x <- get_ideea_cf(resource, tol = tol)
  ideea_gif_cf(x, ideea_cl_sf, ideea_sf, cf_name = cf_name,
               slice = slices_1day_per_month,
               fps = 5,
               filename = glue("{resource}.gif"))


}

