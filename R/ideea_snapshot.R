# A group of functions to plot an instance of the model's data, parameter, variable, or the system operation

#' Plot a snapshot of capacity factors by cluster and time-slice
#'
#' @param x data.frame, capacity factors data, an outpuf of `get_ideea_cf()`
#' @param ideea_cl_sf sf object, cluster shapefile, an output of `get_ideea_cl_sf()`
#' @param ideea_sf sf object, ideea shapefile, an output of `get_ideea_map()`
#' @param cf_name character, the name of the capacity factor column (e.g. "wcf_100m", "scf_tl", etc.)
#' @param slice character of length one, the time-slice to plot (e.g. "d001_h00")
#' @param datatime logical, if TRUE, the time-slice is converted to a timestamp
#' @param return_data logical, if TRUE, the function returns the data used for plotting
#' @param fill_scale_lims numeric of length two, the limits of the fill scale
#' @param ... ignored
#'
#' @return
#' A ggplot object of capacity factors by cluster and time-slice or the data used for plotting
#' @export
#'
#' @examples
#' resource <- "win"
#' nreg <- 5
#' tol <- 0.1
#' cf_name <- "wcf_100m"
#' ideea_sf <- get_ideea_map(nreg = nreg, offshore = T, islands = T)
#' ideea_cl_sf <- get_ideea_cl_sf(resource = resource, tol = tol)
#' x <- get_ideea_cf(resource, tol = tol)
#' ideea_snapshot_cf(x, ideea_cl_sf, ideea_sf, cf_name = cf_name)
#' @import ggplot2
ideea_snapshot_cf <- function(
    x,
    ideea_cl_sf,
    ideea_sf = NULL,
    cf_name = names(x)[grepl("cf_"), names(x)][1],
    # cf_name = "wcf_100m",
    slice = sample(x$slice, 1),
    timestamp.stamp = TRUE,
    return_data = FALSE,
    fill_scale_lims = range(x[[cf_name]]),
    ...
  ) {
  # browser()
  SLICE <- slice

  if (inherits(ideea_cl_sf$cluster, "factor")) {
    ideea_cl_sf <- mutate(
      ideea_cl_sf,
      cluster = as.integer(as.character(cluster))
    )
  }

  if (!inherits(x$cluster, "integer")) {
    x <- mutate(x, cluster = as.integer(cluster))
  }

  ideea_cl_sf <- ideea_cl_sf |>
    select(starts_with("reg"), cluster, any_of(c("offshore", "mainland")))

  vrs <- intersect(names(x), names(ideea_cl_sf))

  d <- x |>
    filter(slice %in% SLICE) |>
    # filter(slice %in% x$slice[1000]) |>
    full_join(ideea_cl_sf, by = vrs, relationship = "many-to-many") |>
    st_as_sf()

  if (return_data) return(d)

  a <- ggplot()

  if (!is.null(ideea_sf)) {
    a <- a + geom_sf(data = ideea_sf, fill = "grey")
  }

  a <- a + geom_sf(aes(fill = .data[[cf_name]]), inherit.aes = F, data = d)

  if (is.numeric(d[[cf_name]])) {
    a <- a + scale_fill_viridis_c(option = "H", limits = range(x[[cf_name]]))
  } else {
    a <- a + scale_fill_viridis_d(option = "H")
  }

  if (isTRUE(timestamp.stamp)) {
    timestamp.stamp <- tsl2dtm(SLICE, year = x$year[1], tmz = "Asia/Kolkata") |>
      format("%Y-%b-%d, %Hh %Z")
  }
  timestamp.position <- c(97, 39.5)
  if (!isFALSE(timestamp.stamp) & is.character(timestamp.stamp)) {
    a <- a + geom_label(
      data = data.frame(x = timestamp.position[1],
                        y = timestamp.position[2],
                        label = timestamp.stamp),
      aes(x = x, y = y, label = label), label.size = 0., color = "black",
      hjust = 1, vjust = 1, fill = "white", alpha = 0.7) +
      labs(x = "", y = "")
  }

  a + scale_x_continuous(expand = c(0., 0.), limits = c(65, 97)) +
    scale_y_continuous(expand = c(0., 0.), limits = c(4, 40)) +
    theme_void()
}

if (F) {
  resource <- "win"; cf_name <- "wcf_100m"
  resource <- "sol"; cf_name <- "scf_tl"
  nreg <- 5
  tol <- 0.01

  ideea_sf <- get_ideea_map(nreg = nreg, offshore = T, islands = T)
  ideea_cl_sf <- get_ideea_cl_sf(resource = resource, tol = tol)
  # ideea_cl_sf$reg5_off |> unique()
  ideea_cl_sf$cluster |> unique()
  plot(ideea_cl_sf["cluster"])

  x <- get_ideea_cf(resource, tol = tol)

  ideea_snapshot_cf(x, ideea_cl_sf, ideea_sf, cf_name = cf_name)
  ideea_snapshot_cf(x, ideea_cl_sf, ideea_sf, cf_name = cf_name)
  ideea_snapshot_cf(x, ideea_cl_sf, ideea_sf, cf_name = cf_name)

}


