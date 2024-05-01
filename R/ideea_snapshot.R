# A group of functions to plot an instance of the model's data, parameter, variable, or the system operation

# ideea_cl_sf <- get_ideea_cl_sf("sol")
# x <- get_ideea_cf("sol")


ideea_snapshot_cf <- function(x,
                              ideea_cl_sf,
                              ideea_sf,
                              cf_name = "wcf_100m",
                              slice = x$slice[1],
                              datatime = FALSE,
                              return_data = FALSE,
                              ...
                              ) {
  SLICE <- slice

  if (inherits(ideea_cl_sf$cluster, "factor")) {
    ideea_cl_sf <- mutate(
      ideea_cl_sf,
      cluster = as.integer(as.character(cluster))
    )
  }

  d <- x |>
    filter(slice %in% SLICE) |>
    full_join(ideea_cl_sf, by = "cluster", relationship = "many-to-many") |>
    st_as_sf()

  if (return_data) return(d)

  ggplot(d) +
    # geom_sf()
    geom_sf(aes(fill = .data[[cf_name]])) +
    scale_fill_viridis_c()



}
