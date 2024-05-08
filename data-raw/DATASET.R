## code to prepare `DATASET` dataset goes here

# library(merra2ools)
# gis_sf <- get_ideea_map(36, offshore = T, islands = T)


if (F) {
  # library(data.table)
  # add km2-weights to the 'ideea_data$reg_tbl' ####
  ideea_data <- ideea_data
  reg_tbl <- ideea_data$reg_tbl

  gis46_sf <- get_ideea_map(46, islands = T)
  reg_tbl_km2 <- gis46_sf |>
    mutate(km2 = set_units(st_area(geometry), "km^2")) |>
    st_drop_geometry()

  reg_tbl <- reg_tbl |>
    left_join(reg_tbl_km2) |>
    mutate(km2 = as.numeric(km2)) |>
    as.data.table()

  ideea_data$reg_tbl <- reg_tbl

  names(ideea_data)
  # save to the package data
  usethis::use_data(ideea_data, internal = F, overwrite = T)

}



# usethis::use_data(DATASET, overwrite = TRUE)
