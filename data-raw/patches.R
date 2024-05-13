## code to prepare `DATASET` dataset goes here

# library(merra2ools)
# gis_sf <- get_ideea_map(36, offshore = T, islands = T)


## Patches #########################################
if (F) {
  # patch: adding km2 to reg_tbl (2024-05-07) ####
  # library(data.table)
  # add km2-weights to the 'ideea_data$reg_tbl'
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

if (F) {
  # patch: renaming region -> reg5 in "biomass_ava_assumption" (2024-05-08) ####
  ideea_data <- ideea_data
  ideea_data$biomass_ava_assumption
  ideea_data$biomass_ava_assumption <- rename(
    ideea_data$biomass_ava_assumption,
    reg5 = region
    )

  # save to the package data
  usethis::use_data(ideea_data, internal = F, overwrite = T)
}


# usethis::use_data(DATASET, overwrite = TRUE)
