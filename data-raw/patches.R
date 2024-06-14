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
  ideea_data <- IDEEA::ideea_data
  ideea_data$biomass_ava_assumption
  ideea_data$biomass_ava_assumption <- rename(
    ideea_data$biomass_ava_assumption,
    reg5 = region
    )

  # save to the package data
  usethis::use_data(ideea_data, internal = F, overwrite = T)
}

if (F) {
  # patch: "generators_wri" (2024-05-15) ####
  # drop excessive "reg*" from "generators_wri"
  # the regionalization is made for 46 regions, based on given lon/lat
  ideea_data$generators_wri |> names()
  ideea_data <- IDEEA::ideea_data
  generators_wri <- ideea_data$generators_wri |>
    select(-(matches("reg") & !matches("reg46")))
  ideea_data$generators_wri <- generators_wri
  names(ideea_data$generators_wri)
  # save to the package data
  usethis::use_data(ideea_data, internal = F, overwrite = T)
}

if (F) {
  library(tidyverse)
  ideea_data <- IDEEA::ideea_data
  ideea_data$gas_ava_assumption <- ideea_data$gas_ava_assumption |>
    rename(reg5 = "region")
  names(ideea_data$gas_ava_assumption)
  # save to the package data
  usethis::use_data(ideea_data, internal = F, overwrite = T)

}

if (F) {
  # transmission
  tra_5x5 <- ideea_data$transmission_5x5 |>
    rename(region1 = destination, case = scenario)

  fwrite(tra_5x5, file = glue("data-raw/trade_matrix_r5_v01.csv"))

}
