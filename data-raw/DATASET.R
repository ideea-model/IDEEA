## code to prepare `DATASET` dataset goes here

library(merra2ools)
gis_sf <- get_ideea_map(36, offshore = T, islands = T)




usethis::use_data(DATASET, overwrite = TRUE)
