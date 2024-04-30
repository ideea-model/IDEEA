## code to prepare `MERRA2` dataset for IDEEA models
library(merra2ools)

# get map for the largest number of regions
gis_sf <- get_ideea_map(36, offshore = T, islands = T)
ideea_locid <- get_locid(gis_sf)
ideea_locid_sf <- get_merra2_grid(type = "poly", locid = ideea_locid)

ideea_merra <- list()

ideea_merra$ideea_locid <- ideea_locid
ideea_merra$ideea_locid_sf <- ideea_locid_sf



# usethis::use_data(MERRA2, overwrite = TRUE)
