## Info: code to prepare IDEEA data embedded in the package
## To run: restart R-session and source the file
## Output: *.rds and *.rda files in R/ and data/
library(tidyverse)
library(sf)
library(fst)
library(here)

# package data building parameters:
# (configure which dataset will be rebuild) Test
make_rda <- list(
  ideea_map = FALSE, # make/overwrite existing "ideea_map.rda"
  # techs_from_google_sheets = FALSE #
  ideea_data = TRUE, #
  # ideea_data = FALSE, #
  # ideea_modules = FALSE
  ideea_modules = TRUE
)

#==============================================================================#
# Google Sheets i/o
# [the import is done in "build.Rmd" file - currently not automated]
#==============================================================================#
if (F) { # make_rda$techs_from_google_sheets - not finished
  # run once to prepare csv-file with regions - for `regions` worksheet
  # !!! No Need to rerun !!!
  # The table 'regions' has been already created and saved
  m <-  get_ideea_map(46, offshore = T, islands = T, ROW = T, aggregate = F,
                      rename = F)
  names_order <- c(
    "name46",	"reg46", "name37", "reg37", "name36", "reg36", "name34", "reg34",
    "name32", "reg32", "name7", "reg7", "name5", "reg5", "name1", "reg1",
    "mainland", "offshore"
  )
  reg_tab <- m %>% st_drop_geometry() %>% select(all_of(names_order))
  write.csv(reg_tab, "data-raw/googlesheets/region.csv", row.names = F)
  regions_tbl <- reg_tab
  save(regions_tbl, file = "data-raw/regions_tbl.RData")
}

#==============================================================================#
# (re-)build datasets ####
#==============================================================================#
## ideea_map.rda ####
if (make_rda$ideea_map) {
  message("Saving 'ideea_map.rda'")
  # add ROW to ideea_r46o15
  # (load("~/R/IDEEA.dev/data-raw/maps/ideea_row.RData"))
  (load("data-raw/maps/ideea_row.RData"))
  (load("data-raw/maps/ideea_r46o15_sf.RData"))

  if (F) {
    # checks
    ideea_row_sf[1] %>% plot(reset = F, col = "lightgrey", border = "grey")
    ideea_r46o15_sf[1] %>% plot(add = T)
  }

  x <- st_drop_geometry(ideea_r46o15_sf[1,])
  x[1,] <- NA
  x$reg1 <- ideea_row_sf$region
  x$name1 <- ideea_row_sf$name
  x
  st_crs(ideea_row_sf)
  ideea_map <- ideea_r46o15_sf %>%
    rbind(st_as_sf(x, geometry = ideea_row_sf$geometry))

  if (F) {
    # checks
    plot(ideea_map[1])
    ggplot(ideea_map) +
      geom_sf(aes(fill = reg32)) +
      scale_fill_viridis_d(option = "H", na.value = "grey") +
      theme_bw()
  }

  # save internal dataset(s) to R/sysdata.rda for use inside IDEEA package
  usethis::use_data(ideea_map, overwrite = TRUE, internal = TRUE)
}

if (make_rda$ideea_data) {
  message("Saving 'ideea_data.rda'")
  ideea_data <- list() # initialize

  # table with all regional groups
  (load("data-raw/regions_tbl.RData"))
  ideea_data$reg_tbl <- regions_tbl
  # load
  (load("data-raw/load_2019_clean.RData"))
  ideea_data$load_2019_MWh <- load_2019_clean |> select(where(~ !all(is.na(.))))

  # Electricity consumption
  (load("data-raw/googlesheets/elc_demand.RData"))
  ideea_data$elc_consumption <- elc_demand |> select(where(~ !all(is.na(.))))

  # Coal
  (load("data-raw/googlesheets/coal.RData"))
  # ideea_data$coal <- filter(coal, !is.na(total_reserve_Mt))
  ideea_data$coal <- coal |> select(where(~ !all(is.na(.))))

  # Lignite
  (load("data-raw/googlesheets/lignite.RData"))
  ideea_data$lignite <- lignite |> select(where(~ !all(is.na(.))))

  # Oil
  (load("data-raw/googlesheets/oil.RData"))
  ideea_data$oil <- oil |> select(where(~ !all(is.na(.))))

  # Gas
  (load("data-raw/googlesheets/gas.RData"))
  ideea_data$gas <- gas |> select(where(~ !all(is.na(.))))

  # CCS
  (load("data-raw/googlesheets/ccs_r5.RData"))
  ideea_data$ccs_r5 <- ccs_r5 |> select(where(~ !all(is.na(.))))

  # Gas-availability
  (load("data-raw/googlesheets/gas_ava.RData"))
  ideea_data$gas_ava_assumption <- gas_ava |> select(where(~ !all(is.na(.))))

  # Biomass
  (load("data-raw/googlesheets/biomass.RData"))
  ideea_data$biomass <- biomass |> select(where(~ !all(is.na(.))))

  # Biomass-availability
  (load("data-raw/googlesheets/bio_ava.RData"))
  ideea_data$biomass_ava_assumption <- bio_ava |> select(where(~ !all(is.na(.))))

  # Hydro hourly capacity factors
  (load("data-raw/googlesheets/hydro_hourly_cf.RData"))
  ideea_data$hydro_hourly_cf_2013 <- hydro_hourly_cf |>
    select(where(~ !all(is.na(.))))

  # Power Plants (WRI)
  if (F) { # tests
    # (load("data-raw/wri_sf.RData"))
    # ideea_data$powerplants_sf <- wri_sf
    # load("data-raw/wri.RData")
    # pp <- wri %>% filter(country == "IND")
    # usethis::use_data(pp)

    # st_as_sf(wri_reg46, geometry = wri_sf$geometry)

    # summary(wri_reg46$primary_fuel == wri_sf$primary_fuel)
    # summary(wri_reg46$primary_fuel == wri_sf$primary_fuel)

    # wri_reg46 %>%
    #   left_join(
    #     select(wri_sf, any_of(c(names(wri_reg46), "geometry")))
    #   ) %>%
    #   st_as_sf()
  }

  # Aggregated (reg46) power plants data
  (load("data-raw/wri_reg46.RData"))
  # correcting data for an error in offshore assignment
  ii <- wri_reg46$offshore; summary(ii)
  wri_reg46$reg_off[ii] <- wri_reg46$region[ii]
  wri_reg46$offshore[ii] <- FALSE
  ideea_data$generators_wri <- wri_reg46

  # corrected Capacities for 2020
  (load("data-raw/googlesheets/capacity_corrected_2020.RData"))
  capacity_corrected_2020 <- capacity_corrected_2020 |>
    mutate(offshore = FALSE)
  ideea_data$generators_2020 <- capacity_corrected_2020

  # GWA intervals map
  (load("data-raw/gwa_iec2_r36o13_sf.RData"))
  ideea_data$gwa_sf <- gwa_iec2_sf

  # Wind clusters
  (load("data-raw/locid_waf100m_cl.RData"))
  ideea_data$merra_wind_clusters <- locid_win_cl

  # Solar clusters
  (load("data-raw/locid_sol_cl.RData"))
  ideea_data$merra_solar_clusters <- locid_sol_cl

  # MERRA-2 raw data (2014)
  merra_raw_2014 <- fst::read_fst("data-raw/merra/merra_raw_2014.fst",
                                  as.data.table = T)
  ideea_data$merra_raw_2014 <- merra_raw_2014
  # ideea_data$merra_raw_2019 <- merra_raw_2019

  # Solar capacity factors
  (load("data-raw/merra/sol_cf_r7.RData"))
  ideea_data$merra_solar_cf_reg7 <- sol_cf_r7

  # Wind capacity factors
  (load("data-raw/merra/win_cf_r7.RData"))
  ideea_data$merra_wind_cf_reg7 <- win_cf_r7

  # Transmission data
  (load("data-raw/googlesheets/transmission_5x5.RData"))
  ideea_data$transmission_5x5 <- transmission_5x5

  names(ideea_data)
  # save to the package data
  usethis::use_data(ideea_data, internal = F, overwrite = T)
}

#==============================================================================#
# (re-)build IDEEA modules ####
# (ready to use model objects)
#==============================================================================#

if (make_rda$ideea_modules) {
  message("Saving 'ideea_modules.rda'")
  # initiate list for modules
  ideea_modules <- list()

  # time resolution options [!!! replaced with timetables]
  # (load("data-raw/time_sets.RData"))
  # names(time_sets)
  # ideea_modules$time_scales <- time_sets
  (load("data-raw/time_tables.RData"))
  ideea_modules$time_tables <- time_tables

  # Primary energy supply
  (load("data-raw/repo_energy.RData"))
  ideea_modules$energy <- repo_energy@data
  names(ideea_modules$energy)
  summary(ideea_modules$energy)

  # Technologies
  (load("data-raw/ideea_techs.RData"))
  ideea_modules$techs <- ideea_techs
  summary(ideea_techs)

  (load(here("data-raw/repo_CCUS.RData")))
  ideea_modules$CCUS <- repo_CCUS@data
  summary(ideea_modules$CCUS)

  (load(here("data-raw/electricity_reg7_base.RData")))
  ideea_modules$electricity <- list()
  ideea_modules$electricity$reg7_base <- electricity
  summary(ideea_modules$electricity$reg7)

  # (load("data-raw/repo_electricity.RData"))
  # ideea_modules$electricity <- repo_electricity@data

  # (load("data-raw/repo_policy.RData"))
  # ideea_modules$policy <- repo_electricity@data

  # save to the package data
  usethis::use_data(ideea_modules, internal = F, overwrite = T)
}








