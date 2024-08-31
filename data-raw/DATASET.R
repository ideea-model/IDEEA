## The script reads "raw" data pre-saved in /data-raw and
## creates the package embedded dataset

## IDEEA modules ####
### energy.Rmd ####
if (F) { # run manually
  # read the package data and replace modules
  library(IDEEA)
  ideea_modules <- IDEEA::ideea_modules
  names(ideea_modules)
  names(ideea_modules$energy)
  # 5-region version
  energy_r5 <- load("data-raw/repo_energy_reg5.RData")
  energy_r5 <- get(energy_r5)
  names(energy_r5)
  # 32-region  version
  energy_r32 <- load("data-raw/repo_energy_reg32.RData")
  energy_r32 <- get(energy_r32)
  names(energy_r32)
  # combine
  energy <- list(
    reg5 = energy_r5,
    reg32 = energy_r32
  )
  # energy$reg32$SUP_GAS
  ideea_modules$energy <- energy
  names(ideea_modules$energy)
  # save to 'data/ideea_modules.rda'
  usethis::use_data(ideea_modules, internal = F, overwrite = T)
  # rebuild the package
}

### calendars.rmd ####
# update calendars
if (F) {
  library(IDEEA)
  ideea_modules <- IDEEA::ideea_modules
  names(ideea_modules)
  names(ideea_modules$calendars)
  cld <- load("data-raw/calendars.RData")
  cld
  names(calendars)
  names(horizons)
  ideea_modules$calendars <- calendars
  ideea_modules$horizons <- horizons
  # ideea_modules$time_tables <- NULL
  names(ideea_modules$calendars)
  names(ideea_modules$horizons)
  # save to 'data/ideea_modules.rda'
  usethis::use_data(ideea_modules, internal = F, overwrite = T)
  # rebuild the package
}

### ccus.Rmd ####
if (F) { # run manually
  # read the package data and replace modules
  library(IDEEA)
  ideea_modules <- IDEEA::ideea_modules
  names(ideea_modules)
  names(ideea_modules$CCUS)
  # 5-region version
  ccus_techs <- load("data-raw/repo_CCUS.RData")
  ccus_techs <- get(ccus_techs)
  names(ccus_techs)
  # update/replace
  ideea_modules$CCUS <- ccus_techs
  names(ideea_modules$CCUS)
  # save to 'data/ideea_modules.rda'
  usethis::use_data(ideea_modules, internal = F, overwrite = T)
}

# usethis::use_data(DATASET, overwrite = TRUE)

### transmission.rmd ####
if (F) {
  library(IDEEA)
  library(data.table)
  library(tidyverse)
  ideea_data <- ideea_data
  transmission <- list(
    reg5 = list(),
    reg32 = list()
  )
  if (!is.null(ideea_data$transmission_5x5)) {
    ideea_data$transmission_5x5 <- NULL
  }

  if (file.exists("data-raw/network_tables.RData")) {
    # the file 'network_tables.RData' is created by running 'transmission.Rmd'
    # it contains the network tables for 5 and 32 regions with added
    # geo-location of nodes, distances, estimated efficiency, and costs
    (load("data-raw/network_tables.RData"))
    transmission$reg5 <- network_r5 |> filter(region.x != region.y)
    transmission$reg32 <- network_r32 |> filter(region.x != region.y)
  } else {
    # raw matrices, use if the decired network configuration
    # is not available in the network_tables.RData.
    # Run 'transmission.Rmd' to estimate missing parameters
    # and save to the network_tables.RData
    transmission$reg5 <- fread("data-raw/trade_matrix_r5_v01.csv")
    transmission$reg32 <- rbind(
      fread("data-raw/trade_matrix_r32_v01.csv"),
      fread("data-raw/trade_matrix_r32_v02.csv"),
      fread("data-raw/trade_matrix_r32_v03.csv")
    )
  }
  ideea_data$transmission <- transmission
  usethis::use_data(ideea_data, internal = F, overwrite = T)
}

### electricity.Rmd ####
if (F) { # run manually
  # read the package data and replace modules
  library(IDEEA)
  ideea_modules <- IDEEA::ideea_modules
  names(ideea_modules)
  names(ideea_modules$electricity)
  # 5-region version
  elc_reg5 <- load("data-raw/electricity_reg5.RData")
  elc_reg5 <- get(elc_reg5)
  names(elc_reg5)
  # 32-region  version
  elc_reg32 <- load("data-raw/electricity_reg32.RData")
  elc_reg32 <- get(elc_reg32)
  names(elc_reg32)
  # update/replace
  ideea_modules$electricity <- list(
    reg5 = elc_reg5,
    reg32 = elc_reg32
  )
  names(ideea_modules$electricity)
  # save to 'data/ideea_modules.rda'
  usethis::use_data(ideea_modules, internal = F, overwrite = T)
  # rebuild the package
}

### Karnataka data ####
# run once to update Karnataka dataset from (updated) raw data
if (F) {
  # read the package data and replace modules
  library(IDEEA)
  library(tidyverse)
  library(data.table)
  library(sf)
  ideea_data <- IDEEA::ideea_data

  karnataka <- list()

  # import Karnataka data

  ## capacity and annual generation
  (load("tmp/data/Karnataka/PTC/LoadCurve/generation.RData"))
  glimpse(gen)

  baseYear <- 2019

  ii <- gen$year %in% baseYear # &
  # !grepl("(total)|(Small Thermal)|(Mini Thermal)",
  #        gen$stations, ignore.case = T) &
  # gen$stations != "2) Co-Gen"
  unique(gen[ii, ]$stations)
  unique(gen[ii, ]$enSource)

  gen_cap <- gen[ii, ] %>%
    # filter(year == modYears) %>%
    group_by(stations, enSource, year) %>%
    summarise(
      cap_MW = mean(cap_MW, na.rm = T),
      # max_MU = max(max_MU, na.rm = T),
      gen_MU = sum(gen_MU, na.rm = T),
      .groups = "drop"
    ) %>%
    # gen_MU = 365 * mean(gen_MU, na.rm = T)) %>%
    ungroup() %>%
    group_by(year, enSource) %>%
    summarise(
      cap_MW = sum(cap_MW, na.rm = T),
      # max_MU = sum(max_MU, na.rm = T),
      gen_MU = sum(gen_MU, na.rm = T),
      .groups = "drop"
    ) %>%
    mutate(
      af = gen_MU / cap_MW / 365 / 24 * 1e3,
      type = tolower(enSource))
  # update solar capacity
  gen_cap$cap_MW[gen_cap$enSource == "Solar"] <- c(7038)
  gen_cap

  karnataka$capacity_MW <- gen_cap

  ## hourly generation
  (load(file.path("tmp/data/Karnataka/KPTCLSLDC_direct/gen_hourly_agg0.RData")))
  gen_agg0 <- filter(gen_agg0, year(date) == 2019)

  gen_agg0_y <- gen_agg0 %>%
    group_by(type) %>%
    summarise(GWh = sum(GWh))
  gen_agg0_y; dplyr::select(gen_cap, type, cap_MW, gen_MU, af) # compare

  karnataka$generation_hourly <- as.data.table(gen_agg0)

  ## reservoir operation
  (load("tmp/data/Karnataka/PTC/rvr_agg.RData"))
  karnataka$reservoir <- as.data.table(rvr_agg)

  ## load curve
  (load("tmp/data/Karnataka/PTC/LoadCurve/load_curve.RData"))
  load
  load_curve <- load; rm(load)
  (rng <- range(load_curve$datetime)) # range of the data

  dem_YY <- filter(load_curve, year == baseYear)
  ii <- is.na(dem_YY$MWh)
  summary(ii)

  if (sum(ii) > 0) {
    patch <- load_curve[load_curve$year == baseYear - 1, ] %>%
      mutate(datetime = datetime + years(1))
    dem_YY <- filter(dem_YY, !is.na(MWh)) %>%
      full_join(
        filter(patch, datetime %in% dem_YY$datetime[ii])
      )
    ii <- is.na(dem_YY$MWh)
    summary(ii)
  }

  dem_YY <- dem_YY %>%
    mutate(slice = dtm2tsl(datetime)) %>%
    filter(!grepl("d366", slice)) |>
    select(datetime, slice, year, MWh)
  dim(dem_YY)
  karnataka$load_curve <- as.data.table(dem_YY)

  ## weather data
  # wind potential by hours
  (load("tmp/data/karnataka/merra_wnd_mean.RData"))
  weaYears <- 2019
  merra_wnd_mean <- filter(merra_wnd_mean, wYear %in% weaYears)
  merra_wnd_mean

  target <- "AF35"
  merra_wnd_mean <- filter(merra_wnd_mean,af120m_class %in% target)

  # solar potential by hours
  (load("tmp/data/karnataka/merra_sol_mean.RData"))
  merra_sol_mean <- filter(merra_sol_mean, wYear %in% weaYears)
  merra_sol_mean

  # spatial installation potential (GW)
  (load("tmp/data/Karnataka/merra_tot.RData"))
  merra_tot

  sol_max_GW <- merra_tot %>%
    group_by(region) %>%
    summarise(max_GW = sum(sol_GW))

  win_max_GW <- merra_tot %>%
    group_by(region, af120m_class) %>%
    summarise(max_GW = sum(win_GW ))

  # state-wide aggregation for existing capacity
  # should be replaced when data of wind power plant
  # locations in available
  # merra_wnd50_state_average <- merra_wnd_mean %>%
  #   ungroup() %>%
  #   group_by(region, wYear, mYear, slice) %>%
  #   summarize(af50m = mean(af50m), .groups = "drop")
  # dim(merra_wnd50_state_average)

  karnataka$merra_CF_mean <- merra_tot |> as.data.table()

  karnataka$wind_cf <- merra_wnd_mean |>
    rename(wcf_120m = af120m, wcf_50m = af50m) |>
    as.data.table()

  karnataka$solar_cf <- merra_sol_mean |>
    rename(scf = afsol) |>
    as.data.table()

  karnataka$wind_max_GW <- win_max_GW |> as.data.table()
  karnataka$solar_max_GW <- sol_max_GW |> as.data.table()

  (load("tmp/data/Karnataka/WRI/wind_KA.RData"))
  karnataka$wind_loc_sf <- sf::st_as_sf(wind_KA)

  names(karnataka)
  ideea_data$karnataka <- karnataka
  usethis::use_data(ideea_data, internal = F, overwrite = T)
}
