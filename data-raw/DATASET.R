## The script reads "raw" data pre-saved in /data-raw and
## creates the package embedded dataset

## IDEEA modules ####
### from energy.Rmd ####
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

### calendars.rmd
# update calendars
if (F) {
  library(IDEEA)
  ideea_modules <- IDEEA::ideea_modules
  names(ideea_modules)
  names(ideea_modules$calendars)
  cld <- load("data-raw/calendars.RData")
  calendars <- get(cld)
  names(calendars)
  ideea_modules$calendars <- calendars
  names(ideea_modules$calendars)
  # save to 'data/ideea_modules.rda'
  usethis::use_data(ideea_modules, internal = F, overwrite = T)
  # rebuild the package
}


### from ccus.Rmd ####
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

### Transmission ####
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

### from electricity.Rmd ####
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
