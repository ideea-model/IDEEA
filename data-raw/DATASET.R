## The script reads "raw" data pre-saved in /data-raw and
## creates the package embedded dataset

## IDEEA modules
## from energy.Rmd
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
    reg5 = energy_r32,
    reg32 = energy_r32
  )
  ideea_modules$energy <- energy
  names(ideea_modules$energy)
  # save to 'data/ideea_modules.rda'
  usethis::use_data(ideea_modules, internal = F, overwrite = T)
}

# usethis::use_data(DATASET, overwrite = TRUE)
