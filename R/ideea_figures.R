tech_type_levels <- c(
  "Coal", "Coal CCS", "Gas", "Natural Gas", "Gas CCS", "Nuclear", "Hydro", "Biomass",
  "Wind", "Solar", "Battery", "Storage", "Storage-Inp", "Storage-Out"
) |> rev()

#' Add 'tech_type' column to a data.frame
#'
#' @param x data.frame that has 'process' column
#' @param factors logical, if TRUE, the 'tech_type' column will have class
#' @param force logical, if TRUE will add unknown types of technologies to the factor level, or report the 'unknown types' error if FALSE.
#'
#' @return x data.frame with added or overwritten 'tech_type' column.
#' @export
#'
#' @examples
add_tech_type <- function(x, factors = TRUE, force = TRUE) {
  x <- x |>
    mutate(tech_type = process) |>
    mutate(
      tech_type = if_else(grepl("^ECOA_CCS_(FX|FL)(?:_\\d+)?$", tech_type),
                          "Coal CCS", tech_type),
      tech_type = if_else(grepl("^ECOA(|SUB|SUP|ULT)(?:_\\d+)?$", tech_type),
                          "Coal", tech_type),
      tech_type = if_else(grepl("^(EGAS|ENGOC|ENGCC|ENGEN)(?:_\\d+)?$", tech_type),
                          "Gas", tech_type),
      tech_type = if_else(grepl("^EGAS_CCS_(FX|FL)(?:_\\d+)?$", tech_type),
                          "Gas CCS", tech_type),
      tech_type = if_else(grepl("^EBIO(?:_\\d+)?$", tech_type),
                          "Biomass", tech_type),
      tech_type = if_else(grepl("^(EWIN|EWIF)(?:_\\d+)?$", tech_type),
                          "Wind", tech_type),
      tech_type = if_else(grepl("^ESOL(?:_\\d+)?$", tech_type),
                          "Solar", tech_type),
      tech_type = if_else(grepl("^EHYD(?:_\\d+)?$", tech_type),
                          "Hydro", tech_type),
      tech_type = if_else(grepl("^ENUC(?:_\\d+)?$", tech_type),
                          "Nuclear", tech_type),
      tech_type = if_else(grepl("^STG_BTR(?:_\\d+)?$", tech_type),
                          "Battery", tech_type)
    )
  if (factors) {
    x_levels <- x$tech_type |> unique()
    if (all(x_levels %in% tech_type_levels) || force) {
      x <- x |>
        mutate(tech_type = factor(tech_type, levels = unique(c(tech_type_levels, x_levels)),
                                  ordered = TRUE))
    } else {
      stop("Unknown technology types")
    }
  }
  x
}


#' Plot or return data for hourly operation of electricity sector
#'
#' @param scen solved scenario object
#' @param YEAR integer vector of years to return
#' @param SLICE character vector of time-slices to return
#' @param return_data logical, if TRUE, the data will be return, or gglot object otherwise.
#'
#' @return ggplot object or data.table with data for the figure.
#' @export
#'
#' @examples
#' # all_slices <- scen_CAP@settings@calendar@timetable$slice
#' # ideea_snapshot(scen_CAP, YEAR = 2055, SLICE = all_slices[grepl("d043", all_slices)])
#' # ideea_snapshot(scen_CAP, YEAR = 2055, SLICE = all_slices[grepl("d015", all_slices)])
ideea_snapshot <- function(scen, YEAR, SLICE, return_data = FALSE) {

  h <- getHorizon(scen)
  if (!any(YEAR %in% h@intervals$mid)) {
    stop("YEAR is not in the scenario range")
  }

  if (!any(SLICE %in% scen@settings@calendar@timetable$slice)) {
    stop("SLICE is not in the scenario range")
  }

  # browser()
  vTechOut_ELC <- getData(
    list(scen_CAP), "vTechOut", process = T, merge = T,
    year = YEAR, slice = SLICE,
    comm = "ELC", tech_ = "^E", drop.zeros = T, digits = 1) |>
    drop_process_vintage() |>
    as.data.table()

  vStorageOut_ELC <- getData(
    list(scen), "vStorageOut", process = T, merge = T,
    year = YEAR, slice = SLICE,
    comm = "ELC", drop.zeros = T, digits = 1) |>
    # mutate(process = "Storage-Out") |>
    drop_process_vintage() |>
    as.data.table()

  vStorageInp_ELC <- getData(
    list(scen), "vStorageInp", process = T, merge = T,
    year = YEAR, slice = SLICE,
    comm = "ELC", drop.zeros = T, digits = 1) |>
    # mutate(process = "Storage-Inp") |>
    drop_process_vintage() |>
    as.data.table()

  vStorageIO_ELC <- rbind(
    vStorageOut_ELC,
    mutate(vStorageInp_ELC, value = -value)
  )
  # browser()
  vDailyOp <- rbindlist(
    list(
      vTechOut_ELC,
      vStorageIO_ELC
    ), use.names = T, fill = T) |>
    mutate(
      datetime = tsl2dtm(slice, year = year, tmz = "Asia/Kolkata")
    ) |>
    add_tech_type() |>
    # group_by(scenario, te)
    group_by(scenario, name, tech_type, comm, region, year, slice, datetime) |>
    summarise(value = sum(value), .groups = "keep")
  # complete()

  if (return_data) return(vDailyOp)

  vDailyOp |>
    group_by(scenario, region, tech_type, year, datetime) |>
    summarise(value = sum(value), .groups = "keep") |>
    ggplot() +
    # geom_area(aes(datetime, value, fill = tech_type)) +
    geom_col(
      aes(datetime, value, fill = tech_type),
      position = position_stack(reverse = F)) +
    scale_fill_viridis_d(option = "H", direction = 1, name = "Process") +
    theme_bw() +
    labs(x = "", y = "GWh")
}

