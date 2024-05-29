#' Get IDEEA map with custom number of regions
#'
#' @param nreg integer, number of regions, one of 1, 5, 7, 32, 34, 36, 37 or 46
#' @param offshore logical, if TRUE, offshore area with associated regions will be returned
#' @param islands logical, should "Andaman and Nicobar" ("AN") and "Lakshadweep" ("LD") be included.
#' @param aggregate logical, if true, geometries will be aggregated by region.
#' @param rename logical, if TRUE, the `region` and `name` will be returned instead of `reg{nreg}` and `name{nreg}`
#' @param reg_off logical, if TRUE, `reg_off` or `reg{nreg}_off` column will be added with added `_off` to the names of offshore areas/regions.
#' @param merge_islands logical, `mainland` column should be dropped (if TRUE) or added (FALSE) to the returned sf-object.
#' @param ...
#'
#' @return IDEEA map with in 'sf' or 'sp' format
#' @export
#'
#' @examples
#' library(sf)
#' gis_sf <- get_ideea_map(nreg = 1, offshore = F, islands = T)
#' plot(gis_sf[1], key.width = lcm(4))
#' gis_sf <- get_ideea_map(nreg = 7, offshore = T, islands = T, reg_off = T)
#' plot(gis_sf["region"], key.width = lcm(4))
#' plot(gis_sf["reg_off"], key.width = lcm(4))
#' gis_sf <- get_ideea_map(nreg = 32, offshore = T, islands = T)
#' plot(gis_sf[1], key.width = lcm(4))
get_ideea_map <- function(
    nreg = 5,
    offshore = FALSE,
    islands = TRUE,
    # ROW = FALSE,
    aggregate = (nreg != 46),
    rename = FALSE,
    reg_off = offshore,
    merge_islands = TRUE,
    ...) {
  # @param ROW logical, if TRUE, an additional geometry with neighbour countries' land within 67-98 latitude and 5-38 longitude (ROW - the Rest of the World) will be added.
  # browser()
  map <- IDEEA:::ideea_map
  nms <- paste0(c("reg", "name"), nreg)
  if (!all(nms %in% names(map))) {
    stop("Allowed values of nreg: 1, 5, 7, 32, 34, 36, or 46.
         (nreg = ", nreg, ")")
  }

  if (!offshore) {
    map <- map |> dplyr::filter(!offshore)
  }
  if (!islands) {
    map <- map |> dplyr::filter(mainland)
  }
  # if (!ROW)
  {
    map <- map |> dplyr::filter(!grepl("ROW", reg1, ignore.case = T))
  }

  if (aggregate) {
    if (merge_islands) {
      map <- map |> select(-any_of("mainland"))
    }
    map <- map |>
      dplyr::group_by(across(
        dplyr::any_of(c(nms, "mainland", "offshore", "reg1", "name1"))
      )) |>
      dplyr::summarize(
        geometry = sf::st_union(geometry),
        .groups = "drop"
        ) |>
      sf::st_make_valid()
  }

  if (reg_off) {
    map <- map |>
      dplyr::mutate(
        reg_off = map[[nms[1]]],
        reg_off = dplyr::if_else(offshore, paste0(reg_off, "_off"), reg_off),
        .after = nms[2]
      )
  }

  nm <- names(map)
  if (rename) {
    nm[nm == nms[1]] <- "region"
    nm[nm == nms[2]] <- "name"
  } else if (reg_off) {
    nm[nm == "reg_off"] <- paste0(nms[1], "_off")
  }
  names(map) <- nm
  map <- select(
    map,
    any_of(c("region", "reg_off", "name", "mainland", "offshore")),
    any_of(c(glue("reg{nreg}"), glue("reg{nreg}_off"), glue("name{nreg}")))
    # any_of(c("reg1", "reg1_off", "name1"))
  )
  return(map)
}


if (F) {
  # library(sf)
  gis_sf <- get_ideea_map(nreg = 1, offshore = F, islands = T)
  plot(gis_sf[1], key.width = lcm(4))
  gis_sf <- get_ideea_map(nreg = 7, offshore = T, islands = T, reg_off = T)
  plot(gis_sf["region"], key.width = lcm(4))
  plot(gis_sf["reg_off"], key.width = lcm(4))
  gis_sf <- get_ideea_map(nreg = 32, offshore = T, islands = T)
  plot(gis_sf[1], key.width = lcm(4))
  gis_sf <- get_ideea_map(nreg = 46, offshore = T, islands = T)
  plot(gis_sf[1], key.width = lcm(4))
}

#' Get data from IDEEA package embedded dataset
#'
#' @param name character, name of data table, one of: "coal", "oil", "gas", "biomass", "lignite", ...
#' @param nreg integer, number of region to return
#' @param variable character, regular expression for the name of variable(s) to return.
#' @param agg_fun character, function to aggregate data by region, default "sum" (for volumes), "mean" is advised for costs variable.
#' @param raw logical, should the raw table be returned, FALSE by default
#' @param offshore logical, should the data for offshore areas be returned, FALSE by default
#' @param islands logical, should the data for remote islands be returned, FALSE by default
#' @param sets character, a regular expression to identify key-columns in the datasets, used for grouping. The default value (`IDEEA:::.ideea_sets_pattern`) covers all sets in the embedded to the package data. If new datasets added with different names of sets, the default value has to be reviewed.
#' @param as_DT logical, results will be returned in `data.table` format if TRUE (default)
#' @param drop_na logical, should `NA` values be dropped from the data
#' @param rename
#' @param reg_tbl data.frame with regional mapping (for custom weights or regions)
#' @param weight character name of column to use as disaggregating weights if the disaggregation of the data is required, for example the saved data is by 5 regions, but the requested data is for 32 regions. In general, it is not recommended to disaggregate due to the strong assumptions. But the algorithm is also used to convert data from 36 to 32 regions because of not exact match of regions' shapes (see [Regions](https://ideea-model.github.io/IDEEA/articles/regions.html) article). The default value is the area of the region (`"km^2"`) for `agg_fun = sum` and it is equal to `1` (no weights) for `agg_fun = mean` and all other functions.
#'
#' @return returns the requested table or NULL.
#' @export
#'
#' @examples
#' get_ideea_data("coal", raw = T)
#' get_ideea_data("coal", nreg = 7, "reserve")
#' get_ideea_data("oil", nreg = 34, "reserve", islands = T)
#' get_ideea_data("coal", nreg = 7, "cost", agg_fun = mean)
#' get_ideea_data("merra_raw_2014", raw = T) |> head()
get_ideea_data <- function(
    name,
    nreg = 5,
    variable = ".",
    sets = .ideea_sets_mask,
    agg_fun = sum,
    raw = FALSE,
    offshore = FALSE,
    islands = TRUE,
    as_DT = TRUE,
    drop_na = TRUE,
    rename = FALSE,
    reg_tbl = ideea_data$reg_tbl,
    weight = ifelse(identical(agg_fun, sum), "km2", 1)
    # ROW = FALSE
    ) {
  # browser()
  regN <- paste0("reg", nreg)

  x <- ideea_data[[name]]
  if (raw) {
    if (as_DT) x <- data.table::as.data.table(x)
    return(x)
  }

  if (offshore) {
    if (is.null(x[["offshore"]])) x$offshore <- FALSE # default
  } else if (!offshore) {
    if (!is.null(x[["offshore"]])) x <- filter(x, !offshore)
  }

  if (!is.null(x[["mainland"]]) && !islands) {
    x <- filter(x, mainland)
  }

  reg_in_x <- names(x)[grepl("^reg[0-9]+(|_off)", names(x))]

  if (length(reg_in_x) == 0) {
    region_in_x <- names(x)[grepl("^reg(|ion)(|_off)$", names(x),
                                  ignore.case = T)]
    if (length(region_in_x) > 0) {
      stop("Region column must have numeric index (aka 'reg5', 'reg36' etc.)",
           "\n", "Columns in data: " , paste(region_in_x, sep = ", "))
    }
    stop("Cannot identify region")
  }

  if (nreg != 1 && length(reg_in_x) > 1) {
    reg_in_x <- reg_in_x[reg_in_x != "reg1" & reg_in_x != "reg1_off"]
    x <- select(x, -any_of(c("reg1", "reg1_off")))
  }
  # find matching names
  variable <- sapply(variable, function(v) names(x)[grepl(v, names(x))])
  # filter out sets
  variable <- variable[!grepl(sets, variable)]
  names(variable) <- NULL
  # filter out non-numeric
  ii <- sapply(variable, function(j) inherits(x[[j]], c("numeric", "integer")))
  sets_in_x <- variable[!ii] # add all non-numeric columns to sets
  variable <- variable[ii]
  sets_in_x <- c(sets_in_x, names(x)[grepl(sets, names(x))])
  names(sets_in_x) <- NULL
  sets_in_x_noReg <- sets_in_x[!grepl("^reg[0-9]+(|_off)$", sets_in_x)] |>
    unique()
  x <- select(x, any_of(variable), matches(sets_in_x))

  x_regN <- x |>
    select(matches("^reg[0-9]+$")) |>
    colnames()
  x_nreg <- str_extract(x_regN, "[0-9]+") |> as.integer()

  # if (regN %in% x_regN) {
  #   # exact match, no (dis)aggregation is needed
  #   warning("dev/ToDo: skip excessive (dis)aggregation for exact match data")
  #   # ii <- grepl("^reg[0-9]+$", sets_in_x) & !(sets_in_x %in% regN)
  # }

  # sets
  # browser()
  # get aggregation table
  y <- reg_tbl |>
    filter(reg1 != "ROW") |>
    select(
      matches(sets_in_x),
      # matches(sets),
      matches("reg46"),
      any_of(c("offshore", "mainland", regN))
      # any_of(weight, "reg46")
    ) |>
    filter(!is.na(.data[[regN]])) |>
    unique()

  # check if dis aggregation is required
  # rows_xy <- intersect(colnames(x), colnames(y))
  # rows_xy <- rows_xy[grepl("^reg[0-9]+$", rows_xy)]
  nrow_y <- y |>
    select(matches("^reg[0-9]+$")) |>
    unique() |>
    nrow()
  nrow_x <- x |>
    select(matches("^reg[0-9]+$")) |>
    unique() |>
    nrow()

  if (drop_na) join_fun <- dplyr::left_join else join_fun <- dplyr::full_join

  # browser()

  sets_in_y <- select(y, matches(sets)) |> colnames()
  sets_in_y_noReg <- sets_in_y[!grepl("^reg[0-9]+(|_off)$", sets_in_y)] |>
    unique()
  sets_in_y <- c(sets_in_y_noReg, regN, x_regN, "reg46") |> unique()

  # y |> filter(!(reg32 == reg36))


  regN_vs_xregN <- y |> ungroup() |> filter(get(regN) != get(x_regN))

  if (nrow(regN_vs_xregN) == 0) {
    # exact match, no (dis-)aggregation required

  } else {
    # if (identical(agg_fun, sum))
    regY_in_regX <- regN_vs_xregN |>
      select(-any_of("reg46")) |>
      unique() |> select(-any_of(regN)) |>
      group_by(across(any_of(c(sets_in_x)))) |>
      summarize(cn = dplyr::n()) |>
      as.data.table()

    regX_in_regY <- regN_vs_xregN |>
      select(-any_of("reg46")) |>
      unique() |> select(-any_of(x_regN)) |>
      group_by(across(any_of(c(sets_in_y)))) |>
      summarize(cn = dplyr::n()) |>
      as.data.table()

    # if (any(regX_in_regY$cn > 1) & any(regY_in_regX$cn > 1)) {
    if (any(regY_in_regX$cn > 1)) {
    # dis-aggregation and aggregation 'regX' -> 'reg46' -> 'regN' required
      x <- disagg_to_reg46(x, variable, from = x_regN, weight = weight)
      # dx <- dx |>
      #   join_fun(y, by = intersect(names(dx), names(y))) |>
      #   filter(!is.na(get(regN))) |>
      #   group_by(across(all_of(c(regN, sets_in_y_noReg))))
        # summarise(across(variable, agg_fun)) |>
        # as.data.table()
    # } else if (any(regX_in_regY$cn > 1)) {
      # disaggregation regX -> regN
    # } else if (any(regY_in_regX$cn > 1)) {
    #   # aggregation regX -> regN
    #   dx <- x |>
    #     group_by()
    # } else {
    #   # should not be here - possible error in data
    #   stop("(Dis)aggregation is not supported for the data,",
    #        " use raw = TRUE for unmodified dataset")
    }

    # if (offshore) {
    #   x <- group_by(x,across(any_of("offshore")), .add = T)
    # } else {
    #   gcol <- group_vars(x); gcol <- gcol[!(gcol == "offshore")]
    #   x <- x |> ungroup() |> filter(!offshore) |>
    #     group_by(across(any_of(gcol)))
    # }
    # if (islands) {
    #   x <- dplyr::group_by(x, across(any_of("mainland")), .add = T)
    # } else {
    #   gcol <- group_vars(x); gcol <- gcol[!(gcol == "mainland")]
    #   x <- x |> ungroup() |> filter(!offshore) |>
    #     group_by(across(any_of(gcol)))
    # }

    # aggregate
    x <- x |>
      join_fun(y, by = intersect(names(x), names(y))) |>
      filter(!is.na(get(regN)))
      # group_by(across(all_of(c(regN, sets_in_y_noReg)))) |>
      # summarise(across(variable, agg_fun, na.rm = TRUE)) |>
      # as.data.table()
  }

  if (drop_na) {
    rr <- ungroup(x) |>
      select(any_of(variable)) |>
      as.matrix() |>
      apply(1, function(r) all(is.na(r) | is.nan(r)))
    x <- x[!rr,]
  }

  if (offshore) {
    x <- group_by(x,across(any_of("offshore")), .add = T)
  } else {
    gcol <- group_vars(x); gcol <- gcol[!(gcol == "offshore")]
    x <- x |> ungroup() |> filter(!offshore) |>
      group_by(across(any_of(gcol)))
  }

  if (islands) {
    x <- dplyr::group_by(x, across(any_of("mainland")), .add = T)
  } else {
    gcol <- group_vars(x); gcol <- gcol[!(gcol == "mainland")]
    x <- x |> ungroup() |> filter(!offshore) |>
      group_by(across(any_of(gcol)))
  }

  # browser()
  if (nrow(regN_vs_xregN) > 0) {
    # aggregate
    x <- x |>
      group_by(across(any_of(c(regN, sets_in_y_noReg, sets_in_x_noReg)))) |>
      summarise(across(variable, ~ agg_fun(.x, na.rm = drop_na))) |>
      as.data.table()
  }
  # sets_in_x_noReg

  if (rename) {
    # browser()
    nm <- names(x)
    nm[nm == regN] <- "region"
    names(x) <- nm
  }
  if (as_DT) x <- data.table::as.data.table(x)
  return(x)
}

if (F) {
  # tests ####
  # library(data.table)
  get_ideea_data("coal", raw = T)
  get_ideea_data("coal", nreg = 5, "reserve")
  get_ideea_data("coal", nreg = 5, "reserve", islands = T)
  get_ideea_data("coal", nreg = 5, "cost", agg_fun = mean)
  get_ideea_data("coal", nreg = 32, "reserve", agg_fun = sum, offshore = T)
  get_ideea_data("coal", nreg = 32, "cost", agg_fun = mean, offshore = T)
  get_ideea_data("coal",
    nreg = 5, "cost", agg_fun = mean, offshore = F,
    islands = T
  )

  get_ideea_data(name = "ccs_r5", nreg = 32)
  get_ideea_data(name = "ccs_r5", raw = T)$CCS_potential_GtCO2 |> sum()
  get_ideea_data(name = "ccs_r5", nreg = 32, islands = F)$CCS_potential_GtCO2 |>
    sum()
  get_ideea_data(name = "ccs_r5", nreg = 32, islands = T)$CCS_potential_GtCO2 |>
    sum()

  ideea_data |> names()

  get_ideea_data("merra_raw_2014", raw = T) |> head()
}


disagg_to_reg46 <- function(x, varname, from = "reg5", weight = "km2") {
  # browser()
  to <- "reg46"

  # disaggregation table
  wt <- ideea_data$reg_tbl
  if (isTRUE(weight == 1)) {
    weight1 <- TRUE
    weight <- ".WEIGHT"
    wt[[weight]] <- 1
  } else {
    weight1 <- FALSE
  }

  wt <- wt |>
    select(all_of(c(from, to, weight))) |>
    filter(!is.na(get(to)), !is.na(get(from)))

  wt_from <- wt |>
    group_by(across(all_of(from))) |>
    summarise(across(weight, sum), .groups = "drop") |>
    rename(wx = weight)

  wt <- wt |>
    full_join(wt_from, by = from)

  if (weight1) {
    wt <- wt |>
      mutate(w = 1) |>
      select(-any_of(c(weight, "wx")))
  } else {
    wt <- wt |>
      mutate(w = get(weight) / wx) |>
      select(-any_of(c(weight, "wx")))
  }

  wt |>
    full_join(x, by = from) |>
    mutate(
      across(all_of(varname), ~ .x * w)
    ) |>
    select(-w)
}



#' IDEEA datasets
#'
#' Embedded datasets for IDEEA models
#'
#' @format list of data frames:
#' \describe{
#'   \item{reg_tbl}{mapping table of regions}
#'   \item{load_2019_MWh}{load curve by region in 2019}
#'   \item{elc_consumption}{Historic and projected electricity consumption by region}
#'   \item{coal}{coal domestic resources by region}
#'   \item{lignite}{lignite domestic resources by region}
#'   \item{oil}{oil domestic resources}
#'   \item{gas}{natural gas domestic resources}
#'   \item{biomass}{biomass domestic resources}
#'   \item{generators}{installed power plants capacity by fuel type and region}
#'   \item{merra_raw_2014}{MERRA2 subset for 2014}
#'   ...
#' }
#' @docType data
#' @keywords datasets
#' @name ideea_data
#' @usage data(ideea_data)
#'
"ideea_data"

#' IDEEA model components
#'
#' Model modules and components
#'
#' @format list of data frames:
#' \describe{
#'   \item{energy}{Primary energy supply module (see article `energy`)}
#'   \item{electricity}{Electric power sector module}
#'   ...
#' }
#'
"ideea_modules"

# get_ideea_cf <- function(nreg = 5, YEAR = 2019, ) {
#
# }

.ideea_sets_mask <- "^reg[0-9]+$|^slice$|^date$|^datetime|^MONTH$|^YDAY$|^HOUR$|^year$|^mainland$|^offshore$"
