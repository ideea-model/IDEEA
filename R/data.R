#' Get IDEEA map with custom number of regions
#'
#' @param nreg integer, number of regions, one of 1, 5, 7, 32, 34, 36, 37 or 46
#' @param offshore logical, if TRUE, offshore area with associated regions will be returned
#' @param islands logical, should "Andaman and Nicobar" ("AN") and "Lakshadweep" ("LD") be included.
#' @param aggregate logical, if true, geometries will be aggregated by region.
#' @param rename logical, if TRUE, the `region` and `name` will be returned instead of `reg{nreg}` and `name{nreg}`
#' @param reg_off logical, if TRUE, `reg_off` or `reg{nreg}_off` column will be added with added `_off` to the names of offshore areas/regions.
# @param ROW logical, if TRUE, an additional geometry with neighbour countries' land within 67-98 latitude and 5-38 longitude (ROW - the Rest of the World) will be added.
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
get_ideea_map <- function(nreg = 7,
                          offshore = FALSE,
                          islands = FALSE,
                          # ROW = FALSE,
                          aggregate = (nreg != 46),
                          rename = TRUE,
                          reg_off = offshore,
                          ...) {
  map <- IDEEA:::ideea_map
  nms <- paste0(c("reg", "name"), nreg)
  if (!all(nms %in% names(map))) {
    stop("Allowed values of nreg: 1, 5, 7, 32, 34, 36, or 46.
         (nreg = ", nreg, ")")
  }

  if (!offshore) {
    map <- map %>% dplyr::filter(!offshore)
  }
  if (!islands) {
    map <- map %>% dplyr::filter(mainland)
  }
  # if (!ROW)
  {
    map <- map %>% dplyr::filter(!grepl("ROW", reg1, ignore.case = T))
  }

  # browser()
  if (aggregate) {
    map <- map %>%
      dplyr::group_by(across(
        dplyr::all_of(c(nms, "mainland", "offshore", "reg1", "name1"))
      )) %>%
      dplyr::summarize(.groups = "drop")
  }

  if (reg_off) {
    map <- map %>%
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
  return(map)
}
# !!! ToDo !!!
# 2. merge merra-grid in offshore
# 3. provide merra-grid for all regions & offshore

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

#' Get data from IDEEA database
#'
#' @param name character, name of data table, one of: "coal", "oil", "gas", "biomass", "lignite", ...
#' @param nreg integer, number of region to return
#' @param variable character, regular expression for the name of variable(s) to return.
#' @param agg_fun character, function to aggregate data by region, default "sum" (for volumes), "mean" is advised for costs variable.
#' @param raw logical, should the raw table be returned, FALSE by default
#' @param offshore logical, should the data for offshore areas be returned, FALSE by default
#' @param islands logical, should the data for remote islands be returned, FALSE by default
#'
#' @return returns the requested table or NULL.
#' @export
#'
#' @examples
#' get_ideea_data("coal", raw = T)
#' get_ideea_data("coal", nreg = 7, "reserve")
#' get_ideea_data("oil", nreg = 34, "reserve", islands = T)
#' get_ideea_data("coal", nreg = 7, "cost", agg_fun = mean)
#' get_ideea_data("merra_raw_2014", raw = T) %>% head()
get_ideea_data <- function(
    name,
    nreg = 7,
    variable = ".",
    sets = "^reg[0-9]+$|^slice$|^date$|^datetime|^MONTH$|^YDAY$|^HOUR$",
    agg_fun = sum,
    raw = FALSE,
    offshore = FALSE,
    islands = FALSE,
    as_DT = TRUE,
    drop_na = TRUE,
    rename = TRUE
    # ROW = FALSE
    ) {
  # browser()
  reg <- paste0("reg", nreg)
  # suppressMessages({
  x <- ideea_data[[name]]
  if (raw) {
    if (as_DT) x <- data.table::as.data.table(x)
    return(x)
  }
  reg_in_x <- names(x)[grepl("^reg[0-9]+(|_off)", names(x))]
  if (nreg != 1) {
    reg_in_x <- reg_in_x[reg_in_x != "reg1"]
    x <- select(x, -any_of(c("reg1", "reg1_off")))
  }
  variable <- sapply(variable, function(v) names(x)[grepl(v, names(x))])
  variable <- variable[!grepl(sets, variable)]
  names(variable) <- NULL
  ii <- sapply(variable, function(j) inherits(x[[j]], c("numeric", "integer")))
  sets_in_x <- variable[!ii]
  variable <- variable[ii]
  sets_in_x <- c(sets_in_x, names(x)[grepl(sets, names(x))])
  names(sets_in_x) <- NULL
  x <- select(x, any_of(variable), matches(sets_in_x))
  if (is.null(x[["offshore"]])) {
    # warning("The '", name, "' table doesn't have `offshore` column.\n",
    #         "   Assuming 'offshore = FALSE'")
    x$offshore <- FALSE
  }
  # if (raw) {
  #   if (as_DT) x <- data.table::as.data.table(x)
  #   return(x)
  # }
  # sets
  # browser()
  y <- ideea_data$reg_tbl |>
    filter(reg1 != "ROW") |>
    select(matches(sets_in_x), any_of(c("offshore", "mainland", reg))) |>
    filter(!is.na(.data[[reg]])) |>
    unique()
  # if (!ROW) {
  # y <- filter(y, reg1 != "ROW") |> unique()
  # }
  if (drop_na) join_fun <- dplyr::left_join else join_fun <- dplyr::full_join
  # browser()
  cross_cols <- names(x)[names(x) %in% names(y)]
  x_try <- try(
    {
      x %>%
        # dplyr::left_join(ideea_data$reg_tbl) %>%
        join_fun(y, relationship = "many-to-one", by = cross_cols) |>
        dplyr::filter(!is.na(.data[[reg]])) %>%
        dplyr::group_by(
          dplyr::across(
            dplyr::any_of(c(reg, sets_in_x))
          ) # , "offshore", "mainland"
        )
    },
    silent = T
  )
  if (inherits(x_try, "try-error")) {
    message(
      "Cannot automatically aggregate table ", name, " to ", nreg,
      " regions. Most likely a disagregation algorighm is required.\n",
      "Use 'raw = TRUE' to request unprocessed data."
    )
    return(NULL)
  } else {
    x <- x_try
    rm(x_try)
  }
  if (offshore) {
    x <- group_by(x, offshore, .add = T)
  } else {
    x <- filter(x, !offshore)
  }
  if (islands) {
    x <- dplyr::group_by(x, mainland, .add = T)
  } else {
    x <- dplyr::filter(x, mainland)
  }

  # browser()
  x <- x |>
    ungroup() |>
    select(-any_of(reg_in_x[reg_in_x != reg])) |>
    group_by(across(all_of(reg))) |>
    group_by(across(any_of(sets_in_x)),
      .add = T
    )

  # group by non-numeric variables
  # browser()
  ii <- sapply(variable, function(i) inherits(x[[i]], c("numeric", "integer")))
  # exclude non-requested region
  # grepl()
  if (length(variable[!ii]) > 0) {
    x <- x |>
      dplyr::group_by(dplyr::across(dplyr::any_of(variable[!ii])), .add = T)
  }
  if (length(variable[ii]) == 0) {
    # return table without numeric info
    x <- select(x, all_of(group_vars(x)))
    if (as_DT) x <- data.table::as.data.table(x)
    return(x)
  }
  x <- x %>%
    dplyr::summarise_at(dplyr::vars(dplyr::matches(variable[ii])),
      agg_fun,
      na.rm = T
    ) %>%
    dplyr::ungroup()
  # })
  if (rename) {
    # browser()
    nm <- names(x)
    nm[nm == reg] <- "region"
    names(x) <- nm
  }
  if (as_DT) x <- data.table::as.data.table(x)
  return(x)
}

if (F) {
  # library(data.table)
  get_ideea_data("coal", raw = T)
  get_ideea_data("coal", nreg = 7, "reserve")
  get_ideea_data("coal", nreg = 7, "reserve", islands = T)
  get_ideea_data("coal", nreg = 7, "cost", agg_fun = mean)
  get_ideea_data("coal", nreg = 32, "cost", agg_fun = mean, offshore = T)
  get_ideea_data("coal",
    nreg = 7, "cost", agg_fun = mean, offshore = F,
    islands = T
  )

  ideea_data %>% names()

  get_ideea_data("merra_raw_2014", raw = T) %>% head()
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
