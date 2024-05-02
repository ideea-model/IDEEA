#' Return path to the IDEEA extra-data directory
#'
#' @param subdir character, the name or path to the sub-directory in the IDEEA-extra folder. File name will also be accepted. The path must be relative to the IDEEA external folder.
#' @param filename character, the name of the file in the IDEEA-extra folder or subfolder if subdir is provided.
#' @param check logical, if TRUE, the function will report error if the directory or file doesn't exist.
#'
#' @return character, the path to the IDEEA external data directory or file.
#' @export
#'
#' @examples
#' ideea_extra()
#' ideea_extra("merra")
#' ideea_extra("merra", "merra_raw_2019.fst")
ideea_extra <- function(subdir = NULL, filename = NULL, check = FALSE) {
  ideea_extra <- getOption("IDEEA.extra")
  if (check & is.null(ideea_extra)) {
    stop("IDEEA external data directory is not set")
  } else if (!dir.exists(ideea_extra)) {
    stop("IDEEA external data directory doesn't exist\n       ", ideea_extra)
  }

  if (!is.null(subdir)) ideea_extra <- file.path(ideea_extra, subdir)
  if (check & !dir.exists(ideea_extra) & !file.exists(ideea_extra)) {
    stop("IDEEA external data directory doesn't exist\n       ", ideea_extra)
  }

  if (!is.null(filename)) ideea_extra <- file.path(ideea_extra, filename)
  if (check & !file.exists(ideea_extra)) {
    stop("IDEEA external data-file doesn't exist\n       ", ideea_extra)
  }

  return(ideea_extra)
}

#' Solar and wind capacity factors by year, region and cluster
#'
#' @param resource character, name of the resource: "sol" or "win"
#' @param nreg integer, the number of regions
#' @param year integer, the year of the data to return
#' @param tol numeric, the tolerance of the information loss in the clustering from 0 to 1. If 0 the number of clusters is equal to the number of locations in MERRA2 database. When tol is 1, there is only one cluster per region.
#' @param data character, the name of the data source, the directory in the `ideea_extra` directory. Currently only "merra2" is supported.
#' @param data_file character, the name of the file with the raw data in the `data` sub-directory in `ideea_extra`. Default is "merra_raw_{year}.fst", valid for MERRA2 data.
#' @param clusters_file character, the name of the file with the clusters in the `data` directory. Default is "locid_{resource}_cl_r{nreg}.RData", available in `ideea_data`.
#' @param tsl_format character, the format of the time-slices. Default is "d365_h24". If changed, the clustering will be done based on the new time-slices.
#' @param load_from_extra logical, if TRUE, the function will try to read the capacity factors from the file in the `data` sub-directory in `ideea_extra`. If the file with requested parameters exists, it will be read. If the file doesn't exist, the function will calculate the capacity factors. Default is TRUE.
#' @param save_to_extra logical, if TRUE, the function will save the estimated capacity factors to the file in the `data` sub-directory in `ideea_extra`. Default is TRUE.
#' @param overwrite logical, if TRUE, the function will overwrite the existing file with the capacity factors. Default is FALSE.
#'
#' @return data.table, the capacity factors for the requested year by region, cluster, and time-slice. The data.table contains the following columns:
#' - `locid` - the location ID from the MERRA2 database,
#' - `year` - the year of the data,
#' - `slice` - the time-slice,
#' - `cluster` - the cluster ID,
#' - `reg{nreg}` and `reg{nreg}_off` - the region ID and the offshore region ID,
#' - `wcf_*` or `scf_*` - the capacity factor for the wind or solar resource at the given height or tracking type.
#' @export
#'
#' @examples
#' # don't run
#' # default tolerance level (0.05)
#' get_ideea_cf(resource = "sol", nreg = 5, year = 2019)
#' get_ideea_cf(resource = "sol", nreg = 32, year = 2019)
#'
#' get_ideea_cf(resource = "win", nreg = 5, year = 2019)
#' get_ideea_cf(resource = "win", nreg = 32, year = 2019)
#'
#' # custom tolerance level (0.01)
#' get_ideea_cf(resource = "sol", nreg = 5, tol = 0.01, year = 2019)
#' get_ideea_cf(resource = "sol", nreg = 32, tol = 0.01, year = 2019)
#'
#' get_ideea_cf(resource = "win", nreg = 5, tol = 0.01, year = 2019)
#' get_ideea_cf(resource = "win", nreg = 32, tol = 0.01, year = 2019)
get_ideea_cf <- function(
    resource = "sol",
    # resource = "win",
    nreg = 5,
    year = 2019,
    tol = 0.05,
    data = "merra2",
    data_file = "merra_raw_{year}.fst",
    clusters_file = "locid_{resource}_cl_r{nreg}.RData",
    tsl_format = "d365_h24",
    load_from_extra = TRUE,
    save_to_extra = TRUE,
    # file_name = "auto",
    overwrite = FALSE
) {
  # browser()
  tmask <- ideea_cl_mask(tol)
  cf_file <- ideea_extra(
    subdir = data,
    filename = glue("cf_{resource}_r{nreg}_{tmask}_{tsl_format}_{year}.fst")
  )

  if (load_from_extra && file.exists(cf_file)) {
    message("Reading capacity factors data from:"); cat("  ", cf_file, "\n")
    merra_cf_cl <- fst::read_fst(cf_file, as.data.table = TRUE)
  } else if (data == "merra2") {
    # load raw MERRA2 data
    merra_file <- ideea_extra(data, glue(data_file))
    message("Reading MERRA2 data:"); cat("  ", merra_file, "\n")
    merra <- fst::read_fst(merra_file, as.data.table = TRUE)

    if (resource == "sol") {
      message("Estimating solar capacity factors (scf_*)")
      merra_cf <- merra |>
        merra2ools::fPOA(
          array.type = c("fh", "fl",
                         # "th", "tv", # rarely used & similar to other types
                         "tl", "td")) |> # solar (Plain of Array irradiance)
        mutate(
          # simplified version of capacity factors,
          # assuming pick of output when POA >= 1000 Watt/m^2
          scf_fh = round(POA.fh / 1e3, 3),
          scf_fl = round(POA.fl / 1e3, 3),
          # scf_th = round(POA.th / 1e3, 3),
          # scf_tv = round(POA.tv / 1e3, 3),
          scf_tl = round(POA.tl / 1e3, 3),
          scf_td = round(POA.td / 1e3, 3)
        ) |>
        mutate(
          # curtail cf > 1 (when POA > 1kW/m^2)
          scf_fh = if_else(scf_fh > 1, 1, scf_fh),
          scf_fl = if_else(scf_fl > 1, 1, scf_fl),
          # scf_th = if_else(scf_th > 1, 1, scf_th),
          # scf_tv = if_else(scf_tv > 1, 1, scf_tv),
          scf_tl = if_else(scf_tl > 1, 1, scf_tl),
          scf_td = if_else(scf_td > 1, 1, scf_td)
        ) |>
        select(locid, UTC, starts_with("scf_"))

      cl_file <- ideea_extra(data, glue(clusters_file))
      if (!file.exists(cl_file)) {
        message("Clustering data-file doesn't exist:\n    ", cl_file)
        return(merra_cf)
        # try *_sf.RData version
        # cl_file_sf <-
        # ideea_extra(data, glue("locid_{resource}_cl_r{nreg}_sf.RData"))
        # if (!file.exists(cl_file_sf)) {
      }
      message("Reading clusters for MERRA2 data:"); cat("  ", cl_file, "\n")
      cl_obj <- load(cl_file)
      message("Aggregating capacity factors by cluster, tolerance = ",
              100 * tol, "%")

      cl_obj <- get(cl_obj) |>
        filter(sd_loss <= tol) |>
        group_by(across(any_of(c(
          glue("reg{nreg}"),
          glue("reg{nreg}_off")
        )))) |>
        mutate(k_min = (k == min(k))) |> ungroup() |>
        filter(k_min) |> select(-k_min) |>
        as.data.table()

      merra_cf_cl <- merra_cf |>
        filter(locid %in% unique(cl_obj$locid)) |> # drop unused locations
        mutate(
          # create time-slices
          slice = energyRt::dtm2tsl(
            lubridate::with_tz(UTC, "Asia/Kolkata"),
            format = tsl_format)
        ) |>
        select(locid, slice, starts_with("scf")) |> # drop unused columns
        left_join(
          # merge with gis-data
          select(cl_obj, any_of(c(
            "locid", "region", "reg_off",
            glue("reg{nreg}"), glue("reg{nreg}_off"),
            "offshore", "cluster", "MW_max"
          ))),
          by = "locid",
          relationship = "many-to-many") |>
        # aggregate
        group_by(across(any_of(
          c("slice", glue("reg{nreg}"), glue("reg{nreg}_off"), "cluster")))
        ) |>
        summarise(
          scf_fh = round(weighted.mean(scf_fh, MW_max, na.rm = T), 3),
          scf_fl = round(weighted.mean(scf_fl, MW_max, na.rm = T), 3),
          scf_tl = round(weighted.mean(scf_tl, MW_max, na.rm = T), 3),
          scf_td = round(weighted.mean(scf_td, MW_max, na.rm = T), 3),
          # geometry = st_union(geometry),
          .groups = "drop"
        ) |>
        mutate(year = year, .before = 1) |>
        as.data.table()
      # merra_cf_cl
      # summary(merra_cf$scf_fl)
      # summary(merra_cf$scf_tl)
      # summary(merra_cf$scf_td)
    } else if (resource == "win") {
      message("Estimating wind capacity factors (wcf_*)")
      merra_cf <- merra |>
        merra2ools::fWindCF(50, return_name = "wcf_50m") |> # wind
        merra2ools::fWindCF(100, return_name = "wcf_100m") |>
        merra2ools::fWindCF(150, return_name = "wcf_150m") |>
        select("UTC", "locid", starts_with("wcf_"))

      cl_file <- ideea_extra(data, glue(clusters_file))
      if (!file.exists(cl_file)) {
        message("Clustering data-file doesn't exist:\n    ", cl_file)
        return(merra_cf)
        # try *_sf.RData version
        # cl_file_sf <-
        # ideea_extra(data, glue("locid_{resource}_cl_r{nreg}_sf.RData"))
        # if (!file.exists(cl_file_sf)) {
      }
      message("Reading clusters for MERRA2 data:"); cat("  ", cl_file, "\n")
      cl_obj <- load(cl_file)
      message("Aggregating capacity factors by cluster, tolerance = ",
              100 * tol, "%")

      cl_obj <- get(cl_obj) |>
        filter(sd_loss <= tol) |>
        group_by(across(any_of(c(
          glue("reg{nreg}"),
          glue("reg{nreg}_off")
        )))) |>
        mutate(k_min = (k == min(k))) |> ungroup() |>
        filter(k_min) |> select(-k_min) |>
        as.data.table()

      merra_cf_cl <- merra_cf |>
        filter(locid %in% unique(cl_obj$locid)) |> # drop unused locations
        mutate(
          # create time-slices
          slice = energyRt::dtm2tsl(
            lubridate::with_tz(UTC, "Asia/Kolkata"),
            format = tsl_format)
        ) |>
        select(locid, slice, starts_with("wcf")) |> # drop unused columns
        left_join(
          # merge with gis-data
          select(cl_obj, any_of(c(
            "locid", "region", "reg_off",
            glue("reg{nreg}"), glue("reg{nreg}_off"),
            "offshore", "cluster", "MW_max"
          ))),
          by = "locid",
          relationship = "many-to-many") |>
        # aggregate
        group_by(across(any_of(
          c("slice", glue("reg{nreg}"), glue("reg{nreg}_off"), "cluster")))
        ) |>
        summarise(
          wcf_50m = round(weighted.mean(wcf_50m, MW_max, na.rm = T), 3),
          wcf_100m = round(weighted.mean(wcf_100m, MW_max, na.rm = T), 3),
          wcf_150m = round(weighted.mean(wcf_150m, MW_max, na.rm = T), 3),
          # geometry = st_union(geometry),
          .groups = "drop"
        ) |>
        mutate(year = year, .before = 1) |>
        as.data.table()
      # merra_cf_cl
      # summary(merra_cf$wcf_50m)
      # summary(merra_cf$wcf_100m)
      # summary(merra_cf$wcf_150m)
    }

    # fname <- file.path("tmp", glue("merra_sol_{sol_cl_mask}_{wyear}.fst"))
    if (save_to_extra) {
      if (!file.exists(cf_file) || overwrite) {
        message("Writing file: "); cat("   ", cf_file, "\n")
        fst::write_fst(merra_cf_cl, cf_file, 100)
        # } else if (!overwrite) {
        # message?
      }

    }

  } else {
    message("Unknown 'resource = ", resource,
            "'. Allowed values: 'win' and 'sol'")
    return(invisible())
  }
  if (is.factor(merra_cf_cl[["cluster"]])) {
    merra_cf_cl <- merra_cf_cl |>
      mutate(
        cluster = as.integer(as.character(cluster))
      )
  } else if (!is.integer(merra_cf_cl[["cluster"]])) {
    merra_cf_cl <- merra_cf_cl |>
      mutate(
        cluster = as.integer(cluster)
      )
  }
  cat("   Maximum number of clusters per region:",
      max(merra_cf_cl$cluster), "\n")
  merra_cf_cl
}

pick_tolerance <- function(x, tol = .05, nreg = NULL) {
  # browser()
  if (is.null(nreg)) {
    nreg <- str_extract(names(x), "(?<=reg)\\d+") |>
      as.integer() |> max(na.rm = T)
    if (!is.numeric(nreg) || is.null(nreg) || is.na(nreg) || nreg == 0) {
      stop("Cannot identify number of regions. `nreg` is not set.")
    }
  }
  x <- x |>
    filter(sd_loss <= tol) |>
    group_by(across(any_of(c(
      glue("reg{nreg}"),
      glue("reg{nreg}_off")
    )))) |>
    mutate(k_min = (k == min(k))) |>
    ungroup() |>
    filter(k_min) |> select(-k_min)
  # if (inherits(x, "sf")) |> as.data.table()
  x
}

#' Load sf (simple feature) object of solar or wind clusters
#'
#' @param resource character, name of the resource: "sol" or "win"
#' @param nreg integer, the number of regions
#' @param tol numeric, the tolerance of the information loss in the clustering from 0 to 1. If 0 the number of clusters is equal to the number of locations in MERRA2 database. When tol is 1, there is only one cluster per region.
#' @param data character, the name of the data source, the directory in the `ideea_extra` directory. Currently only "merra2" is supported.
#' @param clusters_sf_file character, the name of the file with the clusters in the `data` directory. Default is "locid_{resource}_cl_r{nreg}_sf.RData", available in `ideea_data`.
#'
#' @return sf, the simple feature object with the clusters for the requested resource.
#' @export
#'
#' @examples
#' # don't run
#' get_ideea_cl_sf(resource = "sol", nreg = 5)
#' get_ideea_cl_sf(resource = "win", nreg = 32, tol = 0.01)
get_ideea_cl_sf <- function(
    resource = "sol",
    # resource = "win",
    nreg = 5,
    tol = 0.05,
    data = "merra2",
    clusters_sf_file = "locid_{resource}_cl_r{nreg}_sf.RData"
) {
  # browser()
  fl <- ideea_extra(data, glue("locid_{resource}_cl_r{nreg}_sf.RData"))
  if (!file.exists(fl)) {
    stop("file doesn't exist\n   ", fl)
  }
  x <- load(fl) |> get() |> pick_tolerance(nreg = nreg, tol = tol)
  nms <- names(x)[str_detect(names(x), pattern = "cf_")]

  x |> group_by(across(any_of(c(
    glue("reg{nreg}"),
    glue("reg{nreg}_off"),
    "mainland", "offshore", "cluster"
    )))) |>
    summarise(
      across(all_of(nms), ~ mean(.x, na.rm = TRUE)),
      geometry = st_union(geometry),
      .groups = "drop") |>
    mutate(cluster = as.integer(cluster)) |>
    st_as_sf()

}


