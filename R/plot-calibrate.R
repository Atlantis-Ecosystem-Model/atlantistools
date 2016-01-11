


plot_calibrate <- function(dir,
                           nc,
                           bps,
                           fgs,
                           select_groups,
                           select_variable,
                           bboxes,
                           check_acronyms,
                           modelstart,
                           prm_run) {
  data <- load_nc(dir = dir,
                  nc = nc_gen,
                  bps = bps,
                  fgs = fgs,
                  select_groups = select_age_groups,
                  select_variable = select_variable,
                  bboxes = bboxes,
                  check_acronyms = check_acronyms)

  # Aggregate data over layers and polygons!
  if (select_variable == "Nums") {
    data <- agg_sum(data = data, vars = c("species", "time", "agecl"))
  } else {
    data <- agg_mean(data = data, vars = c("species", "time", "agecl"))
  }

  # Convert timestep to actual time!
  if (!is.null(dir)) prm_run <- file.path(dir, prm_run)
  prm_run <- readLines(con = prm_biol)

  convert_time <- function(data, prm_run, modelstart){
    # Exract timesetp from Parameterfile!
    toutinc <- extract_param(chars = prm_run, variable = "toutinc")
    if (any(names(data) == "time")) data$time <- with(data, as.Date.numeric(time * toutinc, origin = modelstart))
    return(data)
  }

  # Convert time given as integer to actual time! This is done as late as possible to simplify coding/execution-time.
  data <- lapply(data, convert_time, modelstart = modelstart, prm_run = prm_run)

  # Divide by initial values
  at_calibrate <- lapply(list(data$at_nums_age, data$agg_age_at_structn, data$agg_age_at_resn, data$biomass_ages), datatrans_calibrate)


}




