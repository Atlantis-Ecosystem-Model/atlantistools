#' This function loads weight at age data (in mgN) from the initial conditions file.
#'
#' @inheritParams load_init
#' @inheritParams load_fgs
#' @inheritParams load_nc
#' @param select_variable Character value spefifying which variable to load.
#' For \code{load_init_age} this can be "Nums", "ResN", "StructN",
#' For \code{load_init_nonage} please select "N" (default)
#' For \code{load_init_physics} simply pass the names of the physical variables.
#' @param select_groups Character vector of funtional groups which shall be read in.
#' Names have to match the ones used in the ncdf file. Check column "Name" in
#' "functionalGroups.csv" for clarification. Default is \code{NULL} resulting in all available groups.
#' @family load functions
#' @export
#' @return A dataframes with columns atoutput, polygon, layer (if present), species (if present).
#'
#' @author Alexander Keth

#' @examples
#' d <- system.file("extdata", "setas-model-new-trunk", package = "atlantistools")
#' init <- file.path(d, "INIT_VMPA_Jan2015.nc")
#' fgs <- file.path(d, "SETasGroupsDem_NoCep.csv")
#' bboxes <- get_boundary(load_box(bgm = file.path(d, "VMPA_setas.bgm")))
#'
#' bps <- load_bps(fgs = fgs, init = init)
#'
#' # There are no values in the initial conditions file. Therefore atoutput is NA.
#' load_init_age(init = init, fgs = fgs, bboxes = bboxes,
#'               select_variable = "ResN",
#'               select_groups = "Planktiv_S_Fish")
#'
#' load_init_age(init = init, fgs = fgs, bboxes = bboxes, select_variable = "ResN")
#' load_init_nonage(init = init, fgs = fgs, bboxes = bboxes, bps = bps,
#'    select_groups = "Megazoobenthos")
#' load_init_nonage(init = init, fgs = fgs, bboxes = bboxes, bps = bps)
#' load_init_stanza(init = init, fgs = fgs, bboxes = bboxes)
#' load_init_weight(init = init, fgs = fgs, bboxes = bboxes)

load_init_age <- function(init, fgs, select_variable, select_groups = NULL, bboxes) {
  # Consrtuct vars to search for!
  fgs_data <- load_fgs(fgs = fgs)
  age_groups <- get_age_groups(fgs = fgs)
  if (any(!is.element(select_groups, age_groups))) stop("Selected group is not a fully age-structured group.")
  if (is.null(select_groups)) select_groups <- age_groups

  num_cohorts <- fgs_data$NumCohorts[is.element(fgs_data$Name, select_groups)]
  ages <- lapply(num_cohorts, seq, from = 1, by = 1)

  vars <- NULL
  for (i in seq_along(select_groups)) {
    tags <- paste0(select_groups[i], ages[[i]], "_", select_variable)
    vars <- c(vars, tags)
  }

  # Extract data
  df_list <- load_init(init = init, vars = vars)
  # Add columns!
  for (i in seq_along(select_groups)) {
    for (j in 1:length(ages[[i]])) {
      if (i == 1 & j == 1) k <- 1
      df_list[[k]]$species <- select_groups[i]
      df_list[[k]]$agecl <- ages[[i]][j]
      k <- k + 1
    }
  }
  result <- do.call(rbind, df_list)

  # Cleanup
  result <- remove_min_pools(df = result)
  result <- remove_bboxes(df = result, bboxes = bboxes)
  result <- dplyr::filter_(result, ~!is.na(layer))
  result$species <- convert_factor(data_fgs = fgs_data, col = result$species)

  return(result)
}

#' @export
#' @rdname load_init_age
load_init_nonage <- function(init, fgs, select_variable = "N", select_groups = NULL, bboxes, bps) {
  # NOTE: Age based inverts are stored in a different way.... Name_Ncohort instead of NameCohort_Var
  # Consrtuct vars to search for!
  if (is.null(select_groups)) {
    fgs_data <- load_fgs(fgs = fgs)
    select_groups <- fgs_data$Name[fgs_data$NumCohorts != 2]
  }
  select_bps <- select_groups[is.element(select_groups, bps)]
  select_groups <- select_groups[!is.element(select_groups, bps)]

  # Extract data for non biomasspools!
  if (length(select_groups) >= 1) {
    df_list <- load_init(init = init, vars = paste(select_groups, select_variable, sep = "_"))
    # Add columns!
    for (i in seq_along(select_groups)) {
      df_list[[i]]$species <- select_groups[i]
    }
    df1 <- do.call(rbind, df_list)
  }

  # Extract data for biomasspools!
  if (length(select_bps)) {
    read_nc <- RNetCDF::open.nc(con = init)
    on.exit(RNetCDF::close.nc(read_nc))
    n_layers    <- RNetCDF::dim.inq.nc(read_nc, 2)$length
    df_list <- load_init(init = init, vars = paste(select_bps, select_variable, sep = "_"))
    # Add columns!
    for (i in seq_along(select_bps)) {
      df_list[[i]]$species <- select_bps[i]
    }
    df2 <- do.call(rbind, df_list)
    df2$layer <- n_layers - 1
  }

  if (length(select_groups) >= 1 & length(select_bps) >= 1)  result <- rbind(df1, df2)
  if (length(select_groups) >= 1 & !length(select_bps) >= 1) result <- df1
  if (!length(select_groups) >= 1 & length(select_bps) >= 1) result <- df2

  # Cleanup
  result <- remove_min_pools(df = result)
  result <- remove_bboxes(df = result, bboxes = bboxes)
  result <- dplyr::filter_(result, ~!is.na(layer))
  result$species <- convert_factor(data_fgs = load_fgs(fgs = fgs), col = result$species)

  return(result)
}

#' @export
#' @rdname load_init_age
load_init_stanza <- function(init, fgs, select_variable = "N", select_groups = NULL, bboxes) {
  # Consrtuct vars to search for!
  fgs_data <- load_fgs(fgs = fgs)
  fgs_data <- fgs_data[fgs_data$NumCohorts == 2, ]
  age_groups <- fgs_data$Name

  ages <- 1:2

  if (any(!is.element(select_groups, age_groups))) stop("Selected group is not a stanza group.")
  if (is.null(select_groups)) select_groups <- age_groups

  vars <- NULL
  for (i in seq_along(select_groups)) {
    tags <- paste0(select_groups[i], "_", select_variable, 1:2)
    vars <- c(vars, tags)
  }

  # Extract data
  df_list <- load_init(init = init, vars = vars)
  # Add columns!
  for (i in seq_along(select_groups)) {
    for (j in 1:length(ages)) {
      if (i == 1 & j == 1) k <- 1
      df_list[[k]]$species <- select_groups[i]
      df_list[[k]]$agecl <- ages[j]
      k <- k + 1
    }
  }
  result <- do.call(rbind, df_list)

  # Cleanup
  result <- remove_min_pools(df = result)
  result <- remove_bboxes(df = result, bboxes = bboxes)
  result <- dplyr::filter_(result, ~!is.na(layer))
  result$species <- convert_factor(data_fgs = fgs_data, col = result$species)

  return(result)
}


#' @export
#' @rdname load_init_age
load_init_physics <- function(init, select_variable, bboxes) {
  # Extract data!
  df_list <- load_init(init = init, vars = select_variable)
  # Add columns!
  for (i in seq_along(select_variable)) {
    df_list[[i]]$variable <- select_variable[i]
  }
  result <- do.call(rbind, df_list)

  # Cleanup
  result <- remove_min_pools(df = result)
  result <- remove_bboxes(df = result, bboxes = bboxes)
  result <- dplyr::filter_(result, ~!is.na(layer))

  return(result)
}

#' @export
#' @rdname load_init_age
load_init_weight <- function(init, fgs, bboxes) {
  rn <- load_init_age(init = init, fgs = fgs, select_variable = "ResN", bboxes = bboxes) %>%
    dplyr::filter_(~!is.na(atoutput)) %>%
    dplyr::select_(.dots = c("atoutput", "species", "agecl")) %>%
    dplyr::rename_(.dots = c("rn" = "atoutput")) %>%
    unique()
  sn <- load_init_age(init = init, fgs = fgs, select_variable = "StructN", bboxes = bboxes) %>%
    dplyr::filter_(~!is.na(atoutput)) %>%
    dplyr::select_(.dots = c("atoutput", "species", "agecl")) %>%
    dplyr::rename_(.dots = c("sn" = "atoutput")) %>%
    unique()
  df <- dplyr::inner_join(rn, sn, by = c("species", "agecl")) %>%
    dplyr::select_(.dots = c("species", "agecl", "sn", "rn"))
  return(df)
}






