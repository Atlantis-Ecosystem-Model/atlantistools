#' This function loads weight at age data (in mgN) from the initial conditions file.
#'
#' @param dir Character string giving the path of the Atlantis model folder.
#' If data is stored in multiple folders (e.g. main model folder and output
#' folder) you should use 'NULL' as dir.
#' @param init Character string giving the filename of the initial conditions netcdf file.
#' Usually "init[...].nc".
#' @param fgs Character string giving the filename of 'functionalGroups.csv'
#' file. In case you are using multiple folders for your model files and
#' outputfiles pass the complete folder/filename string as fgs.
#' In addition set dir to 'NULL' in this case.
#' @param bboxes Integer vector giving the box-id of the boundary boxes.
#' @param select_variable Character value spefifying which variable to load.
#' Only one variable of the options available (i.e., \code{c(
#' "N", "Nums", "ResN", "StructN", "Eat", "Growth", "Prodn", "Grazing")
#' }) can be loaded at a time.
#' @param select_groups Character vector of funtional groups which shall be read in.
#' Names have to match the ones used in the ncdf file. Check column "Name" in
#' "functionalGroups.csv" for clarification.
#'
#' @family load functions
#' @export
#' @return A \code{data.frame} in long format with the following column names:
#'   species, agecl, rn and sn.
#'
#' @author Alexander Keth

#' @examples
#' dir <- system.file("extdata", "gns", package = "atlantistools")
#' load_init_weight(dir = dir, init = "init_simple_NorthSea.nc", fgs = "functionalGroups.csv")

load_init_weight <- function(dir = getwd(), init, fgs) {
  init_read <- RNetCDF::open.nc(con = convert_path(dir = dir, file = init))
  on.exit(RNetCDF::close.nc(init_read))

  # Construct vector of variable names to search!
  search_clean <- get_tags(dir = dir, fgs = fgs)

  # Extract data from init-file remove duplicated values and zeros!
  extract_data <- function(tags, nc) {
    at_data <- lapply(tags, RNetCDF::var.get.nc, ncfile = nc)
    at_data <- lapply(at_data, function(x) unique(x[!x %in% c(0, 1e-08, 1e-16)]))
    dims <- vapply(at_data, length, integer(1))
    if (all(dims == 1)) {
      unlist(at_data)
    } else {
      warning("Multiple weight at age values in initial file. Only the first value is used per group/age.")
      vapply(at_data, function(x) x[1], numeric(1))
    }
  }

  # Store in df
  df <- data.frame(species = rep(search_clean$species, times = sapply(search_clean$cohorts, length)),
                   agecl = unlist(search_clean$cohorts),
                   rn = extract_data(paste0(search_clean$tags, "_ResN"), init_read),
                   sn = extract_data(paste0(search_clean$tags, "_StructN"), init_read), stringsAsFactors = FALSE)

  return(df)
}

#' @export
#' @rdname load_init_weight
load_init_num <- function(dir = getwd(), init, fgs) {
  init_read <- RNetCDF::open.nc(con = convert_path(dir = dir, file = init))
  on.exit(RNetCDF::close.nc(init_read))

  # Construct vector of variable names to search!
  search_clean <- get_tags(dir = dir, fgs = fgs)

  # Extract data from init-file remove duplicated values and zeros!
  extract_data <- function(tags, nc) {
    at_data <- lapply(tags, RNetCDF::var.get.nc, ncfile = nc)
    at_data <- lapply(at_data, function(x) x[1, ]) # only use 1st layer
    at_data <- lapply(at_data, function(x) data.frame(atoutput = x, polygon = 0:(length(x) - 1), stringsAsFactors = FALSE))
    at_data <- lapply(at_data, function(x) x[!is.element(x$atoutput, c(0, 1e-08, 1e-16)), ])
    return(at_data)
  }

  # Store in df
  wuwu <- extract_data(paste0(search_clean$tags, "_Nums"), nc = init_read)
  for (i in seq_along(search_clean[[2]])) {
    for (j in 1:length(search_clean[[3]][[i]])) {
      if (i == 1 & j == 1) k <- 1
      wuwu[[k]]$species <- search_clean[[2]][i]
      wuwu[[k]]$agecl <- search_clean[[3]][[i]][j]
      k <- k + 1
    }
  }
  df <- do.call(rbind, wuwu)

  return(df)
}

#' @export
#' @rdname load_init_weight
load_init_n <- function(dir = getwd(), init, select_groups) {
  init_read <- RNetCDF::open.nc(con = convert_path(dir = dir, file = init))
  on.exit(RNetCDF::close.nc(init_read))

  search_clean <- paste0(select_groups, "_N")
  at_data <- lapply(search_clean, RNetCDF::var.get.nc, ncfile = init_read)

  sed_id <- sapply(at_data, function(x) length(dim(x))) == 1

  at_data <- at_data[!sed_id]
  if (length(at_data) >= 1) {
    at_data <- lapply(at_data, function(x) x[1, ]) # only use 1st layer
    at_data <- lapply(at_data, function(x) data.frame(atoutput = x, polygon = 0:(length(x) - 1), stringsAsFactors = FALSE))
    at_data <- lapply(at_data, function(x) x[!is.element(x$atoutput, c(0, 1e-08, 1e-16)), ])

    species <- select_groups[!sed_id]
    # Store in df
    for (i in seq_along(at_data)) {
      at_data[[i]]$species <- species[i]
    }
    df <- do.call(rbind, at_data)

    return(df)
  } else {
    stop("Only sediment groups selected. No extraction performed.")
  }
}

#' @rdname load_init_weight
load_init_physics <- function(dir = getwd(), init, tags) {
  init_read <- RNetCDF::open.nc(con = convert_path(dir = dir, file = init))
  on.exit(RNetCDF::close.nc(init_read))

  at_data <- lapply(tags, RNetCDF::var.get.nc, ncfile = init_read)

  sed_id <- sapply(at_data, function(x) length(dim(x))) == 1

  at_data <- at_data[!sed_id]
  at_data <- lapply(at_data, function(x) x[1, ]) # only use 1st layer
  at_data <- lapply(at_data, function(x) data.frame(atoutput = x, polygon = 0:(length(x) - 1), stringsAsFactors = FALSE))
  at_data <- lapply(at_data, function(x) x[!is.element(x$atoutput, c(0, 1e-08, 1e-16)), ])

  species <- groups_rest[!sed_id]
  # Store in df
  for (i in seq_along(at_data)) {
    at_data[[i]]$species <- species[i]
  }
  df <- do.call(rbind, at_data)

  return(df)
}


get_tags <- function(dir = getwd(), fgs) {
  # Construct vector of variable names to search!
  fgs_data <- load_fgs(dir = dir, fgs = fgs)
  species <- get_age_groups(dir = dir, fgs = fgs)
  numcohorts <- fgs_data$NumCohorts[is.element(fgs_data$Name, species)]
  cohorts <- lapply(numcohorts, seq, from = 1)
  search_clean <- unlist(Map(f = paste0, species, cohorts, USE.NAMES = FALSE))
  return(list(tags = search_clean, species = species, cohorts = cohorts))
}


load_init <- function(dir = getwd(), init, vars) {
  read_nc <- RNetCDF::open.nc(con = convert_path(dir = dir, file = init))
  on.exit(RNetCDF::close.nc(read_nc))

  # Extract ncdf dimensions!
  n_timesteps <- RNetCDF::dim.inq.nc(read_nc, 0)$length
  if (n_timesteps != 1) stop("More than 1 timestep! init was not an initial conditions file.")
  n_boxes     <- RNetCDF::dim.inq.nc(read_nc, 1)$length
  n_layers    <- RNetCDF::dim.inq.nc(read_nc, 2)$length
  num_layers <- get_layers(dir = dir, init = init)
  layerid <- get_layerid(num_layers = num_layers, max_layer = n_layers, n_boxes = n_boxes)
  var_names_ncdf <- sapply(seq_len(RNetCDF::file.inq.nc(read_nc)$nvars - 1),
                           function(x) RNetCDF::var.inq.nc(read_nc, x)$name)

  wrong_var <- vars[!vars %in% var_names_ncdf]
  if (length(wrong_var) >= 1) stop(paste("Variable", paste(wrong_var, collapse = " "), "not found in init file."))

  at_data <- lapply(vars, RNetCDF::var.get.nc, ncfile = read_nc)

  # Box and layer!
  convert2d <- function(mat, layerid, n_boxes) {
    if (!(is.matrix(mat) & length(dim(mat)) == 2)) {
      stop("Wrong data format. Variable is not stored as 2d data in initial file.")
    }
    data.frame(atoutput = as.vector(mat),
               polygon = rep(0:(n_boxes - 1), each = length(layerid) / n_boxes),
               layer = layerid, stringsAsFactors = FALSE)
  }

  # Only Box data!
  convert1d <- function(vec, n_boxes) {
    if (!(is.vector(vec) & length(vec) != n_boxes)) {
      stop("Wrong data format. Variable is not stored as 1d vector in initial file.")
    }
    data.frame(atoutput = as.vector(mat),
               polygon = 0:(n_boxes - 1), stringsAsFactors = FALSE)
  }

  at_dim <- vapply(at_data, function(x) length(dim(x)), integer(1))

  # Check cases and apply formulas!
  if (all(at_dim == 2)) df_list <- lapply(at_data, convert2d, layerid, n_boxes)
  if (all(at_dim == 1)) df_list <- lapply(at_data, convert1d, n_boxes)
  if (length(unique(at_dim)) > 1) stop("Vars are stored in different dimensions. Please, either pick only 2d or 1d data.")

  # Data extracted for every variable?
  if (length(vars) != length(df_list)) stop("Starnge ncdf extraction. Please contact package development Team.")

  return(df_list)
}

get_layers <- function(dir = getwd(), init) {
  read_nc <- RNetCDF::open.nc(con = convert_path(dir = dir, file = init))
  on.exit(RNetCDF::close.nc(read_nc))

  num_layers <- RNetCDF::var.get.nc(ncfile = read_nc, variable = "numlayers")
  if (length(dim(num_layers)) == 2) {
    if (all(apply(num_layers, MARGIN = 1, FUN = function(x) length(unique)) == 1)) {
      num_layers <- num_layers[, 1]
    } else {
      stop("Different numbers of layers per Box. This nc-structure is not supported.")
    }
  }

  return(num_layers)
}

get_layerid <- function(num_layers, max_layer, n_boxes) {
  wc_id <- lapply(num_layers, function(x) rep(1, times = x))
  wc_id <- lapply(wc_id, function(x) rev(cumsum(x) - 1)) # ids are in reverse order in the nc file
  wc_fill <- lapply(num_layers, function(x) rep(NA, times = max_layer - x - 1))
  wc <- Map(f = c, wc_id, wc_fill)
  if (length(unique(sapply(wc, length))) != 1) stop("Layers inconsistent. Contact package development Team.")
  wc <- lapply(wc, function(x) c(x, max_layer)) # add sediment layer
  unlist(wc)
}

remove_min_pools <- function(df, col = "atoutput", min_pools = c(0, 1e-08, 1e-16)) {
  expr <- lazyeval::interp(quote(!(x %in% y)), x = as.name(col), y = min_pools)
  df %>% dplyr::filter_(expr)
}

remove_bboxes <- function(df, bboxes) {
  if (!any(names(df) == "polygon")) stop("No column polygon in df. Cannot remove boundary boxes.")
  df %>% dplyr::filter(!(polygon %in% bboxes))
}


load_init_age <- function(dir = getwd(), init, fgs, select_variable, select_groups = NULL, bboxes) {
  # Consrtuct vars to search for!
  fgs_data <- load_fgs(dir = dir, fgs = fgs)
  age_groups <- get_age_groups(dir = dir, fgs = fgs)
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
  df_list <- load_init(dir = dir, init = init, vars = vars)
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
  result <- dplyr::filter(result, !is.na(layer))

  return(result)
}

load_init_nonage <- function(dir = getwd(), init, fgs, select_variable = "N", select_groups = NULL, bboxes, bps) {
  # Consrtuct vars to search for!
  if (is.null(select_groups)) select_groups <- get_groups(dir = dir, fgs = fgs)
  select_bps <- select_groups[is.element(select_groups, bps)]
  select_groups <- select_groups[!is.element(select_groups, bps)]

  # Extract data for non biomasspools!
  if (length(select_groups) >= 1) {
    df_list <- load_init(dir = dir, init = init, vars = paste(select_roups, select_variable, sep = "_"))
    # Add columns!
    for (i in seq_along(select_groups)) {
      df_list[[k]]$species <- select_groups[i]
    }
    df1 <- do.call(rbind, df_list)
  }

  # Extract data for biomasspools!
  if (length(select_bps)) {
    read_nc <- RNetCDF::open.nc(con = convert_path(dir = dir, file = init))
    on.exit(RNetCDF::close.nc(read_nc))
    n_layers    <- RNetCDF::dim.inq.nc(read_nc, 2)$length
    df_list <- load_init(dir = dir, init = init, vars = paste(select_bps, select_variable, sep = "_"))
    # Add columns!
    for (i in seq_along(select_bps)) {
      df_list[[k]]$species <- select_bps[i]
    }
    df2 <- do.call(rbind, df_list)
    df2$layer <- n_layers
  }

  if (exists(df1) & exists(df2)) result <- rbind(df1, df2)
  if (exists(df1) & !exists(df2)) result <- df1
  if (!exists(df1) & exists(df2)) result <- df2

  # Cleanup
  result <- remove_min_pools(df = result)
  result <- remove_bboxes(df = result, bboxes = bboxes)
  result <- dplyr::filter(result, !is.na(layer))

  return(result)
}

load_init_physics <- function(dir = getwd(), init, select_variable, bboxes) {
  # Extract data!
  df_list <- load_init(dir = dir, init = init, vars = select_variable)
  # Add columns!
  for (i in seq_along(select_groups)) {
    df_list[[k]]$species <- select_groups[i]
  }
  df1 <- do.call(rbind, df_list)

  # Extract data for biomasspools!
  df_list <- load_init(dir = dir, init = init, vars = paste(select_bps, select_variable, sep = "_"))
  # Add columns!
  for (i in seq_along(select_bps)) {
    df_list[[k]]$species <- select_bps[i]
  }
  df2 <- do.call(rbind, df_list)
  df2$layer <- max(df1$layer)

  result <- rbind(df1, df2)

  # Cleanup
  result <- remove_min_pools(df = result)
  result <- remove_bboxes(df = result, bboxes = bboxes)
  result <- dplyr::filter(result, !is.na(layer))

  return(result)
}

# stringr::str_sub(string, start = -1)
#
# var_names_ncdf <- sapply(seq_len(RNetCDF::file.inq.nc(read_nc)$nvars - 1),
#                          function(x) RNetCDF::var.inq.nc(read_nc, x)$name)
#
# wawa <- at_data[sapply(at_data, function(x) length(dim(x)))]
#
# wuwu <- vector(mode = "list", length = length(wawa))
# for (i in seq_along(wuwu)) {
#   wuwu[[i]] <- apply(wawa[[i]], MARGIN = 2, function(x) sum(x != 0))
# }
#
#
# at_data <- lapply(var_names_ncdf, RNetCDF::var.get.nc, ncfile = nc)
#
# sapply(at_data, function(x) length(dim(x)))
#
# dim1 <- var_names_ncdf[sapply(at_data, function(x) length(dim(x))) == 1]
# dim2 <- var_names_ncdf[sapply(at_data, function(x) length(dim(x))) == 2]
#


