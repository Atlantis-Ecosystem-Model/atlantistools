#' Combine ageclasses to juvenile and adult stanza according to age at maturity.
#'
#' @param data Dataframe with ageclass specific information.
#' @param grp_col Character string giving the name of the group column in \code{data}.
#' E.g. 'species', 'pred', 'prey' etc.
#' @param agemat First mature age class for age structured groups. This dataframe should
#' be generated with \code{\link{prm_to_df}} using "age_mat" as parameter.
#' @param value_col Character string giving the name of the column to sum.
#' Default is \code{"atoutput"}.
#' @return Dataframe with ageclasses combined to stanzas.
#' @export
#'
#' @examples
#' dir <- system.file("extdata", "gns", package = "atlantistools")
#' nc_gen <- "outputNorthSea.nc"
#' prm_biol <- "NorthSea_biol_fishing.prm"
#' prm_run = "NorthSea_run_fishing_F.prm"
#' bps = load_bps(dir, fgs = "functionalGroups.csv", init = "init_simple_NorthSea.nc")
#' fgs = "functionalGroups.csv"
#' bboxes = get_boundary(boxinfo = load_box(dir, bgm = "NorthSea.bgm"))
#'
#' data <- calculate_biomass_spatial(dir, nc_gen, prm_biol, prm_run, bps, fgs, bboxes)#'
#' agemat <- prm_to_df(dir = dir, prm_biol = prm_biol, fgs = fgs,
#'                     group = get_age_acronyms(dir = dir, fgs = fgs),
#'                     parameter = "age_mat")
#'
#' combine_ages(data, grp_col = "species", agemat = agemat)

combine_ages <- function(data, grp_col, agemat, value_col = "atoutput") {
  # Check input dataframe structure.
  cols <- c(grp_col, value_col, "agecl")
  missing <- cols[!cols %in% names(data)]
  if (length(missing) > 0) stop(paste0("Columnname '", missing, "' missing in data."))

  # Combine data with agemat!
  agegrps <- unique(data[!is.na(data$agecl) & data$agecl > 2, grp_col])
  if (any(!agegrps %in% agemat$species)) stop("Agegroups in data not present in agemat.")
  names(agemat)[names(agemat) == "species"] <- grp_col # in case grp_col is not 'species'

  data_stanza <- dplyr::left_join(data, agemat)

  # NAs remain NAs!
  data_stanza$stanza <- ifelse(data_stanza$agecl < data_stanza$age_mat, 1, 2)
  data_stanza$stanza[is.na(data_stanza$stanza)] <- 2 # Not sure if this is correct!

  result <- data_stanza %>%
    agg_data(col = value_col, groups = names(.)[!names(.) %in% c(value_col, "agecl")], out = value_col, fun = sum)
  names(result)[names(result) == "stanza"] <- paste(grp_col, "stanza", sep = "_")
  result$age_mat <- NULL

  return(result)
}
