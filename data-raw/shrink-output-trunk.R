# Skript to shrink atlantis output to fix R CMD Check NOTES

# Run model according to README.txt settings.
# Convert output ncs to cdfs
# Run this script
# Remove duplicated line 1213 in "outputSETAS.cdf"
# Remove duplicated line 296 in "outputSETASPROD.cdf"
# Re-Convert the cdfs in "new" folder to nc files
# Copy file in "new" folder to atlantistools directory
# Copy the remaining output files listed in README.txt

library("atlantistools")

file_fgs <- "SETasGroupsDem_NoCep.csv"
file_init <- "INIT_VMPA_Jan2015"
file_gen <- "outputTrunk/outputSETAS"
file_prod <- "outputTrunk/outputSETASPROD"

setwd("c:/ATLANTIS_Stuff/Atlantis_models/SETas_model_New_Trunk/")

# utility functions -------------------------------------------------------------------------------
find_flags <- function(chars) {
  ids <- sort(unlist(lapply(c("double", "short", "int "), grep, x = chars)))
  flags <- chars[ids]
  flags <- stringr::str_split(flags, pattern = " ", n = 2)
  if (!all(sapply(flags, length) == 2)) stop("wrong split.")
  flags <- sapply(flags, function(x) x[2])
  flags <- stringr::str_split(flags, pattern = "\\(", n = 2)
  if (!all(sapply(flags, length) == 2)) stop("wrong split.")
  flags <- sapply(flags, function(x) x[1])
  return(paste0(" ", flags))
}


find_block <- function(chars, var) {
  flags <- find_flags(chars)
  if (which(flags == var) == length(flags))  {
    block <- grep(var, chars, ignore.case = FALSE)
    block <- c(block, (block[length(block)] + 1):(length(chars) - 1))
  } else {
    next_var <- flags[which(flags == var) + 1]

    ids <- lapply(c(var, next_var), grep, x = chars, ignore.case = FALSE)
    if (any(sapply(ids, length) != 2)) stop(paste(var, "found multiple times."))
    ids_min <- c(ids[[1]][1], ids[[1]][length(ids[[1]])])
    ids_max <- c(ids[[2]][1], ids[[2]][length(ids[[2]])])
    block <- unlist(Map(seq, ids_min, ids_max - 1))
  }
  return(block)
}


# functionalGroups file ---------------------------------------------------------------------------
fgs <- load_fgs(fgs = file_fgs)
fgs <- fgs[fgs$IsTurnedOn == 1, ]
fgs$Index <- 1:nrow(fgs)
write.csv(fgs, file = file.path("new", file_fgs), quote = FALSE, row.names = FALSE)

# initial conditions file -------------------------------------------------------------------------
vars <- paste0(" ", c("porosity", "topk", "sedbiodepth", "seddetdepth", "sedoxdepth", "sedbiodens", "sedirrigenh",
                      "sedturbenh", "erosion_rate", "reef", "flat", "canyon", "soft", "eddy", "water", "DON",
                      "MicroNut", "Stress", "DiagNGain", "DiagNLoss", "DiagNFlux", "Light_Adaptn_MB",
                      "Light_Adaptn_PL", "Light_Adaptn_DF", "Light_Adaptn_PS", "t", "Light", "Oxygen", "Si", "Det_Si"))

chars <- readLines(paste0(file_init, ".cdf"))
flags <- find_flags(chars)

ff <- load_fgs(fgs = file.path("new", file_fgs))
coh_groups <- ff$Name[ff$NumCohorts == 10]

keep_vars <- c(
  paste0(" ", c(sort(as.vector(outer(as.vector(outer(coh_groups, 1:10, FUN = paste0)),
                       c("Nums", "ResN", "StructN"), FUN = paste, sep = "_"))),
                sort(paste(ff$Name[ff$NumCohorts != 2], "N", sep = "_")),
                sort(as.vector(outer(paste(ff$Name[ff$NumCohorts == 2], "N", sep = "_"), 1:2, FUN = paste0))))),
  flags[1:45][!flags[1:45] %in% vars])

ids <- lapply(keep_vars, find_block, chars = chars)
length(keep_vars[sapply(ids, length) > 38]) == 0

res <- vector(mode = "logical", length = length(ids))
for (i in seq_along(ids)) {
  dummy <- FALSE
  for (j in seq(1:length(ids))[-i]) {
    dummy <- dummy + any(ids[[i]] %in% ids[[j]])
  }
  res[i] <- dummy
}
length(keep_vars[res != 0]) == 0

ids <- unlist(ids)
length(ids) == length(unique(ids))

gl_at <- grep(chars, pattern = "global attributes")

new_init <- c(chars[1:8],
              chars[sort(c(unlist(ids), gl_at:(gl_at + 9)))],
              chars[length(chars)])

# Replace fill values
update_vars <- keep_vars[unlist(purrr::map(c("ResN", "StructN"), ~which(grepl(pattern = .x,  x = keep_vars))))]
fill <- paste0(stringr::str_sub(update_vars, start = 2), ":_FillValue")

ids <- purrr::map_int(fill, grep, x = new_init)

filler <- as.numeric(stringr::str_replace(stringr::str_split_fixed(new_init[ids], pattern = " = ", n = 2)[, 2], pattern = " ;", replacement = ""))

for (i in seq_along(update_vars)) {
  wuwu <- paste0(update_vars[i], " =")
  id <- grep(pattern = wuwu, x = new_init)
  singleline <- paste0(" ", paste(rep(filler[i], times = 7), collapse = ", "))

  new_init[id + 1:10] <- paste0(singleline, ",")
  new_init[id + 11] <- paste0(singleline, ";")
}

new_init <- new_init[-ids]

writeLines(new_init, con = file.path("new", paste0(paste0(file_init, ".cdf"))))

# general output file -----------------------------------------------------------------------------
chars <- readLines(paste0(file_gen, ".cdf"))

ids <- lapply(keep_vars, find_block, chars = chars)
ids <- unlist(ids)
length(ids) == length(unique(ids))

gl_at <- grep(chars, pattern = "global attributes")

new_init <- c(chars[1:8],
              chars[sort(c(unlist(ids), gl_at:(gl_at + 11)))],
              chars[length(chars)])

writeLines(new_init, con = file.path("new", paste0(paste0(basename(file_gen), ".cdf"))))

# productivity output file ------------------------------------------------------------------------
chars <- readLines(paste0(file_prod, ".cdf"))

flags <- find_flags(chars)

ff <- load_fgs(fgs = file.path("new", file_fgs))

keep_vars <- c(
  paste0(" ", c(sort(as.vector(outer(as.vector(outer(coh_groups, 1:10, FUN = paste0)),
                                     c("Growth", "Eat"), FUN = paste, sep = "_"))),
                sort(paste0(ff$Name[ff$NumCohorts != 10 & ff$IsPredator != 0], "Prodn")),
                sort(paste0(ff$Name[ff$NumCohorts != 10 & ff$IsPredator != 0], "Grazing")),
                c("dz", "volume", "numlayers"))))


ids <- lapply(keep_vars, find_block, chars = chars)
ids <- unlist(ids)
length(ids) == length(unique(ids))

gl_at <- grep(chars, pattern = "global attributes")

new_init <- c(chars[1:8],
              chars[sort(c(unlist(ids), gl_at:(gl_at + 11)))],
              chars[length(chars)])

writeLines(new_init, con = file.path("new", paste0(paste0(basename(file_prod), ".cdf"))))

