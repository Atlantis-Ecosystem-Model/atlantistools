library("atlantistools")
file_fgs <- "SETasGroupsDem_NoCep.csv"
file_init <- "INIT_VMPA_Jan2015"

# utility functions -------------------------------------------------------------------------------
find_flags <- function(chars) {
  ids <- sort(unlist(lapply(c("double", "short"), grep, x = chars)))
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
                      "Light_Adaptn_PL", "Light_Adaptn_DF", "Light_Adaptn_PS", "t", "Light", "Oxygen"))

chars <- readLines(paste0(file_init, ".cdf"))
flags <- find_flags(chars)

ff <- load_fgs(fgs = file.path("new", file_fgs))

keep_vars <- c(
  paste0(" ", c(sort(as.vector(outer(as.vector(outer(ff$Name[ff$NumCohorts == 10], 1:10, FUN = paste0)), 
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
keep_vars[res != 0]

ids <- unlist(ids)
length(ids) == length(unique(ids))

new_init <- c(chars[1:8], chars[unlist(ids)], chars[length(chars)])




