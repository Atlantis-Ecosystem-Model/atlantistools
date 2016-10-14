library("atlantistools")
file_fgs <- "SETasGroups.csv"
file_init <- "init_vmpa_setas_25032013"
file_gen <- "outputSETAS"
file_prod <- "outputSETASPROD"

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
    if (any(length(ids[[1]]) != 2)) stop(paste(var, "found multiple times."))
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
chars <- readLines(paste0(file_init, ".cdf"))
flags <- find_flags(chars)

ff <- load_fgs(fgs = file.path("new", file_fgs))

keep_vars <- c(
  paste0(" ", c(sort(as.vector(outer(as.vector(outer(ff$Name[ff$NumCohorts == 10], 1:10, FUN = paste0)), 
                       c("Nums", "ResN", "StructN"), FUN = paste, sep = "_"))),
                sort(paste(ff$Name[ff$NumCohorts != 2], "N", sep = "_")),
                sort(as.vector(outer(paste(ff$Name[ff$NumCohorts == 2], "N", sep = "_"), 1:2, FUN = paste0))))),
  c(" volume", " hdsource", " hdsink", " eflux", " vflux", " nominal_dz", " dz", 
    " numlayers"," NH3", " NO3", " Temp", " salt", " Denitrifiction", " Nitrification", " Chl_a"))

ids <- lapply(keep_vars, find_block, chars = chars)
length(keep_vars[sapply(ids, length) > 39]) == 0

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

writeLines(new_init, con = file.path("new", paste0(paste0(file_gen, ".cdf"))))

# productivity output file ------------------------------------------------------------------------
chars <- readLines(paste0(file_prod, ".cdf"))

flags <- find_flags(chars)

ff <- load_fgs(fgs = file.path("new", file_fgs))

keep_vars <- c(
  paste0(" ", c(sort(as.vector(outer(as.vector(outer(ff$Name[ff$NumCohorts == 10], 1:10, FUN = paste0)), 
                                     c("Growth", "Eat"), FUN = paste, sep = "_"))),
                sort(paste0(ff$Name[ff$NumCohorts != 10 & ff$isPredator != 0], "Prodn")),
                sort(paste0(ff$Name[ff$NumCohorts != 10 & ff$isPredator != 0], "Grazing")), 
                c("dz", "volume", "numlayers"))))


ids <- lapply(keep_vars, find_block, chars = chars)
ids <- unlist(ids)
length(ids) == length(unique(ids))

gl_at <- grep(chars, pattern = "global attributes")

new_init <- c(chars[1:8], 
              chars[sort(c(unlist(ids), gl_at:(gl_at + 11)))], 
              chars[length(chars)])

writeLines(new_init, con = file.path("new", paste0(paste0(file_prod, ".cdf"))))

