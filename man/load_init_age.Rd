% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/load-init-age.R
\name{load_init_age}
\alias{load_init_age}
\alias{load_init_nonage}
\alias{load_init_stanza}
\alias{load_init_physics}
\alias{load_init_weight}
\title{This function loads weight at age data (in mgN) from the initial conditions file.}
\usage{
load_init_age(init, fgs, select_variable, select_groups = NULL, bboxes)

load_init_nonage(
  init,
  fgs,
  select_variable = "N",
  select_groups = NULL,
  bboxes,
  bps
)

load_init_stanza(
  init,
  fgs,
  select_variable = "N",
  select_groups = NULL,
  bboxes
)

load_init_physics(init, select_variable, bboxes)

load_init_weight(init, fgs, bboxes)
}
\arguments{
\item{init}{Character string giving the connection of the initial conditions netcdf file.
The filename usually contains \code{init} and ends in \code{.nc}.}

\item{fgs}{Character string giving the connection to the functional groups file.
The filename usually contains \code{Groups} and does end in \code{.csv}.}

\item{select_variable}{Character value specifying which variable to load.
For \code{load_init_age} this can be "Nums", "ResN", "StructN",
For \code{load_init_nonage} please select "N" (default)
For \code{load_init_physics} simply pass the names of the physical variables.}

\item{select_groups}{Character vector of functional groups which shall be read in.
Names have to match the ones used in the ncdf file. Check column "Name" in
"functionalGroups.csv" for clarification. Default is \code{NULL} resulting in all available groups.}

\item{bboxes}{Integer vector giving the box-id of the boundary boxes.
Can be created with \code{get_boundary}.}

\item{bps}{Vector of character strings giving the complete list of epibenthic
functional groups (Only present in the sediment layer). The names have to match
the column 'Name' in the 'functionalGroups.csv' file. Can be created with
\code{load_bps}.#'}
}
\value{
A dataframes with columns atoutput, polygon, layer (if present), species (if present).
}
\description{
This function loads weight at age data (in mgN) from the initial conditions file.
}
\examples{
d <- system.file("extdata", "setas-model-new-trunk", package = "atlantistools")
init <- file.path(d, "INIT_VMPA_Jan2015.nc")
fgs <- file.path(d, "SETasGroupsDem_NoCep.csv")
bboxes <- get_boundary(load_box(bgm = file.path(d, "VMPA_setas.bgm")))

bps <- load_bps(fgs = fgs, init = init)

# There are no values in the initial conditions file. Therefore atoutput is NA.
load_init_age(init = init, fgs = fgs, bboxes = bboxes,
              select_variable = "ResN",
              select_groups = "Planktiv_S_Fish")

load_init_age(init = init, fgs = fgs, bboxes = bboxes, select_variable = "ResN")
load_init_stanza(init = init, fgs = fgs, bboxes = bboxes)
load_init_weight(init = init, fgs = fgs, bboxes = bboxes)
}
\seealso{
Other load functions: 
\code{\link{load_box}()},
\code{\link{load_bps}()},
\code{\link{load_dietcheck}()},
\code{\link{load_fgs}()},
\code{\link{load_init}()},
\code{\link{load_mort}()},
\code{\link{load_nc_physics}()},
\code{\link{load_nc}()},
\code{\link{load_rec}()},
\code{\link{load_spec_mort}()},
\code{\link{load_spec_pred_mort}()},
\code{\link{load_txt}()}
}
\author{
Alexander Keth
}
\concept{load functions}
