% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/load-nc.R
\name{load_nc}
\alias{load_nc}
\title{Load Atlantis outputfiles (netcdf)}
\usage{
load_nc(
  nc,
  fgs,
  bps,
  select_groups,
  select_variable,
  prm_run,
  bboxes,
  check_acronyms = TRUE,
  warn_zeros = FALSE,
  report = TRUE
)
}
\arguments{
\item{nc}{Character string giving the connection of the netcdf file to read in.
The filename usually contains \code{output} and ends in \code{.nc}".}

\item{fgs}{Character string giving the connection to the functional groups file.
The filename usually contains \code{Groups} and does end in \code{.csv}.}

\item{bps}{Vector of character strings giving the complete list of epibenthic
functional groups (Only present in the sediment layer). The names have to match
the column 'Name' in the 'functionalGroups.csv' file. Can be created with
\code{load_bps}.#'}

\item{select_groups}{Character vector of funtional groups which shall be read in.
Names have to match the ones used in the netcdf file. Check column "Name" in
"functionalGroups.csv" for clarification.}

\item{select_variable}{Character value specifying which variable to load.
Only one variable of the options available (i.e., \code{c(
"N", "Nums", "ResN", "StructN", "Eat", "Growth", "Prodn", "Grazing")
}) can be loaded at a time.}

\item{prm_run}{Character string giving the connection of the run parameterfile.
The filename usually contains \code{run_fishing} and ends in \code{.prm}".}

\item{bboxes}{Integer vector giving the box-id of the boundary boxes.
Can be created with \code{get_boundary}.}

\item{check_acronyms}{Logical testing if functional-groups in
select_groups are inactive in the current model run. They will be omitted
in the output.}

\item{warn_zeros}{Logical indicating if check for actual zeros in the
data shall be printed or not. Default is \code{FALSE}.}

\item{report}{Logical indicating if progress bars shall be printed (\code{TRUE}) or
not (\code{FALSE}). Default is \code{TRUE}.}
}
\value{
A \code{data.frame} in long format with the following column names:
  species, timestep, polygon, agecl, and atoutput (i.e., variable).
}
\description{
This function loads Atlantis outputfiles (any netcdf file) and
converts them to a \code{data.frame}.
}
\examples{
d <- system.file("extdata", "setas-model-new-trunk", package = "atlantistools")

nc <- file.path(d, "outputSETAS.nc")
fgs <- file.path(d, "SETasGroupsDem_NoCep.csv")
bps <- load_bps(init = file.path(d, "INIT_VMPA_Jan2015.nc"), fgs = fgs)
bboxes <- get_boundary(boxinfo = load_box(bgm = file.path(d, "VMPA_setas.bgm")))
prm_run <- file.path(d, "VMPA_setas_run_fishing_F_Trunk.prm")

test <- load_nc(nc = nc, bps = bps, fgs = fgs, prm_run = prm_run, bboxes = bboxes,
  select_groups = c("Planktiv_S_Fish", "Cephalopod", "Diatom"),
  select_variable = "ResN")

test <- load_nc(nc = nc, bps = bps, fgs = fgs, prm_run = prm_run, bboxes = bboxes,
  select_groups = c("Planktiv_S_Fish", "Cephalopod", "Diatom"),
  select_variable = "Nums")
}
\seealso{
Other load functions: 
\code{\link{load_box}()},
\code{\link{load_bps}()},
\code{\link{load_dietcheck}()},
\code{\link{load_fgs}()},
\code{\link{load_init_age}()},
\code{\link{load_init}()},
\code{\link{load_mort}()},
\code{\link{load_nc_physics}()},
\code{\link{load_rec}()},
\code{\link{load_spec_mort}()},
\code{\link{load_spec_pred_mort}()},
\code{\link{load_txt}()}
}
\author{
Alexander Keth
}
\concept{load functions}
\keyword{gen}
