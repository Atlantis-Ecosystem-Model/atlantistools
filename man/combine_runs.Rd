% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/combine-runs.R
\name{combine_runs}
\alias{combine_runs}
\title{This function is used to combine model output from different simulations!}
\usage{
combine_runs(outs, runs)
}
\arguments{
\item{outs}{List of preprocessed Atlantis simulations. Each entry is a list generated with
`preprocess()`.}

\item{runs}{Vector of character strings giving the name of each simulation settings.}
}
\value{
Named list with the the same format as in `preprocess()`. Each dataframe has
an additional column run.
}
\description{
This function is used to combine model output from different simulations!
}
\examples{
outs <- list(preprocess, preprocess)
runs <- c("run1", "run2")
test <- combine_runs(outs, runs)
names(test[[1]])
head(test[[1]])
}
\seealso{
Other combine functions: 
\code{\link{combine_groups}()}
}
\concept{combine functions}
