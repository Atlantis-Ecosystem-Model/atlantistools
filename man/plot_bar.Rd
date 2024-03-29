% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot-bar.R
\name{plot_bar}
\alias{plot_bar}
\title{Function to plot relative contribution of biomass and numbers per cohort.}
\usage{
plot_bar(
  data,
  x = "time",
  y = "atoutput",
  fill = "species",
  wrap = NULL,
  ncol = NULL
)
}
\arguments{
\item{data}{Dataframe to be plotted.}

\item{x}{x-variable. Default is \code{'time'}.}

\item{y}{y-variable. Default is \code{'atoutput'}.}

\item{fill}{Column to use as filling colour. Default is \code{"species"}.}

\item{wrap}{Wraping column. Default is \code{'species'}}

\item{ncol}{Number of columns in multipanel plot. Default is \code{7}.}
}
\value{
ggplot2 object
}
\description{
Function to plot relative contribution of biomass and numbers per cohort.
}
\examples{
plot_bar(preprocess$biomass)

# Most models have a large number of groups. Please make sure to combine groups with a low
# contribution prior to plotting with \code{\link{combine_groups}}.
df <- combine_groups(preprocess$biomass, group_col = "species", combine_thresh = 3)
plot_bar(df)

# This function can also be used to plot age-specific data.
plot_bar(preprocess$nums_age, fill = "agecl", wrap = "species")

# Please use \code{\link{agg_perc}} to visualize the relative cohort structure over time.
df <- agg_perc(preprocess$nums_age, groups = c("time", "species"))
plot_bar(df, fill = "agecl", wrap = "species")
}
\seealso{
Other plot functions: 
\code{\link{plot_boxes}()},
\code{\link{plot_diet_bec_dev}()},
\code{\link{plot_diet}()},
\code{\link{plot_line}()},
\code{\link{plot_rec}()},
\code{\link{plot_species}()}
}
\concept{plot functions}
