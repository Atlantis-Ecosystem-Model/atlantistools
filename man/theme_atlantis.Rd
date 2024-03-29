% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/theme-atlantis.R
\name{theme_atlantis}
\alias{theme_atlantis}
\title{Customized theme used in all plots.}
\usage{
theme_atlantis(
  large = 22,
  medium = 18,
  small = 14,
  scale_font = 1,
  rot_xaxis_text = FALSE,
  rot_strips_y = TRUE
)
}
\arguments{
\item{large}{Integer giving the size of the font for the main parts of the plot.
Default is \code{22}.}

\item{medium}{Integer giving the size of the font used in the legend and
facet labels. Default is \code{18}.}

\item{small}{Integer giving the size of the font used in the rest of the plot.
Default is \code{14}.}

\item{scale_font}{Numeric used to scale all font sizes. Default is \code{1}.}

\item{rot_xaxis_text}{Logical indicating if x-axis text should be
rotated by 45 degree. Default is \code{FALSE}.}

\item{rot_strips_y}{Logical indicating if facet labels should be
rotated by 90 degree. Default is \code{TRUE}.}
}
\description{
This function is a customized theme for ggplot2 plots. It's applied
by default to all plots created within \code{atlantistools}.
}
\examples{
\dontrun{nums_agg <- agg_data(data = ref_nums, groups = c("species", "time"), fun = sum)
ggplot2::ggplot(data = nums_agg, ggplot2::aes(x = time, y = atoutput)) +
   ggplot2::facet_wrap(~species) +
   theme_atlantis()}
}
\concept{theme functions}
