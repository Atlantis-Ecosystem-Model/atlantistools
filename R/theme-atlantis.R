#' Customised theme used in all plots.
#'
#' This function is a customised theme for ggplot2 plots. It's applied
#' by default to all plots created within \code{atlantistools}.
#' @param large Integer giving the size of the font for the main parts of the plot.
#' Default is \code{22}.
#' @param medium Integer giving the size of the font used in the legend and
#' facet labels. Default is \code{18}.
#' @param small Integer giving the size of the font used in the rest of the plot.
#' Default is \code{14}.
#' @param scale_font Numeric used to scale all font sizes. Default is \code{1}.
#' @param rot_xaxis_text Logical indicating if x-axis text should be
#' rotated by 45 degree. Default is \code{FALSE}.
#' @param rot_strips_y Logical indicating if facet labels should be
#' rotated by 90 degree. Default is \code{TRUE}.
#' @family theme functions
#' @export
#'
#' @examples
#' \dontrun{nums_agg <- agg_data(data = ref_nums, groups = c("species", "time"), fun = sum)
#' ggplot2::ggplot(data = nums_agg, ggplot2::aes(x = time, y = atoutput)) +
#'    ggplot2::facet_wrap(~species) +
#'    theme_atlantis()}

theme_atlantis <- function(large = 22, medium = 18, small = 14, scale_font = 1, rot_xaxis_text = FALSE, rot_strips_y = TRUE){
  ggplot2::theme(
    text                = ggplot2::element_text(family = "sans", size = large * scale_font),
    title               = ggplot2::element_text(hjust = .5),
    axis.title.x        = ggplot2::element_text(hjust = .5),
    axis.title.y        = ggplot2::element_text(hjust = .5, vjust = 0.3),
    #    axis.text  =  element_text(),  #	inherits from text
    axis.text.x         = ggplot2::element_text(angle = ifelse(rot_xaxis_text, 45, 0), hjust = ifelse(rot_xaxis_text, 1, 0.5), size = scale_font * small, colour = "black"),
    axis.text.y         = ggplot2::element_text(size = small * scale_font, colour = "black"),
    # axis.line seems to be broken at the moment in ggplot...
    # https://github.com/hadley/ggplot2/issues/1567
    # axis.line           = ggplot2::element_line(colour = "black", size = 5),
       axis.line.x  =	ggplot2::element_line(colour = "black"),	#	inherits from axis.line
       axis.line.y	=	ggplot2::element_line(colour = "black"),	#	inherits from axis.line
    #    axis.ticks   =	element_line(),	#	inherits from line
    #    axis.ticks.x	=	element_line(),	#	inherits from axis.ticks
    #    axis.ticks.y	=	element_line(),	#	inherits from axis.ticks
    #    axis.ticks.length	=	unit(),
    #    axis.ticks.margin	=	unit(),
    plot.margin         = grid::unit(c(1,1,1,1), "mm"),
    #    plot.background     =	element_rect(),	#	inherits from rect
    #    plot.title	         =	element_text(),	#	 inherits from title
    panel.grid          = ggplot2::element_blank(),
    panel.border        = ggplot2::element_blank(),
    panel.background    = ggplot2::element_blank(),
    #    panel.margin  =	unit	,
    #    panel.grid.major	=	element_line(),	#	inherits from panel.grid
    #    panel.grid.minor	=	element_line(),	#	inherits from panel.grid
    #    panel.grid.major.x	=	element_line(),	#	inherits from panel.grid.major
    #    panel.grid.major.y	=	element_line(),	#	inherits from panel.grid.major
    #    panel.grid.minor.x	=	element_line(),	#	inherits from panel.grid.minor
    #    panel.grid.minor.y	=	element_line(),	#	inherits from panel.grid.minor
    legend.position     = "bottom",
    legend.text         = ggplot2::element_text(size = small * scale_font),
    legend.key.width    = grid::unit(0.75, "cm"),
    legend.title        = ggplot2::element_text(size = medium * scale_font),
    #    legend.background   =	element_rect(),	#	inherits from rect
    #    legend.margin       =	unit(),
    #    legend.key          =	element_rect(fill = NULL, colour = NULL, size = NULL, linetype = NULL, color = NULL)	,	#	inherits from rect
    #    legend.key.size	   =	unit,	#	inherits from legend.key.size
    #    legend.key.height	 =	unit,	#	inherits from legend.key.size
    #    legend.text.align	 =	,	#	number from 0 (left) to 1 (right)
    #    legend.title.align	 =	,	#	number from 0 (left) to 1 (right)
    #     legend.direction	   =	"horizontal",
    #    legend.justification	=	,	#	center or two-element numeric vector
    legend.box	         =	"horizontal",
    #    legend.box.just	   =	,	#	top, "bottom", "left", or "right"
    strip.background    = ggplot2::element_blank(),
    strip.text          = ggplot2::element_text(size = small),
    strip.text.x        = ggplot2::element_text(size = scale_font * small),	#	inherits from strip.text
    strip.text.y	      = ggplot2::element_text(size = scale_font * small, angle = ifelse(rot_strips_y, 0, 90))	#	inherits from strip.text
  )
}



