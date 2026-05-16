# Customized theme used in all plots.

This function is a customized theme for ggplot2 plots. It's applied by
default to all plots created within `atlantistools`.

## Usage

``` r
theme_atlantis(
  large = 22,
  medium = 18,
  small = 14,
  scale_font = 1,
  rot_xaxis_text = FALSE,
  rot_strips_y = TRUE
)
```

## Arguments

- large:

  Integer giving the size of the font for the main parts of the plot.
  Default is `22`.

- medium:

  Integer giving the size of the font used in the legend and facet
  labels. Default is `18`.

- small:

  Integer giving the size of the font used in the rest of the plot.
  Default is `14`.

- scale_font:

  Numeric used to scale all font sizes. Default is `1`.

- rot_xaxis_text:

  Logical indicating if x-axis text should be rotated by 45 degree.
  Default is `FALSE`.

- rot_strips_y:

  Logical indicating if facet labels should be rotated by 90 degree.
  Default is `TRUE`.

## Examples

``` r
if (FALSE) nums_agg <- agg_data(data = ref_nums, groups = c("species", "time"), fun = sum)
ggplot2::ggplot(data = nums_agg, ggplot2::aes(x = time, y = atoutput)) +
   ggplot2::facet_wrap(~species) +
   theme_atlantis() # \dontrun{}
#> Error: object 'nums_agg' not found
```
