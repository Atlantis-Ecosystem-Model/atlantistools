# Plot layout of boxes!

Plot layout of boxes!

## Usage

``` r
plot_boxes(data, color_boxes = TRUE)
```

## Arguments

- data:

  Dataframe to be plotted.

- color_boxes:

  logical indicating if polygons should be color coded or not. Default
  is `TRUE`.

## Value

ggplot2 object

## See also

Other plot functions:
[`plot_bar()`](https://andybeet.github.io/atlantistools/reference/plot_bar.md),
[`plot_diet()`](https://andybeet.github.io/atlantistools/reference/plot_diet.md),
[`plot_line()`](https://andybeet.github.io/atlantistools/reference/plot_line.md),
[`plot_rec()`](https://andybeet.github.io/atlantistools/reference/plot_rec.md),
[`plot_species()`](https://andybeet.github.io/atlantistools/reference/plot_species.md)

## Examples

``` r
d <- system.file("extdata", "setas-model-new-trunk", package = "atlantistools")
bgm_data <- convert_bgm(file.path(d, "VMPA_setas.bgm"))

# Use color coding for polygons.
plot_boxes(bgm_data)


# Only use text to indicate polygons.
plot_boxes(bgm_data, color_boxes = FALSE)
```
