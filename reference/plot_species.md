# Create species specific overview plot.

This plotting routine is based on Raphael's (Ifremer) plotting routine
used during model calibration. Currently 6 plots are created by
default: - Biomass over time - Biomass over time per age - StructN over
time per age - ResN over time per age - Condition over time per age -
Numbers over time per age

## Usage

``` r
plot_species(data_pre, species)
```

## Arguments

- data_pre:

  List of preprocessed Atlantis simulation. The list of dataframes
  should be created with `model-preprocess.Rmd`.

- species:

  Character srtring giving the name of the species to plot. Only age
  based species are supported.

## Value

ggplot2 object of class grob

## See also

Other plot functions:
[`plot_bar()`](https://andybeet.github.io/atlantistools/reference/plot_bar.md),
[`plot_boxes()`](https://andybeet.github.io/atlantistools/reference/plot_boxes.md),
[`plot_diet()`](https://andybeet.github.io/atlantistools/reference/plot_diet.md),
[`plot_diet_bec_dev()`](https://andybeet.github.io/atlantistools/reference/plot_diet_bec_dev.md),
[`plot_line()`](https://andybeet.github.io/atlantistools/reference/plot_line.md),
[`plot_rec()`](https://andybeet.github.io/atlantistools/reference/plot_rec.md)

## Examples

``` r
plot <- plot_species(preprocess, species = "Shallow piscivorous fish")
# Use grid.arrange to draw the plot on the current device
gridExtra::grid.arrange(plot)
```
