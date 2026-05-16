# Plot contribution of diet contents for each functional group.

Visualize diet proportions form predator and prey perspective. The upper
panel plot shows the predator perspective while the lower panel plot
shows the prey perspective for a given group. Please note that this
function uses the SpecPredMort.txt file to visualize feeding
interactions and therefore is only an indication of the realized true
diet within the model. Please use `plot_diet` instead.

## Usage

``` r
plot_diet_bec_dev(data, species = NULL, wrap_col, combine_thresh = 15)
```

## Arguments

- data:

  SpecPredMort.txt read in with `load_spec_mort`.

- species:

  Character string giving the acronyms of the species you aim to plot.
  Default is `NULL` resulting in all available species being ploted.

- wrap_col:

  Character specifying the column of the dataframe to be used as
  multipanel plot. In case you aim to plot SpecMort.txt data use either
  "agecl" or "stanza".

- combine_thresh:

  Number of different categories to plot. Lets say predator X has eaten
  20 different prey items. If you only want to show the 3 most important
  prey items set `combine_thresh` to 3. As rule of thumb values \< 10
  are useful otherwise to many colors are used in the plots. Default is
  `15`.

## Value

List of ggplot2 objects.

## See also

Other plot functions:
[`plot_bar()`](https://andybeet.github.io/atlantistools/reference/plot_bar.md),
[`plot_boxes()`](https://andybeet.github.io/atlantistools/reference/plot_boxes.md),
[`plot_diet()`](https://andybeet.github.io/atlantistools/reference/plot_diet.md),
[`plot_line()`](https://andybeet.github.io/atlantistools/reference/plot_line.md),
[`plot_rec()`](https://andybeet.github.io/atlantistools/reference/plot_rec.md),
[`plot_species()`](https://andybeet.github.io/atlantistools/reference/plot_species.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Plot SpecMort.txt per ageclass.
plots <- plot_diet_bec_dev(preprocess_setas$diet_specmort, wrap_col = "agecl")
gridExtra::grid.arrange(plots[[1]])

# Only plot specific species
plots <- plot_diet_bec_dev(preprocess_setas$diet_specmort, species = "CEP", wrap_col = "agecl")
gridExtra::grid.arrange(plots[[1]])

# Plot SpecMort.txt per stanza First we need to transform the ageclasses to stanzas.
d <- system.file("extdata", "setas-model-new-becdev", package = "atlantistools")
diet_stanza <- combine_ages(dir = d,
                            data = preprocess_setas$diet_specmort,
                            col = "pred",
                            prm_biol = "VMPA_setas_biol_fishing_New.prm")
plots <- plot_diet_bec_dev(diet_stanza, wrap_col = "stanza")
gridExtra::grid.arrange(plots[[1]])
} # }
```
