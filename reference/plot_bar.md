# Function to plot relative contribution of biomass and numbers per cohort.

Function to plot relative contribution of biomass and numbers per
cohort.

## Usage

``` r
plot_bar(
  data,
  x = "time",
  y = "atoutput",
  fill = "species",
  wrap = NULL,
  ncol = NULL
)
```

## Arguments

- data:

  Dataframe to be plotted.

- x:

  x-variable. Default is `'time'`.

- y:

  y-variable. Default is `'atoutput'`.

- fill:

  Column to use as filling colour. Default is `"species"`.

- wrap:

  Wraping column. Default is `'species'`

- ncol:

  Number of columns in multipanel plot. Default is `7`.

## Value

ggplot2 object

## See also

Other plot functions:
[`plot_boxes()`](https://andybeet.github.io/atlantistools/reference/plot_boxes.md),
[`plot_diet()`](https://andybeet.github.io/atlantistools/reference/plot_diet.md),
[`plot_line()`](https://andybeet.github.io/atlantistools/reference/plot_line.md),
[`plot_rec()`](https://andybeet.github.io/atlantistools/reference/plot_rec.md),
[`plot_species()`](https://andybeet.github.io/atlantistools/reference/plot_species.md)

## Examples

``` r
plot_bar(preprocess$biomass)


# Most models have a large number of groups. Please make sure to combine groups with a low
# contribution prior to plotting with \code{\link{combine_groups}}.
df <- combine_groups(preprocess$biomass, group_col = "species", combine_thresh = 3)
#> Joining with `by = join_by(species)`
#> Joining with `by = join_by(species)`
plot_bar(df)


# This function can also be used to plot age-specific data.
plot_bar(preprocess$nums_age, fill = "agecl", wrap = "species")


# Please use \code{\link{agg_perc}} to visualize the relative cohort structure over time.
df <- agg_perc(preprocess$nums_age, groups = c("time", "species"))
plot_bar(df, fill = "agecl", wrap = "species")
```
