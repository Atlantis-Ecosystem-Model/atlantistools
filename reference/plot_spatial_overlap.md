# Plot spatial overlap.

Plot spatial overlap.

## Usage

``` r
plot_spatial_overlap(df_list)
```

## Arguments

- df_list:

  List of dataframes generated with
  [`calculate_spatial_overlap`](https://andybeet.github.io/atlantistools/reference/calculate_spatial_overlap.md)

## Value

ggplot2 plot.

## Examples

``` r
sp_overlap <- calculate_spatial_overlap(ref_bio_sp, ref_dietmatrix, ref_agemat)
if (FALSE) { # \dontrun{
plot_spatial_overlap(sp_overlap)
} # }
```
