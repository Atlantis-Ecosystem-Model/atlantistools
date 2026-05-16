# Flip layers for visualization.

Within Atlantis the water column id 0 is the water column closest to the
sediment. In order to simplify graphical interpretation of vertical
plots this order is reversed. The surface layer is 1 by default. The
sediment layer id is equal to the number of total layers. Please note
that this is only used for graphical display.

## Usage

``` r
flip_layers(data)
```

## Arguments

- data:

  dataframe with columns polygon and layer. layer id is based on
  atlantis output (0 = layer closest to the sediment)

## Value

dataframe with flipped layerids. 1 = surface.

## Examples

``` r
data <- rbind(expand.grid(species = "sp1", polygon = 0, layer = 0:7),
              expand.grid(species = "sp1", polygon = 1, layer = 0:4),
              expand.grid(species = "sp1", polygon = 2, layer = 0:2),
              expand.grid(species = "sp1", polygon = 3, layer = c(0:3, 7)))
data$atoutput <- runif(nrow(data), min = 0, max = 2)
flip_layers(data)
#>    species polygon   atoutput layer
#> 1      sp1       0 0.16150028     7
#> 2      sp1       0 1.66866607     6
#> 3      sp1       0 1.20152177     5
#> 4      sp1       0 0.31441688     4
#> 5      sp1       0 0.01479888     3
#> 6      sp1       0 0.93278699     2
#> 7      sp1       0 0.99555478     1
#> 8      sp1       0 0.57953449     8
#> 9      sp1       1 1.46576397     5
#> 10     sp1       1 1.54504302     4
#> 11     sp1       1 1.74920132     3
#> 12     sp1       1 0.34988125     2
#> 13     sp1       1 0.06848267     1
#> 14     sp1       2 0.64077146     3
#> 15     sp1       2 0.80465648     2
#> 16     sp1       2 0.39133967     1
#> 17     sp1       3 0.80707623     4
#> 18     sp1       3 0.12732291     3
#> 19     sp1       3 0.77740263     2
#> 20     sp1       3 1.95109567     1
#> 21     sp1       3 0.57978459     8
```
