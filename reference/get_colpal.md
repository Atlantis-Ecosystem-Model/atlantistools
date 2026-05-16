# Create discrete color palette used in plots.

The colors are derived from http://colorbrewer2.org/ using the palette
12-class Paired. In addition 9 different gray tones were added in case
more than 12 categories are needed (This happens quite often with
feeding data).

## Usage

``` r
get_colpal()
```

## Value

Vector of colors in hexadecimal code.

## See also

Other get functions:
[`get_boundary()`](https://andybeet.github.io/atlantistools/reference/get_boundary.md),
[`get_conv_mgnbiot()`](https://andybeet.github.io/atlantistools/reference/get_conv_mgnbiot.md),
[`get_groups()`](https://andybeet.github.io/atlantistools/reference/get_groups.md)
