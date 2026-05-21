# Get boundary boxes from Atlantis box information.

Use the output from
[`load_box`](https://andybeet.github.io/atlantistools/reference/load_box.md)
and obtain a `vector` specifying which boxes are along the boundary.

## Usage

``` r
get_boundary(boxinfo)
```

## Arguments

- boxinfo:

  A `list` as returned from
  [`load_box`](https://andybeet.github.io/atlantistools/reference/load_box.md).

## Value

A `vector` specifying which boxes are on the boundary.

## See also

[`load_box`](https://andybeet.github.io/atlantistools/reference/load_box.md)

Other get functions:
[`get_colpal()`](https://andybeet.github.io/atlantistools/reference/get_colpal.md),
[`get_conv_mgnbiot()`](https://andybeet.github.io/atlantistools/reference/get_conv_mgnbiot.md),
[`get_groups()`](https://andybeet.github.io/atlantistools/reference/get_groups.md)

Other get functions:
[`get_colpal()`](https://andybeet.github.io/atlantistools/reference/get_colpal.md),
[`get_conv_mgnbiot()`](https://andybeet.github.io/atlantistools/reference/get_conv_mgnbiot.md),
[`get_groups()`](https://andybeet.github.io/atlantistools/reference/get_groups.md)

## Examples

``` r
d <- system.file("extdata", "setas-model-new-trunk", package = "atlantistools")
boxes <- load_box(bgm = file.path(d, "VMPA_setas.bgm"))
get_boundary(boxinfo = boxes)
#> [1]  0  6  7  8  9 10
```
