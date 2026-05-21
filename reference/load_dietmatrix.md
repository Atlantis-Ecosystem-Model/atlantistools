# Extract the dietmatrix from the biological parameterfile

Extracts the diet matrix as long dataframe from the biological paremeter
file of any ATLANTIS simulation.

## Usage

``` r
load_dietmatrix(prm_biol, fgs, transform = TRUE, convert_names = FALSE)

write_diet(dietmatrix, prm_biol, save_to_disc = TRUE)
```

## Arguments

- prm_biol:

  Character string giving the connection to the biological
  parameterfile. The filename usually contains `biol_fishing` and does
  end in `.prm`.

- fgs:

  Character string giving the connection to the functional groups file.
  The filename usually contains `Groups` and does end in `.csv`.

- transform:

  Boolean indicating if the returned dataframe is displayed in "long"
  (`transform = TRUE, default`) or "wide" (`transform = FALSE`) format.
  You should use the "wide" format in case you aim to change your diet
  matrix entries.

- convert_names:

  Logical indicating if group codes are transformed to LongNames
  (`TRUE`) or not (default = `FALSE`).

- dietmatrix:

  Dataframe of the ATLANTIS dietmatrix generated with `load_dietmatrix`
  using `transform = FALSE`.

- save_to_disc:

  Logical indicating if the resulting prm file should be overwritten
  (`TRUE`) or not (`FALSE`). Defaults to `TRUE`.

## Value

dataframe of the availability matrix in long format with columns pred,
pred_stanza (1 = juvenile, 2 = adult), prey_stanza, prey, avail, code.

## Examples

``` r
# Can be applied to trunk models.
d <- system.file("extdata", "setas-model-new-trunk", package = "atlantistools")
prm_biol <- file.path(d, "VMPA_setas_biol_fishing_Trunk.prm")
fgs <- file.path(d, "SETasGroupsDem_NoCep.csv")

dm <- load_dietmatrix(prm_biol, fgs)
head(dm, n = 10)
#> # A tibble: 10 × 7
#>    pred  pred_stanza prey_stanza code       prey  avail prey_id
#>    <chr>       <dbl>       <dbl> <chr>      <chr> <dbl>   <int>
#>  1 FPS             1           1 pPREY1FPS1 FPS    0          1
#>  2 FPS             1           1 pPREY1FPS1 FVS    0          2
#>  3 FPS             1           1 pPREY1FPS1 CEP    0          3
#>  4 FPS             1           1 pPREY1FPS1 BML    0.01       4
#>  5 FPS             1           1 pPREY1FPS1 PL     0          5
#>  6 FPS             1           1 pPREY1FPS1 DL     0          6
#>  7 FPS             1           1 pPREY1FPS1 DR     0          7
#>  8 FPS             1           1 pPREY1FPS1 DC     0          8
#>  9 FPS             1           1 pPREY1FPS1 DLsed  0          9
#> 10 FPS             1           1 pPREY1FPS1 DRsed  0         10

# Use write_diet to update your existing parameterfile.
dietmatrix <- load_dietmatrix(prm_biol, fgs, transform = FALSE)

# Write is set to FALSE here for technical reasons. Make sure to set it to TRUE in case you
# want to update your file.
new_diet <- write_diet(dietmatrix, prm_biol, save_to_disc = FALSE)
```
