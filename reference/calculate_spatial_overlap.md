# Calculate 3d overlap of predator groups with their prey over time using Schoener Index.

Calculate 3d overlap of predator groups with their prey over time using
Schoener Index.

## Usage

``` r
calculate_spatial_overlap(biomass_spatial, dietmatrix, agemat)
```

## Arguments

- biomass_spatial:

  Biomass timeseries of each group and ageclass per polygon and layer.
  This dataframe should be generated with
  [`calculate_biomass_spatial`](https://andybeet.github.io/atlantistools/reference/calculate_biomass_spatial.md).

- dietmatrix:

  Availability matrix given in the biological parameter file. This
  dataframe should be generated with
  [`load_dietmatrix`](https://andybeet.github.io/atlantistools/reference/load_dietmatrix.md).
  Please make sure to use `convert_names = TRUE` in
  [`load_dietmatrix`](https://andybeet.github.io/atlantistools/reference/load_dietmatrix.md).

- agemat:

  First mature age class for age structured groups. This dataframe
  should be generated with
  [`prm_to_df`](https://andybeet.github.io/atlantistools/reference/prm_to_df.md)
  using "age_mat" as parameter.

## Value

List of Schoener's similarity indices ranging from 1 (perfect overlap)
to 0 (zero overlap). Dataframe in first listentry gives pred, pred
agecl, prey, preyageclass specific index. Dataframe in second listentry
gives index per pred, pred agecl. Si is calculated as weighted mean with
the availabilities as weights.

## Examples

``` r
# Using built in datasets.
d <- system.file("extdata", "setas-model-new-trunk", package = "atlantistools")
prm_biol <- file.path(d, "VMPA_setas_biol_fishing_Trunk.prm")
fgs <- file.path(d, "SETasGroupsDem_NoCep.csv")

dietmatrix <- load_dietmatrix(prm_biol, fgs, convert_names = TRUE)
agemat <- prm_to_df(prm_biol = prm_biol, fgs = fgs,
                    group = get_age_acronyms(fgs = fgs),
                    parameter = "age_mat")

sp_overlap <- calculate_spatial_overlap(biomass_spatial = ref_bio_sp,
                                        dietmatrix = dietmatrix,
                                        agemat = agemat)
```
