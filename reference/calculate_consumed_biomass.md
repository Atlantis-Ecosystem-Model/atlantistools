# Calculate the consumed biomass in \[t\] of prey j by predator i.

Consumption data is extracted from output\[...\]PROD.nc. Age based
groups are stored as "Eat\_" non age based groups as "Grazing\_". Units
are mg N m^-3 d^-1. Factors are species, time, box and agecl (if
present). We will refer to species as pred from here on to indicate the
predator perspective. Diet contribution data is extracted from
DietCheck.txt. Currently this only works for models based on the trunk
code. Units are The consumed biomass is calculated as follows: -
Calculate consumed biomass as Eat (or Grazing) \* boxvolume per time,
pred, agecl, box. - Convert to biomass in \[t\]. - Combine with diet
contributions and calculate consumed biomass of prey species.

## Usage

``` r
calculate_consumed_biomass(eat, grazing, dm, vol, bio_conv)
```

## Arguments

- eat:

  Dataframe with information about consumption for age-based groups.
  Should be generated with
  [`load_nc`](https://andybeet.github.io/atlantistools/reference/load_nc.md).

- grazing:

  Dataframe with information about consumption for non-age-based groups.
  Should be generated with
  [`load_nc`](https://andybeet.github.io/atlantistools/reference/load_nc.md).

- dm:

  Dataframe with information about diet contributions per predator.
  Should be generated with
  [`load_dietcheck`](https://andybeet.github.io/atlantistools/reference/load_dietcheck.md)
  using `convert_names = TRUE`.

- vol:

  Dataframe with information about volume per polygon and layer. Should
  be generated with
  [`load_nc_physics`](https://andybeet.github.io/atlantistools/reference/load_nc_physics.md).

- bio_conv:

  Numeric value to transform weight in mg N to tonnes. Should be
  generated with
  [`get_conv_mgnbiot`](https://andybeet.github.io/atlantistools/reference/get_conv_mgnbiot.md).

## Value

Dataframe with columns 'pred', 'agecl', 'polygon', 'time', 'prey'.
Consumed biomass in \[t\] is stored in column 'atoutput'.

## Examples

``` r
# 1. Using built-in datasets.
d <- system.file("extdata", "setas-model-new-trunk", package = "atlantistools")
prm_biol <- file.path(d, "VMPA_setas_biol_fishing_Trunk.prm")

bio_conv <- get_conv_mgnbiot(prm_biol = prm_biol)

df <- calculate_consumed_biomass(eat = ref_eat, grazing = ref_grazing, dm = ref_dm,
                                 vol = ref_vol, bio_conv = bio_conv)
#> 50% matching timesteps between PROD.nc and DietCheck.txt
#> 0.21% data is lost due to missing diet data despite available eat data.
#> 21.97% data is lost due to missing eat data despite available diet data.

# 2. Read in dataframes from existing Atlantis simulation.
bboxes <- get_boundary(boxinfo = load_box(file.path(d, "VMPA_setas.bgm")))
nc_gen <- file.path(d, "outputSETAS.nc")
nc_prod <- file.path(d, "outputSETASPROD.nc")
prm_run <- file.path(d, "VMPA_setas_run_fishing_F_Trunk.prm")
fgs <- file.path(d, "SETasGroupsDem_NoCep.csv")
init <- file.path(d, "INIT_VMPA_Jan2015.nc")

bps <- load_bps(fgs = fgs, init = init)

groups_age <- c("Planktiv_S_Fish", "Pisciv_S_Fish")
groups_rest <- c("Cephalopod", "Megazoobenthos", "Diatom", "Lab_Det", "Ref_Det")

df_eat <- load_nc(nc = nc_prod, bps = bps, fgs = fgs,
               select_groups = groups_age, select_variable = "Eat",
               prm_run = prm_run, bboxes = bboxes)
#> Reading in the nc file: /home/runner/work/_temp/Library/atlantistools/extdata/setas-model-new-trunk/outputSETASPROD.nc
df_grz <- load_nc(nc = nc_prod, bps = bps, fgs = fgs,
               select_groups = groups_rest, select_variable = "Grazing",
               prm_run = prm_run, bboxes = bboxes)
#> Reading in the nc file: /home/runner/work/_temp/Library/atlantistools/extdata/setas-model-new-trunk/outputSETASPROD.nc
df_dm <- load_dietcheck(dietcheck = file.path(d, "outputSETASDietCheck.txt"),
                        fgs = fgs, prm_run = prm_run, convert_names = TRUE)
vol <- load_nc_physics(nc = nc_gen, select_physics = "volume",
                       prm_run = prm_run, bboxes = bboxes, aggregate_layers = FALSE)

df <- calculate_consumed_biomass(eat = df_eat, grazing = df_grz, dm = df_dm,
                                 vol = vol, bio_conv = bio_conv)
#> 50% matching timesteps between PROD.nc and DietCheck.txt
#> 11.61% data is lost due to missing diet data despite available eat data.
#> 21.97% data is lost due to missing eat data despite available diet data.
```
