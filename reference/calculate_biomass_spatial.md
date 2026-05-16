# Calculate spatially explicit biomass (in \[t\]) for each group and ageclass per timestep.

Calculate spatially explicit biomass time series for each group and
ageclass within our model. Data is read in from 'output\[...\].nc'.
Biomass for age-based groups is calculated as (StructN \[mgN\] + ResN
\[mgN\]) \* Numbers \[individuals\]. Biomass for non age-based groups is
calculated as N \[mgN\] \* volume \[m^3\] (sediment-dz \[m\] / volume
\[m^3\] for epibenthic groups). mgN is converted to t based on the
stettings in the biol.prm file. Simulation time steps are converted to
time in years based on output timesteps given in run.prm.

## Usage

``` r
calculate_biomass_spatial(nums, sn, rn, n, vol_dz, bio_conv, bps)
```

## Arguments

- nums:

  Dataframe with information about numbers for age-based groups. Should
  be generated with
  [`load_nc`](https://andybeet.github.io/atlantistools/reference/load_nc.md).

- sn:

  Dataframe with information about structural nitrogen for age-based
  groups. Should be generated with
  [`load_nc`](https://andybeet.github.io/atlantistools/reference/load_nc.md).

- rn:

  Dataframe with information about reserve nitrogen for age-based
  groups. Should be generated with
  [`load_nc`](https://andybeet.github.io/atlantistools/reference/load_nc.md).

- n:

  Dataframe with information about nitrogen for non-ge-based groups.
  Should be generated with
  [`load_nc`](https://andybeet.github.io/atlantistools/reference/load_nc.md).

- vol_dz:

  Dataframe with information about volume and layer height per polygon.
  Should be generated with
  [`load_nc_physics`](https://andybeet.github.io/atlantistools/reference/load_nc_physics.md).

- bio_conv:

  Numeric value to transform weight in mg N to tonnes. Should be
  generated with
  [`get_conv_mgnbiot`](https://andybeet.github.io/atlantistools/reference/get_conv_mgnbiot.md).

- bps:

  Vector of character strings giving the complete list of epibenthic
  functional groups (Only present in the sediment layer). The names have
  to match the column 'Name' in the 'functionalGroups.csv' file. Should
  be generated with
  [`load_bps`](https://andybeet.github.io/atlantistools/reference/load_bps.md).

## Value

Dataframe with columns 'species', 'agecl', 'polygon', 'layer', 'time'.
Biomass in \[t\] is stored in column 'atoutput'.

## Examples

``` r
# 1. Using built in datasets.
d <- system.file("extdata", "setas-model-new-trunk", package = "atlantistools")
prm_biol <- file.path(d, "VMPA_setas_biol_fishing_Trunk.prm")
fgs <- file.path(d, "SETasGroupsDem_NoCep.csv")
init <- file.path(d, "INIT_VMPA_Jan2015.nc")
bps <- load_bps(fgs = fgs, init = init)

bio_conv <- get_conv_mgnbiot(prm_biol = prm_biol)

df <- calculate_biomass_spatial(nums = ref_nums, sn = ref_structn, rn = ref_resn, n = ref_n,
                                vol_dz = ref_vol_dz, bio_conv = bio_conv, bps = bps)

# 2. Read in dataframes from existing Atlantis simulation.
bboxes <- get_boundary(boxinfo = load_box(file.path(d, "VMPA_setas.bgm")))
nc_gen <- file.path(d, "outputSETAS.nc")
prm_run <- file.path(d, "VMPA_setas_run_fishing_F_Trunk.prm")

groups_age <- c("Planktiv_S_Fish", "Pisciv_S_Fish")
groups_rest <- c("Cephalopod", "Megazoobenthos", "Diatom", "Lab_Det", "Ref_Det")

nums <- load_nc(nc = nc_gen, bps = bps, fgs = fgs,
                select_groups = groups_age, select_variable = "Nums",
                prm_run = prm_run, bboxes = bboxes)
#> Reading in the nc file: /home/runner/work/_temp/Library/atlantistools/extdata/setas-model-new-trunk/outputSETAS.nc
sn <- load_nc(nc = nc_gen, bps = bps, fgs = fgs,
              select_groups = groups_age, select_variable = "StructN",
              prm_run = prm_run, bboxes = bboxes)
#> Reading in the nc file: /home/runner/work/_temp/Library/atlantistools/extdata/setas-model-new-trunk/outputSETAS.nc
rn <- load_nc(nc = nc_gen, bps = bps, fgs = fgs,
              select_groups = groups_age, select_variable = "ResN",
              prm_run = prm_run, bboxes = bboxes)
#> Reading in the nc file: /home/runner/work/_temp/Library/atlantistools/extdata/setas-model-new-trunk/outputSETAS.nc
n <- load_nc(nc = nc_gen, bps = bps, fgs = fgs,
             select_groups = groups_rest, select_variable = "N",
             prm_run = prm_run, bboxes = bboxes)
#> Reading in the nc file: /home/runner/work/_temp/Library/atlantistools/extdata/setas-model-new-trunk/outputSETAS.nc
vol <- load_nc_physics(nc = nc_gen, select_physics = c("volume", "dz"),
                       prm_run = prm_run, bboxes = bboxes, aggregate_layers = FALSE)

df <- calculate_biomass_spatial(nums = nums, sn = sn, rn = rn, n = n, vol_dz = vol,
                                bio_conv = bio_conv, bps = bps)

# 3. Read in dataframes from existing Atlantis simulation with Map().
vars <- list("Nums", "StructN", "ResN", "N")
grps <- list(groups_age, groups_age, groups_age, groups_rest)
dfs <- Map(load_nc, select_variable = vars, select_groups = grps,
           MoreArgs = list(nc = nc_gen, bps = bps, fgs = fgs,
                           prm_run = prm_run, bboxes = bboxes))
#> Reading in the nc file: /home/runner/work/_temp/Library/atlantistools/extdata/setas-model-new-trunk/outputSETAS.nc
#> Reading in the nc file: /home/runner/work/_temp/Library/atlantistools/extdata/setas-model-new-trunk/outputSETAS.nc
#> Reading in the nc file: /home/runner/work/_temp/Library/atlantistools/extdata/setas-model-new-trunk/outputSETAS.nc
#> Reading in the nc file: /home/runner/work/_temp/Library/atlantistools/extdata/setas-model-new-trunk/outputSETAS.nc

df <- calculate_biomass_spatial(nums = dfs[[1]], sn = dfs[[2]], rn = dfs[[3]], n = dfs[[4]],
                                vol_dz = vol, bio_conv = bio_conv, bps = bps)
```
