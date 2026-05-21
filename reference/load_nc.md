# Load Atlantis outputfiles (netcdf)

This function loads Atlantis outputfiles (any netcdf file) and converts
them to a `data.frame`.

## Usage

``` r
load_nc(
  nc,
  fgs,
  bps,
  select_groups,
  select_variable,
  prm_run,
  bboxes,
  check_acronyms = TRUE,
  warn_zeros = FALSE,
  report = TRUE
)
```

## Arguments

- nc:

  Character string giving the connection of the netcdf file to read in.
  The filename usually contains `output` and ends in `.nc`".

- fgs:

  Character string giving the connection to the functional groups file.
  The filename usually contains `Groups` and does end in `.csv`.

- bps:

  Vector of character strings giving the complete list of epibenthic
  functional groups (Only present in the sediment layer). The names have
  to match the column 'Name' in the 'functionalGroups.csv' file. Can be
  created with `load_bps`.#'

- select_groups:

  Character vector of funtional groups which shall be read in. Names
  have to match the ones used in the netcdf file. Check column "Name" in
  "functionalGroups.csv" for clarification.

- select_variable:

  Character value specifying which variable to load. Only one variable
  of the options available (i.e.,
  `c( "N", "Nums", "ResN", "StructN", "Eat", "Growth", "Prodn", "Grazing") `)
  can be loaded at a time.

- prm_run:

  Character string giving the connection of the run parameterfile. The
  filename usually contains `run_fishing` and ends in `.prm`".

- bboxes:

  Integer vector giving the box-id of the boundary boxes. Can be created
  with `get_boundary`.

- check_acronyms:

  Logical testing if functional-groups in select_groups are inactive in
  the current model run. They will be omitted in the output.

- warn_zeros:

  Logical indicating if check for actual zeros in the data shall be
  printed or not. Default is `FALSE`.

- report:

  Logical indicating if progress bars shall be printed (`TRUE`) or not
  (`FALSE`). Default is `TRUE`.

## Value

A `data.frame` in long format with the following column names: species,
timestep, polygon, agecl, and atoutput (i.e., variable).

## See also

Other load functions:
[`load_box()`](https://andybeet.github.io/atlantistools/reference/load_box.md),
[`load_bps()`](https://andybeet.github.io/atlantistools/reference/load_bps.md),
[`load_dietcheck()`](https://andybeet.github.io/atlantistools/reference/load_dietcheck.md),
[`load_fgs()`](https://andybeet.github.io/atlantistools/reference/load_fgs.md),
[`load_init()`](https://andybeet.github.io/atlantistools/reference/load_init.md),
[`load_init_age()`](https://andybeet.github.io/atlantistools/reference/load_init_age.md),
[`load_mort()`](https://andybeet.github.io/atlantistools/reference/load_mort.md),
[`load_nc_physics()`](https://andybeet.github.io/atlantistools/reference/load_nc_physics.md),
[`load_rec()`](https://andybeet.github.io/atlantistools/reference/load_rec.md),
[`load_spec_mort()`](https://andybeet.github.io/atlantistools/reference/load_spec_mort.md),
[`load_spec_pred_mort()`](https://andybeet.github.io/atlantistools/reference/load_spec_pred_mort.md),
[`load_txt()`](https://andybeet.github.io/atlantistools/reference/load_txt.md)

## Examples

``` r
d <- system.file("extdata", "setas-model-new-trunk", package = "atlantistools")

nc <- file.path(d, "outputSETAS.nc")
fgs <- file.path(d, "SETasGroupsDem_NoCep.csv")
bps <- load_bps(init = file.path(d, "INIT_VMPA_Jan2015.nc"), fgs = fgs)
bboxes <- get_boundary(boxinfo = load_box(bgm = file.path(d, "VMPA_setas.bgm")))
prm_run <- file.path(d, "VMPA_setas_run_fishing_F_Trunk.prm")

test <- load_nc(nc = nc, bps = bps, fgs = fgs, prm_run = prm_run, bboxes = bboxes,
  select_groups = c("Planktiv_S_Fish", "Cephalopod", "Diatom"),
  select_variable = "ResN")
#> Reading in the nc file: /home/runner/work/_temp/Library/atlantistools/extdata/setas-model-new-trunk/outputSETAS.nc

test <- load_nc(nc = nc, bps = bps, fgs = fgs, prm_run = prm_run, bboxes = bboxes,
  select_groups = c("Planktiv_S_Fish", "Cephalopod", "Diatom"),
  select_variable = "Nums")
#> Reading in the nc file: /home/runner/work/_temp/Library/atlantistools/extdata/setas-model-new-trunk/outputSETAS.nc
```
