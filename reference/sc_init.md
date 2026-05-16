# Sanity check initial conditions file

Sanity check initial conditions file

## Usage

``` r
sc_init(
  init,
  prm_biol,
  fgs,
  bboxes,
  pred = NULL,
  set_avail = NULL,
  version_flag = 2
)

plot_sc_init(df, mult_mum, mult_c, pred = NULL)
```

## Arguments

- init:

  Character string giving the connection of the initial conditions
  netcdf file. The filename usually contains `init` and ends in `.nc`.

- prm_biol:

  Character string giving the connection to the biological
  parameterfile. The filename usually contains `biol_fishing` and does
  end in `.prm`.

- fgs:

  Character string giving the connection to the functional groups file.
  The filename usually contains `Groups` and does end in `.csv`.

- bboxes:

  Integer vector giving the box-id of the boundary boxes. Can be created
  with `get_boundary`.

- pred:

  Vector of predator acronyms to check. If `NULL` (default) all age
  based predators are selected.

- set_avail:

  Numeric value. All present availabilities can be set to a specific
  value. Default value is `NULL` which results in no changes to the
  present availability matrix.

- version_flag:

  The version of ATLANTIS model. 1 for bec_dev, 2 for trunk.
  `default is 2.`.

- df:

  Dataframe to pass to `plot_sc_init()`. df should be generated with
  sc_init or read in from \*.rda (also generated with sc_init()).

- mult_mum:

  Numeric vector of multiplication factors applied to the initial mum
  values.

- mult_c:

  Numeric vector of multiplication factors applied to the initial C
  values.

## Value

Dataframe/ Plot.

## Examples

``` r
d <- system.file("extdata", "setas-model-new-trunk", package = "atlantistools")
init <- file.path(d, "INIT_VMPA_Jan2015.nc")
prm_biol <- file.path(d, "VMPA_setas_biol_fishing_Trunk.prm")
fgs <- file.path(d, "SETasGroupsDem_NoCep.csv")
bboxes <- get_boundary(load_box(bgm = file.path(d, "VMPA_setas.bgm")))

data1 <- sc_init(init, prm_biol, fgs, bboxes)
#> Read in data from out.nc, init.nc and prm.biol!
if (FALSE) { # \dontrun{
dir <- system.file("extdata", "gns", package = "atlantistools")
fgs <- "functionalGroups.csv"
init <- "init_simple_NorthSea.nc"
prm_biol <- "NorthSea_biol_fishing.prm"
bboxes <- get_boundary(load_box(dir = dir, bgm = "NorthSea.bgm"))
mult_mum <- seq(0.5, 10, by = 1)
mult_c <- seq(0.5, 10, by = 1)
no_avail <- FALSE
save_to_disc <- FALSE
data1 <- sc_init(dir, init, prm_biol, fgs, bboxes, save_to_disc = FALSE)
plot_sc_init(df = data1, mult_mum, mult_c)
plot_sc_init(df = data1, mult_mum, mult_c, pred = "Cod")

data2 <- sc_init(dir, init, prm_biol, fgs, bboxes, pred = "Cod", save_to_disc = FALSE)
plot_sc_init(df = data2, mult_mum, mult_c)
} # }
```
