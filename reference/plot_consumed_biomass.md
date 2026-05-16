# Circle diagram to visualize the consumed biomass for the whole system.

Circle diagram to visualize the consumed biomass for the whole system.

## Usage

``` r
plot_consumed_biomass(bio_consumed, select_time = NULL, show = 0.95)
```

## Arguments

- bio_consumed:

  Consumed biomass of prey groups by predatorgroup and agecl in tonnes
  for each timestep and polygon. Dataframe with columns 'pred', 'agecl',
  'polygon', 'time', 'prey'. Consumed biomass in \[t\] is stored in
  column 'atoutput'. Should be generated with
  `link{calculate_consumed_biomass}`.

- select_time:

  Numeric value to control the simulation time in years to visualize. By
  default the start of the simulation is shown. Default is `NULL`.

- show:

  Numeric value between 0 - 1 to control the amount of links shown.
  Default is `0.95`. Thus, the most important 95 are grouped together as
  'Rest'.

## Value

plot

## Examples

``` r

if (FALSE) { # \dontrun{
# Need to fix NAs in init cdf at some point.
plot_consumed_biomass(ref_bio_cons)
plot_consumed_biomass(ref_bio_cons, select_time = 2)
plot_consumed_biomass(ref_bio_cons, select_time = 2, show = 0.99)

# Add some gns testing.
load("z:/Atlantis_models/Runs/dummy_01_ATLANTIS_NS/preprocess-north-sea.rda", verbose = T)
plot_consumed_biomass(result$biomass_consumed, show = 0.95, select_time = 2.2)

times <- c(0.2, seq(10, 90, 10), 99.8)
par(mfcol = c(3, 4))
for (i in seq_along(times)) {
  plot_consumed_biomass(result$biomass_consumed, show = 0.95, select_time = times[i])
}

times <- seq(0.2, 2.2, 0.2)
# times <- times[times != 1]
par(mfcol = c(3, 4))
for (i in seq_along(times)) {
  plot_consumed_biomass(result$biomass_consumed, show = 0.95, select_time = times[i])
}
} # }
```
