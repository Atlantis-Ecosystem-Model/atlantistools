# Consumed biomass.

Consumed biomass.

## Usage

``` r
ref_bio_cons
```

## Format

- pred:

  Name of the functional groups given as character string. The names
  match with the column 'LongName' in the functionalGroups.csv file.

- agecl:

  Ageclass given as integer from 1 to NumCohorts.

- polygon:

  Boxid starting from 0 to numboxes - 1.

- time:

  Simulation time in years. Modeltimestep was converted to actual time
  based on the settings in the 'run.prm' file.

- prey:

  Name of the functional groups given as character string. The names
  match with the column 'LongName' in the functionalGroups.csv file.

- atoutput:

  Obseravtion column storing the actual output value. Consumed biomass
  in tonnes.
