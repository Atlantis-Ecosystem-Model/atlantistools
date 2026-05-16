# Dietcheck.

Dietcheck.

## Usage

``` r
ref_dm
```

## Format

- time:

  Simulation time in years. Modeltimestep was converted to actual time
  based on the settings in the 'run.prm' file.

- pred:

  Name of the functional groups given as character string. The names
  match with the column 'LongName' in the functionalGroups.csv file.

- agecl:

  Ageclass given as integer from 1 to NumCohorts.

- prey:

  Name of the functional groups given as character string. The names
  match with the column 'LongName' in the functionalGroups.csv file.

- atoutput:

  Obseravtion column storing the actual output value. Diet contribution
  in percentage.
