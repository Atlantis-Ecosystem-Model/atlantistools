# Nitrogen.

Nitrogen.

## Usage

``` r
ref_n
```

## Format

- species:

  Name of the functional groups given as character string. The names
  match with the column 'LongName' in the functionalGroups.csv file.

- polygon:

  Boxid starting from 0 to numboxes - 1.

- layer:

  Layerid starting from 0 to numlayers - 1.

- time:

  Simulation time in years. Modeltimestep was converted to actual time
  based on the settings in the 'run.prm' file.

- atoutput:

  Obseravtion column storing the actual output value. Nitrogen in mg N
  m-3.
