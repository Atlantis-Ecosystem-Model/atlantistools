# Reserve nitrogen.

Reserve nitrogen.

## Usage

``` r
ref_resn
```

## Format

- species:

  Name of the functional groups given as character string. The names
  match with the column 'LongName' in the functionalGroups.csv file.

- agecl:

  Ageclass given as integer from 1 to NumCohorts.

- polygon:

  Boxid starting from 0 to numboxes - 1.

- layer:

  Layerid starting from 0 to numlayers - 1.

- time:

  Simulation time in years. Modeltimestep was converted to actual time
  based on the settings in the 'run.prm' file.

- atoutput:

  Obseravtion column storing the actual output value. Reserve weight in
  mg N.
