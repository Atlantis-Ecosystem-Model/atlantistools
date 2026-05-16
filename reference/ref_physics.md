# Physical variables.

Physical variables.

## Usage

``` r
ref_physics
```

## Format

- variable:

  Name of the physical variable: "salt", "NO3", "NH3", "Temp", "Chl_a"
  and "Denitrifiction".

- polygon:

  Boxid starting from 0 to numboxes - 1.

- layer:

  Layerid starting from 0 to numlayers - 1.

- time:

  Simulation time in years. Modeltimestep was converted to actual time
  based on the settings in the 'run.prm' file.

- atoutput:

  Obseravtion column storing the actual output value. units are salt =
  PSU; NO3, NH3 = mg N m-3; Temp = degrees Celcius; Chl_a,
  Denitrifiction = ?
