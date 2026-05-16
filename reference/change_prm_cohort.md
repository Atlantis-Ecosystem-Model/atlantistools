# Change biological parameterfile for parameters which expect multiple values.

This function is used to help automate the calibration routine for
ATLANTIS models.

## Usage

``` r
change_prm_cohort(
  prm_biol,
  select_acronyms,
  roc,
  parameter,
  relative = TRUE,
  save_to_disc = TRUE
)
```

## Arguments

- prm_biol:

  Character string giving the connection to the biological
  parameterfile. The filename usually contains `biol_fishing` and does
  end in `.prm`.

- select_acronyms:

  Character vector of functional groups which shall be read in. Names
  have to match the ones used in the \*.prm file. Check column "Code" in
  "functionalGroups.csv" for clarification.

- roc:

  Matrix of multiplication factors which shall be applied to the old set
  of parameters. Please supply one row per selected group. Each row
  should have as many entries as the parameter itself. E.g. if you want
  to change the clearance rate for two fish groups you need to supply a
  matrix with 2 rows and 10 columns. In case you use different cohort
  numbers for age-structured groups supply a list of multiplication
  factors. Each list entry should be group specific.

- parameter:

  Character value of the model parameter which shall be changed. Only
  one parameter can be selected per function call.

- relative:

  Logical if TRUE values are changed relative to base values. If FALSE
  new values can be passed directly. Default is `TRUE`.

- save_to_disc:

  Logical indicating if the resulting prm file should be overwritten
  (`TRUE`) or not (`FALSE`). Defaults to `TRUE`.

## Value

parameterfile \*.prm file with the new parameter values.

## Examples

``` r
d <- system.file("extdata", "setas-model-new-trunk", package = "atlantistools")

new_prm <- change_prm_cohort(prm_biol = file.path(d, "VMPA_setas_biol_fishing_Trunk.prm"),
                             select_acronyms = c("FPS", "FVS"),
                             roc = matrix(rep(2, times = 20), nrow = 2, ncol = 10),
                             parameter = "C",
                             save_to_disc = FALSE)
# C_FPS is in line 640. Old values are 0.0002 0.3 0.6 0.6 0.6 0.6 0.5 0.5 0.4 and 0.4.
new_prm[640:641]
#> [1] "C_FPS            10"                           
#> [2] "4e-04\t0.6\t1.2\t1.2\t1.2\t1.2\t1\t1\t0.8\t0.8"
# C_FVS is in line 652. Old values are 40.0 40.0 40.0 120.0 150.0 250.0 250.0 300.0 300.0 and 300.0.
new_prm[652:653]
#> [1] "C_FVS            10"                          
#> [2] "80\t80\t80\t240\t300\t500\t500\t600\t600\t600"

# Also works for lists as argument
new_prm <- change_prm_cohort(prm_biol = file.path(d, "VMPA_setas_biol_fishing_Trunk.prm"),
                             select_acronyms = c("FPS", "FVS"),
                             roc = list(rep(3, times = 10), rep(2, times = 10)),
                             parameter = "C",
                             save_to_disc = FALSE)
```
