# Change biological parameterfile to simplify automated ATLANTIS calibrations.

This function is used to help automate the calibration routine for
ATLANTIS models.

## Usage

``` r
change_prm(
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

  Vector of multiplication factors which shall be applied to the old set
  of parameters. Please supply one value per selected group. In case
  relative is FALSE the new absolute values can be passed as roc.

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
prm_biol <- file.path(d, "VMPA_setas_biol_fishing_Trunk.prm")

new_prm <- change_prm(prm_biol,
                      select_acronyms = c("FPS", "FVS"),
                      roc = c(2,3),
                      parameter = "KWRR",
                      save_to_disc = FALSE)
```
