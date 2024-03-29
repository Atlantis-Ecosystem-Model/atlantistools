
# atlantistools 0.5.0

## New features

* New `get` function to extract information from `functionalGroups.csv`
  * `get_cohorts_acronyms`. Extract Codes of species with specified number of cohorts
  * `get_fished_acronyms`. Extract Codes of species for which IsFished = 1
  * `get_turnedon_acronyms`. Extract Codes of species for which IsTurneOn = 1
* `load_mort` Reads in output from Mort.txt file
* `load_spec_mort`. Reads in mortality output from SpecificMort.txt

## Minor improvements

* `load_spec_pred_mort`renamed from `load_spec_mort`. Reads in mortality output from SpecificPredMort.txt
* `preprocess_txt`. Added argument `removeZeros = T`. User can select not to remove zeros from output
* `plot_line`. Added argument, `ylim` to specify min and max values 

## Bug fixes
* `load-init` Variables that have data at box level only (no layer data) can now be read from the nc file. 


# atlantistools 0.4.3
## Bug fixes
* Minor bugfixes associated with R CMD check NOTE.
    * Reduced file size of `inst\extdata\setas-model-new-trunk`.

## New features
* `get_maturity_fishbase` Extract maturity parameters (length and age based) from fishbase.
* `scan_reference_fishbase` Scan reference list from fishbase for a particular species for 
a species character string, e.g. "diet" or "feeding".

# atlantistools 0.4.2

## Bug fixes
* Minor bugfixes associated with R CMD check NOTE.
    * Reduced file size of pre-build vignettes with `tools::compactPDF(gs_quality = 'ebook')`.
    * Moved `rfishbase` to `Suggests` in DESCRIPTION.
    * Started to add visual tests using `vdiffr`.

# atlantistools 0.4.1
## New features
* `get_growth_fishbase` Extract Van Bertalannfy growth parameters (k, linf, t0) from http:://fishbase.se.
You need to pass a vector of fish names consisiting of both species and genus. In addition you can
modify the optional mirror argument to speed up data extraction. The argument currently defaults
to ".se".
* `get_ids_fishbase` In order to extract data from fishbase species specific URLs need to be generated.
This function is used to extract the fishbase reference id for any amount of fish species. The id
is extracted from `rfishbase::fishbase`.
* `get_ref_fishbase` Extract bibliographic information for Van Bertalannfy growth parameters from 
fishbase.

## Minor improvements
* Remove column `Updated` which is generated in new trunk models in `load_dietcheck` (Thanks to Sieme Bossier).

## Bug fixes
* Added NA handling to some functions (Thanks to Javier Porobic).
* Prevent `prm_to_df` to no longer overwrite existing columns (Thanks to Javier Porobic).
* Switch column names `pred` and `prey` in `load_spec_mort` for trunk models.

# atlantistools 0.4.0.9000
## New features
Removed the `dir` parameter from all functions. This will provide the most flexibility in setting your
Atlantis simulations. You can now use any kind of filestructure. The easiest way to use `atlantistools` 
is to define the Atlantis folder as working directory in your active R session. In addition you should try
to work with a R project. By doing so you can simply pass the filenames (no directory needed anymore) as
parameters. In addition files in subdirectories are auto-completed in the R project (given you have the newest
RStudio version).

Unfortunately, this might break existing code but we will all benefit from this greatly in the future.

# atlantistools 0.3.3.9000
## Development manifesto
The internal structure of atlantistools was updadted considerably. Unfortunately, this might break existing code.
Please check out the updated versions of the vignettes `model-calibration`,  `model-calibration-species` and
`model-comparison` to get an idea on how to update your existing code.

Following are the main changes to the previous version:

1. Examples and tests are now based on the SETAS-trunk model. The package still works with bec_dev models,
however the focus of development is shifted towards the trunk code base.

2. `preprocss()` was replaced with `model-preprocess.Rmd`. Modelers can now build their own preprocessing
routine using the built in `model-preprocess` vignette. This does not impair the functionality of the package in any
way but further improves the modulate structure of atlantisttols. Modelers can decide for themselves
which routines they want to include in their personalised preprocessing application. In addition 
contributing to atlantistools is much easier now due to the removed technical overhead within the old `preprocess` function.

3. Functions within atlantistools follow a specific hierarchy. The lowest level of hierarchy is 
comprised of the `load_` functions. These functions form the main link between the Atlantis simulation
and the R environment and ensure data is transformed according to the tidy dataframe framework 
by Hadley Wickham (Tidy Data - Available at: http://vita.had.co.nz/papers/tidy-data.pdf).
Based on the `load_` functions various `calculate_` functions can be applied to calculate model specific metrics
like spatial biomass or consumed biomass. The last layer is formed by the `plot_` functions. These functions can be applied
to dataframes generated with `load_` functions directly or via the `calculate_` functions. Please feel free to
contribute to any of those functions to further improve the functionality of atlantistools.

4. The plotting modules within atlantistools were overhauled.
Instead of dataframe specific plotting routines two main plot functions, naimly `plot_line` and `plot_bar`
are introduced. Most plots within atlantistools are based on the ggplot2 architecture. Thus, it is very easy
to modulate the plots to your personal likings. To further specialise the generalised main plotting functions low-level functions 
like `plot_add_box` and `plot_add_range` can be added to customise the visualisations.

## New features
* `load_init_stanza` Read in nitrogen values from your initial conditions file for stanza based groups.
* `plot_line` Visualise data as line chart.
* `plot_bar` Visualise data as bar chart.
* `plot_add_box` Add boxes around relative timeseries plots to indicate if data is within an acceptable range.
* `plot_add_range` Add external data as rug-plot to the left and right y-axis to check if data from 
an Atlantis simulation is within the range of obeserved data.

## Documentation improvements
* Added various reference dataframes from the SETAS-trunk model. See `data.R` for more information.

# atlantistools 0.3.2.9000
## New features
* `calculate_biomass_spatial` Calculate the total biomass in tonnes for every group and ageclass combination
per time, polygon and layer. Data is read in from 'output[...].nc' using the `load_nc` function. 
Biomass for age based groups is calculated as (StructN [mgN] + ResN [mgN]) * Numbers [individuals]. 
Biomass for non age based groups is calculated as N [mgN] * volume [m^3] (sediment-dz [m] / volume [m^3] for epibenthic groups).
mgN is converted to tonnes based on the stettings in the biol.prm file. 
Simulation time steps are converted to time in years based on the output timesteps given in run.prm. Group names are unified to
 match the column 'LongName' in the functionalGroups.csv. All these transformations are applied in every
`calculate` or `load` function to guarantee the same structure in all dataframes.

* `calculate_consumed_biomass` Calculate the consumed biomass in tonnes of every preygroup
by a predator/predatorageclass combination for each timestep and polygon. Consumption data is extracted from output[...]PROD.nc. Age based groups are stored as "Eat_" non age based groups as "Grazing_". Units are consumed mass in mg N m^-3 d^-1.
The consumed biomass is scaled by the boxvolume and transformed to tonnes.
Diet contribution data is extracted from DietCheck.txt. Currently this only works
for models based on the trunk code. Units are % diet contribution. To calculate the consumed biomass for each preytype
the total consumed biomass (per predator/ageclass/time/polygon) is multiplied with the dietcontribution
of that prey to the predator.

* `calculate_spatial_overlap` The spatial overlap between predator and prey groups is calculated based on the concept of
Schoener's similarity index ranging from 1 (perfect overlap) to 0 (zero overlap). 
The function returns 2 dataframes. Firstly the Schoener index per predator/ageclass/prey combination.
Secondly the index is calculated as a weighted mean per predator/predatorageclass with the prey availabilities 
as weights. The second version can be used to test the overlap of the predator with an emphasis on the most important
prey groups. In the current version all potential polygons and layers (islands/boundary boxes and non-existent 
layers are excluded) are used to calculate the spatial overlap even if a predator is never present in a specific polygon and layer. This may result in very low overlap
indices in case your species have a narrow spatial distribution.

* `check_growth` helps to track changes in group- and age specific individual weight over time.
You can either track the maximum relative change in individual weight over the whole simulation
period or the relative change within each year in case you have multiple outputs per year. 
This will allow you to easily track unrealistically high changes in individual weight throughout the simulation.

* `plot_diet` I updated the way `plot_diet` works. The new version of `plot_diet` only works with trunk models. Please use `plot_diet_bec_dev` for models based on the bec_dev code version. `plot_diet()` now displays the realised diet contribution (both per predator and
prey perspective) based an the amount of consumed biomas. 

* Please note, `plot_diet_bec_dev` does not work with DietCheck.txt anymore. Please use SpecPredMort.txt instead. It should be noted that the resulting plot only gives an indication of the 
realisd diets with bec_dev models.

* `sc_init` sanity check for your initial conditions file. Given the prey densities, C and MUM values 
in your initial conditions file the required growth per group and ageclass (which is the growth needed to 
grow from ageclass i to ageclass i+1) is compared with the realised growth given your initial conditions file.

* `plot_sc_init` This function is tightly linked to `sc_init`. Based on the calculations in `sc_init` you can pass
multiplication factors for your initial MUM and C values to visualise the effect on the relative growth 
(realised growth/required growth). This visualisation can be used to give you an indication of the MUM/C/Avail
parameterspace prior to your simulation. Please keep in mind that growth is highly simplified and
may not represent the actual growth in your soimulation due to the absence of major ecological processes
(e.g. rugosity, reproductioncosts, refuge, ...).

* `plot_consumed_biomass` In addition to `diet_plot` you can use this function to visualise 
the group specific biomass flow for the whole system at any timestep of your simulation with
a circle-flow-diagram (These things just look awesome). Please note, this function is still
in early development.

* `plot_spatial_overlap` Visualise the Schoener index calculated with `calculate_spatial_overlap`.
Please note, this function is still in early development.

* `combine_ages` Transform an ageclass based dataset to a stanza based dataset using the age_mat
parameters in your biol.prm file.

* `prm_to_df` Extract parameters from your biological parameter file (e.g. MUM, C, KWRR, KWSR, age_mat ...)
and convert the output to a tidy dataframe. Please note, there are two versions of this function available.
Like the `extract_prm` functions one function is used to convert non ageclass specific parameters (these
are parameters which have 1 value per parameter. Value and parameter flag are stored in the same line the prm file) 
`prm_to_df` and ageclass specific ones (these parameters
are ususally stored in the row following the flag in the prm file and multiple values are stored per parameter. E.g. MUM,
C, VERTday) `prm_to_df_ages`. This can be very helpful to either add additional information to any kind of
tidy dataframe in atlantistools. For example adding age at maturity to an ageclass based dataframe.
In addition this might be helpful to extract parameter values from any model and distribute them among collegues
in a unified format (E.g. to compare parameters).

* `load_init` Read in information from your initial conditions file (e.g. StructN, ResN, Nums, N). Based on the
different format of the variables there are 3 functions available currently:
1. `load_init_age` to read in age-specific data like ResN, StructN, Nums.
2. `load_init_nonage` to read in N.
3. `load_init_physics` to read in physical parameters like Salt, Volume, Temp, NH3 and so on.

# atlantistools 0.3.1.9000
## New features
* `plot_species()` is based on the species-specific overview plots used at CSIRO and Ifremer (Raphael).
For each age-structured group (10 ageclasses) the biomass, individual weight, numbers and condition 
are shown per age for the model simulation.

## Minor improvements
* `combine_groups()` can be used to combine different grouping variables within a plot to a `Rest-group`.
Assume you want to visualise the feeding interactions of species x. The prey groups are usually
mapped to colors in `atlantistool`. For some groups there might be high number of prey organisms (e.g. 20+).
Therefore color-coding becomes as hassle. You can use the `combine_groups()` function to gather all
groups with a low contribution to the overall value to a `Rest` group to minimise the number of
grouping variables resulting in fewer colors in the final plot. The function is build into the `plot_diet()`
function but can be used in other occasions as well.
* `convert_time()` now converts timesteps/days to years. You can still convert to time in date-format.
In this case set `as_date` to `TRUE` and pass the `modelstart` as usual. By default `as_date` is `FALSE`
in the new version of the package. You might need to adjust this in your plotting routines.

# atlantistools 0.3.0.9000
## New features
* `plot_diet()` This functions replaces `plot_dietcheck()`. The resulting plot will give an indication
about feeding interactions for each functional group. The upper panel shows the feeding interaction
from the predator perspective while the lower panel focuses on the prey perspective. You can either 
pass data from the DietCheck.txt or the SpecMort.txt file. In the first case the plots are faceted according
to the habitat while in the latter case plots are either faceted by ageclass or stanza. 
* `combine_ages()` can be used to convert ageclass to stanzas (juvenile/adult). Currently this only works
for groups with exactly 10 ageclasses. The stanzas are split according to the age_mat parameter in
the biological parameterfile. You can apply this function to convert the ageclass based specific mortality
data to stanzas in order to simplify your plotting routines/anaylsis of the feeding interactions.
* `load_spec_mort()` allows the extraction of predation mortalities from the SpecMort.txt file. The data is
stored in the overall summary list generated with `preprocess()`. In order to guarantee the highest amount
of flexibility in further analysis the data is not aggregated in any way. You may choose to aggregate
ageclasses to stanzas with `combine_ages()` if you please. The addition of this file might blow up
the resulting .rda file but the filesize should still be easily manageable (1GB .nc and 100MB SpecMort.txt 
are stored as 30MB .rda).

# atlantistools 0.2.0.9000

## New features
* `combine_runs()` can be used to combine preprocessed data created with `preprocess()` from different 
Atlantis simulations. The model output can be compared with the vignette code `model-comparison.Rmd`.
* The vignette `model-calibration.Rmd` can be used to create various model calibration and tuning plots
for a single simulation. The vignette can be used as blueprint for different Atlantis models. Simply
download the vignette code, change the first section where the data is read in to your likings,
modify the plot dimensions and you are good to go.
* Added `model-comparison.Rmd` to compare multiple Atlantis simulations. It works very similar to
`model-calibration.Rmd`.

## Minor improvements
* `load_dietcheck()` and `load_rec()` are incorporated into `preprocess()`. The resulting dataframe are now
part of the output and can be accessed directly from the list via `out$diet` and `out$ssb_rec`.
`convert_factor()` was overhauled. Due to the inclusion of `load_dietcheck()` there
is no more `Rest` group when the name conversion is performed.
* `convert_time()` can now handle different input formats without any user specifications.
* Added vignette `model-calibration` to automatically build a pdf document with various plotting routines
used for model tuning and calibration.

# atlantistools 0.1.0.9000

## New features

* `plot_bench()` 
* `plot_boxes()` 
* `plot_calibrate()` 
* `plot_dietcheck()` 
* `plot_flux()` 
* `plot_physics()` 
* `plot_rec()` 
* `plot_struct()` 
* `plot_ts()` 

# atlantistools 0.0.0.9000

## New features

* `load_fgs()` Read in the functional group file as dataframe.
* `load_bps()` Extracts the names of the epibenthic biomasspools from the initial conditions file.
* `load_box()` Load the box specification file (*.bgm) for an Atlantis scenario.
* `load_nc()` Read in data from Atlantis netcdf output files. Currently the general and the 
production outputfile are supported. In addition the following variables can be read in:
"N", "Nums", "ResN", "StructN", "Eat", "Growth", "Prodn" and "Grazing". Please only pass one
variable per function call. 
* `load_nc_physics()` Read in physical data from Atlantis netcdf output file. Works very
similar like load_nc.
* `load_dietcheck()` Read in data from the `DietCheck.txt` output file. Currently only models based on the
`bec_dev` branch are supported due to differences in data structure with `trunc` models.
* `load_rec()` Read in data from the `*SSB.txt` and `*YOY.txt` output files. 
* `plot_calibrate()` Plot relative timeseries of various variables (E.g. biomass@age, 
weight@age). The start of the model run is used as reference point.
* `preprocess()` Various model specifications are read in from the ncdf-output files
and aggregated with different levels of complexity. (E.g. biomass per species, time and ageclass
or biomass per species, time and box). The data can be stored in a *.rda file to save
storage on harddisc. The function returns a list of dataframes which all have the same structure.
This function should be executed before you start plotting as most of the plotting routines 
pull out the data from this file/object.
* `agg_mean()` Aggregate data using dplyr functionality. This is basically a wrapper
for 'group_by' followed by 'summarise'.
* `agg_sum()` See 'agg_mean' for further details. The summary function used her is 'sum'.

## Minor improvements

* `convert_path()` Allows users to use the model with either one model folder or multiple
model folders (e.g. one main model folder and an extra folder for model output). I highly 
recommend to store all the data in one folder because then you only have to supply the
directory once and can acces all files using only the filename. If you are using multiple
files you always have to pass the complete folder/filename-string as filename and set
the 'dir' paramater to NULL.
* `convert_bgm()` Convert `*.bgm` file to a dataframe using a spatial projection.
* `convert_factor()` Convert species names in output files to long names. Therefore the plots all
use the same labeling. Simply change the values in your `functionalGroups.csv` file in the column `LongName` 
if you want to change the names.
* `convert_time()` Convert from timestep (either as integer or number of days) to actual time.
* `get_boundary()` Extract the boundary boxes from the list of boxinfos generated with 'load_box()'. 
* `get_conv_mgnbiot()` Extract the conversionfactor to convert data from mg nitrogen to biomass in t
from the biol*.prm file.
* `get_groups()` Collection of similar functions to extract specific
#' columns from the Atlantis functionalGroups.csv file. (E.g. cohort-groups, acronyms, group names ...)
* `theme_atlantis` Customised ggplot2 theme used in every plot.

# Dummy x.x.x.9000
## New features
## Documentation improvements
## Minor improvements
## Bug fixes

