# The output files were created using the following settings.
# The files were checked out via SVN at revision 5904!
# 'atlantismain.exe' was build using the 'bec_dev' sourcecode.

# The following files are used in the model run!
init_vmpa_setas_25032013.nc
VMPA_setas_run_fishing_F_New.prm
VMPA_setas_force_fish.prm
VMPA_setas_physics.prm
VMPA_setas_biol_fishing_New.prm
VMPA_setas_harvest_F_New.prm
SETasGroups.csv
SETasFisheries.csv

# Settings in "VMPA_setas_run_fishing_F.prm"
flagdietcheck 1        # Periodically list realised diet matchups (tuning diagnostic)
dt         	12 hour     # 12 hour time step
tstop       1095 day     # Stop time after the given period 15000 5000
toutstart  	0 day       # Output start time
toutinc    	365 day     # Write output with this periodicity
toutfinc   	365 day     # Write fisheries output with this periodicity
tsumout    	30 day      # Write stock state summary with this periodicity

# Settings in "SETasGroups.csv"
# The following groups are turned on during the model run.
# Note: Not all groups are turned on to minimize size of output files.
# Note: Column "Long Name" is renamed to "LongName".
Code	Index	IsTurnedOn	Name	LongName	NumCohorts	MovesVertically	MovesHorizontally	isFished	IsImpacted	isTAC	InvertType	isPredator	IsCover	isSiliconDep	isAssessed	IsCatchGrazer	isOverWinter
FPS	2	1	Planktiv_S_Fish	Small planktivorous fish	10	1	1	1	1	1	FISH	1	0	0	1	0	0
FVS	5	1	Pisciv_S_Fish	Shallow piscivorous fish	10	1	1	1	1	1	FISH	1	0	0	1	0	0
CEP	35	1	Cephalopod	Cephalopod	2	1	1	1	1	1	CEP	1	0	0	1	0	0
BML	41	1	Megazoobenthos	Megazoobenthos	1	1	1	1	1	1	MOB_EP_OTHER	1	0	0	1	0	0
PL	51	1	Diatom	Diatom	1	0	0	0	0	0	LG_PHY	0	0	1	1	0	0
ZM	54	1	Zoo	Mesozooplankton	1	1	0	0	0	0	MED_ZOO	1	0	0	1	0	0
DL	59	1	Lab_Det	Labile detritus	1	0	0	0	0	0	LAB_DET	0	0	0	1	0	0
DR	60	1	Ref_Det	Refractory detritus	1	0	0	0	0	0	REF_DET	0	0	0	1	0	0

