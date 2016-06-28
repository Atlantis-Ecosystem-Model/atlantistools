# Version 1.0.0 from 2016/06/28

# The output files were created using the following settings.
# The model files were checked out via SVN at revision 6042 from
# https://svnserv.csiro.au/svn/ext/atlantis/runFiles/trunk/SETas_model_New_Trunk/ (external users)
# https://svnserv.csiro.au/svn/atlantis/runFiles/trunk/SETas_model_New_Trunk/ (csiro)
# See https://confluence.csiro.au/display/Atlantis/Sample+input+files for further information.
# 'atlantismain.exe' was build using the 'trunk' sourcecode checked out at revision 6042
# with "atlantis_VS2010.sln".

# The following files are used in the model run!
INIT_VMPA_Jan2015.nc
VMPA_setas_run_fishing_F_Trunk.prm
VMPA_setas_force_fish_Trunk.prm
VMPA_setas_physics.prm
VMPA_setas_biol_fishing_Trunk.prm
VMPA_setas_harvest_F_Trunk.prm
SETasGroupsDem_NoCep.csv
SETasFisheries.csv

# Settings in "VMPA_setas_run_fishing_F_Trunk.prm"
flagdietcheck 1        # Periodically list realised diet matchups (tuning diagnostic)
dt         	12 hour     # 12 hour time step
tstop       1095 day     # Stop time after the given period 15000 5000
toutstart  	0 day       # Output start time
toutinc    	365 day     # Write output with this periodicity
toutfinc   	365 day     # Write fisheries output with this periodicity
tsumout    	30 day      # Write stock state summary with this periodicity

# Settings in "SETasGroupsDem_NoCep.csv"
# The following groups are turned on during the model run.
# Note: Not all groups are turned on to minimize size of output files.
# Note: Column "Long Name" is renamed to "LongName".
Code Index	IsTurnedOn	Name	        LongName	                NumCohorts	MovesVertically	MovesHorizontally	isFished	IsImpacted	isTAC	InvertType	isPredator	IsCover	isSiliconDep	isAssessed	IsCatchGrazer	isOverWinter
FPS	 2	    1	        Planktiv_S_Fish	Small planktivorous fish	10	1	2	1	1	1	1	1	1	1	1	FISH	        1	0	0	1	0	0	0	0
FVS	 5	    1	        Pisciv_S_Fish	Shallow piscivorous fish	10	1	2	1	1	1	1	1	1	1	1	FISH	        1	0	0	1	0	0	0	0
CEP	 35	    1	        Cephalopod	    Cephalopod	                2	1	2	1	1	1	1	1	1	1	1	CEP	            1	0	0	1	0	0	0	0
BML	 41	    1	        Megazoobenthos	Megazoobenthos	            1	1	1	1	1	1	1	1	1	1	1	MOB_EP_OTHER	1	0	0	1	0	0	0	0
PL	 51	    1	        Diatom	        Diatom	                    1	1	1	1	1	1	0	0	0	0	0	LG_PHY	        0	0	1	1	0	0	0	0
ZM	 54	    1	        Zoo	            Mesozooplankton	            1	1	1	1	1	1	1	0	0	0	0	MED_ZOO	        1	0	0	1	0	0	0	0
DL	 59	    1	        Lab_Det	        Labile detritus	            1	1	1	1	1	1	0	0	0	0	0	LAB_DET	        0	0	0	1	0	0	0	0
DR	 60	    1	        Ref_Det	        Refractory detritus	        1	1	1	1	1	1	0	0	0	0	0	REF_DET	        0	0	0	1	0	0	0	0

# The following flags/parameters had to be added to "VMPA_setas_biol_fishing_Trunk.prm"
# to prevent the model from crashing.
# There is no documentation available on the wiki.
flag_shrinkfat 0
flag_predratiodepend 0
ht_FPS 0
ht_FVS 0

# Previously the size of inverts was assumed to be 1.0 when calculating selectivity.
# The updated code reads in two new values from the biology.prm file.
# These values are used for all inverts much the same way the li_a and li_b values are set for all vertebrates.
li_a_invert 0.01
li_b_invert 3.0

# The following output files are used:
outputSETAS.nc
outputSETASPROD.nc
outputSETASDietCheck.txt
outputSETASSSB.txt
outputSETASYOY.txt
