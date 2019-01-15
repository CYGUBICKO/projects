# WDBC-Codes
### Hooks for the editor to set the default target

current: target
-include target.mk

##################################################################

## Defs

# stuff

Sources += Makefile .ignore .gitignore

msrepo = https://github.com/dushoff
ms = makestuff
Ignore += local.mk
-include local.mk
-include $(ms)/os.mk

# -include $(ms)/perl.def

Ignore += $(ms)
## Sources += $(ms)
Makefile: $(ms) $(ms)/Makefile
$(ms):
	git clone $(msrepo)/$(ms)

######################################################################

## Loading data and defining some important functions

# Define all important R-functions in one file
globalFunctions.Rout: globalFunctions.R
outputNames.Rout: outputNames.R

# Read data
loadData.Rout: loadData.R

# Generate a codebook
codebook.Rout: outputNames.Rout globalFunctions.Rout loadData.Rout codebook.R
hh_codebook.Rout: codebook.Rout hh_codebook.R
hh_codebook.xlsx: hh_codebook.Rout ;

# Working dataset:
# *Stata dataset is loaded as values instead of labels
workingDF.Rout: codebook.Rout workingDF.R

## Proportion of missingness 
missingProp.Rout: workingDF.Rout missingProp.R
hh_miss_prop_summary.xlsx: missingProp.Rout ;
missPlot.Rout: missingProp.Rout missPlot.R

## Data Exploration
# 1. ID variables
idVars.Rout: workingDF.Rout idVars.R

# background Information
backgroundSummary.Rout: workingDF.Rout backgroundSummary.R


## Report
hh_data_cleaning_report.html: hh_data_cleaning_report.Rmd

######################################################################

clean: 
	rm *Rout.*  *.Rout .*.RData .*.Rout.* .*.wrapR.* .*.Rlog *.RData *.wrapR.* *.Rlog

######################################################################


### Makestuff

-include $(ms)/pandoc.mk
-include $(ms)/git.mk
-include $(ms)/visual.mk
-include $(ms)/wrapR.mk
