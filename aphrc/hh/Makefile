# hh-amen-xtics
### Hooks for the editor to set the default target

current: target
-include target.mk

##################################################################

## Defs

# stuff

Sources += Makefile

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
## ln -s ~/Dropbox/aphrc/hh_amen_xtics/data/ data ##
## ln -s ~/Dropbox/aphrc/hh_amen_xtics/docs/ docs ##
Ignore += data docs

Sources += $(wildcard *.R *.Rmd)

# Define all important R-functions in one file
globalFunctions.Rout: globalFunctions.R
outputNames.Rout: outputNames.R

# Read data
loadData.Rout: globalFunctions.Rout loadData.R

# Save .xlsx copy of codebook
hh_codebook.Rout: loadData.Rout hh_codebook.R
hh_codebook.xlsx: hh_codebook.Rout ;

# Working dataset:
# Drop variables with 100% missingness
workingDF.Rout: outputNames.Rout loadData.Rout workingDF.R

## Proportion of missingness 
#missingProp.Rout: workingDF.Rout missingProp.R
Ignore += *.xlsx
hh_miss_prop_summary.xlsx: workingDF.Rout ;
missPlot.Rout: missingProp.Rout missPlot.R

## Data Exploration
# 1. ID variables
idVars.Rout: workingDF.Rout idVars.R

# background Information
backgroundSummary.Rout: idVars.Rout backgroundSummary.R

# Household ammenities
hhamenitiesFunc.Rout: backgroundSummary.Rout hhamenitiesFunc.R
hhamenitiesSummary.Rout: hhamenitiesFunc.Rout hhamenitiesSummary.R
#hhamenitiesObjc.Rout: hhamenitiesSummary.Rout hhamenitiesObjc.R

# Objects to report. This should be last script
objectsReport.Rout: hhamenitiesSummary.Rout objectsReport.R 

## Report
hh_data_cleaning_report.html: objectsReport.Rout hh_data_cleaning_report.Rmd

######################################################################

clean: 
	rm -f *Rout.*  *.Rout .*.RData .*.Rout.* .*.wrapR.* .*.Rlog *.RData *.wrapR.* *.Rlog

######################################################################

### Makestuff

-include $(ms)/pandoc.mk
-include $(ms)/git.mk
-include $(ms)/visual.mk
-include $(ms)/wrapR.mk
