### Hooks for the editor to set the default target

## https:cygubicko.github.io/projects

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
Makefile: $(ms)/Makefile
	touch $@
$(ms)/Makefile:
	ls ../makestuff/Makefile && /bin/ln -s ../makestuff 

######################################################################

## Loading data and defining some important functions
## ln -s ~/Dropbox/aphrc/wash/data 

Ignore += data docs

Sources += $(wildcard *.R *.Rmd)

# Define all important R-functions in one file
globalFunctions.Rout: globalFunctions.R

# Read data
loadData.Rout: data/NUHDSS_Wash.dta loadData.R

# Some cleaning
cleaning.Rout: cleaning.R

# Drop cases
complete.Rout: complete.R
# wash_codebook.csv: complete.Rout ;

# Logistic PCA to create a single variable for WASH variables
logisticpca.Rout: logisticpca.R

# Descriptives
descriptivePlots.Rout: descriptivePlots.R
descriptives.Rout: descriptives.R

# Analysis
## 1. Generalized Linear Mixed-effects Model (GLMM)
glmmModel.Rout: glmmModel.R

# Unsupervised learning (Clustering)

## Report
wash_report.html: wash_report.Rmd

######################################################################

clean: 
	rm -f *Rout.*  *.Rout .*.RData .*.Rout.* .*.wrapR.* .*.Rlog *.RData *.wrapR.* *.Rlog *.rdeps *.rda .*.rdeps .*.rda

######################################################################

### Makestuff

-include $(ms)/pandoc.mk
-include $(ms)/git.mk
-include $(ms)/visual.mk
-include $(ms)/stepR.mk
