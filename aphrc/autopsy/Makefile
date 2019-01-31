### Hooks for the editor to set the default target

## https:cygubicko.github.io/projects

current: target
-include target.mk

##################################################################

## Defs

# stuff

Sources += Makefile

msrepo = https://github.com/dushoff
ms = ../makestuff
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
Ignore += data docs

Sources += $(wildcard *.R *.Rmd)

# Define all important R-functions in one file
globalFunctions.Rout: globalFunctions.R

# Read data
loadData.Rout: globalFunctions.Rout loadData.R

# Drop cases: No VA result and VA not performed
completeVA.Rout: loadData.Rout completeVA.R

# Under 5 dataset
underfiveVA.Rout: globalFunctions.Rout completeVA.Rout underfiveVA.R

# Save .csv copy of codebook
vaCodebook.Rout: underfiveVA.Rout vaCodebook.R
va_codebook.csv: vaCodebook.Rout ;

# Descrirapptives

## 1. Demographics
demographicFunc.Rout: demographicFunc.R
demographics.Rout: demographicFunc.Rout underfiveVA.Rout demographics.R

## Report


######################################################################

clean: 
	rm -f *Rout.*  *.Rout .*.RData .*.Rout.* .*.wrapR.* .*.Rlog *.RData *.wrapR.* *.Rlog

######################################################################

### Makestuff

-include $(ms)/pandoc.mk
-include $(ms)/git.mk
-include $(ms)/visual.mk
-include $(ms)/wrapR.mk
