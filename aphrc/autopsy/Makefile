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
## ln -s ~/Dropbox/aphrc/verbal_autopsy/data 

Ignore += data docs

Sources += $(wildcard *.R *.Rmd)

# Define all important R-functions in one file
# globalFunctions.Rout: globalFunctions.R

# Read data
loadData.Rout: data/verbalautopsy_2002-2015.dta loadData.R

# Drop cases: No VA result and VA not performed
# completeVA.Rout: completeVA.R
# va_codebook.csv: completeVA.Rout ;

# Under 5 dataset
underfiveVA.Rout: underfiveVA.R

# Save .csv copy of codebook
vaCodebook.Rout: vaCodebook.R

# Descrirapptives

## 1. Demographics
demographicFunc.Rout: demographicFunc.R
demographics.Rout: demographics.R

## Report


######################################################################

clean: 
	rm -f *Rout.*  *.Rout .*.RData .*.Rout.* .*.wrapR.* .*.Rlog *.RData *.wrapR.* *.Rlog

######################################################################

### Makestuff

-include $(ms)/pandoc.mk
-include $(ms)/git.mk
-include $(ms)/visual.mk
-include $(ms)/stepR.mk