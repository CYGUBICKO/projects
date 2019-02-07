### Hooks for the editor to set the default target

## https:cygubicko.github.io/projects

current: target
-include target.mk

##################################################################

## Defs

# stuff

Sources += Makefile notes.md

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
# This is a dependency line with no recipe
# Make will look for an implicit rule with a recipe
loadData.Rout: data/verbalautopsy_2002-2015.dta loadData.R

Ignore += steve.out
Sources += steve.in
steve.out: steve.in
	cat steve.in > steve.out
	cat $< >> $@ ## First dependency
	cat $^ >> $@ ## All dependencies

# Some cleaning
cleaningVA.Rout: cleaningVA.R

# Drop cases: No VA result and VA not performed
completeVA.Rout: completeVA.R
Ignore += va_codebook.csv
# va_codebook.csv: completeVA.Rout ;

# Descriptives

## 1. Demographics
demographicFunc.Rout: demographicFunc.R
demographics.Rout: demographics.R

## 2. General symptoms
multiresFuncs.Rout: multiresFuncs.R
generalsymptoms.Rout: generalsymptoms.R

Ignore += *.rda

## Report
Ignore += autopsy_descriptive_report.html
autopsy_descriptive_report.html: demographics.rda generalsymptoms.rda cleaningVA.rda autopsy_descriptive_report.Rmd

######################################################################

clean: 
	rm -f *Rout.*  *.Rout .*.RData .*.Rout.* .*.wrapR.* .*.Rlog *.RData *.wrapR.* *.Rlog

######################################################################

### Makestuff

-include $(ms)/pandoc.mk
-include $(ms)/git.mk
-include $(ms)/visual.mk
-include $(ms)/stepR.mk
