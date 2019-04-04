### Hooks for the editor to set the default target

## https:cygubicko.github.io/projects

current: target
-include target.mk

##################################################################

## Defs

# stuff

Sources += Makefile notes.md
Ignore += ../autopsy/*.Rlog

msrepo = https://github.com/dushoff
ms = makestuff
-include $(ms)/os.mk

## Used by Steve to link data to right place
Ignore += local.mk
-include local.mk

# -include $(ms)/perl.def

Ignore += $(ms)
## Sources += $(ms)
Makefile: $(ms)/Makefile
	touch $@
$(ms)/Makefile:
	ls ../makestuff/Makefile && /bin/ln -s ../makestuff 

######################################################################

## Loading data and defining some important functions
## ln -fs ~/Dropbox/aphrc/wash/data 

Ignore += data docs temp_files 

Sources += $(wildcard *.R *.rmd) $(.../funs/*.R)

# Define all important R-functions in one file
globalFunctions.Rout: globalFunctions.R

# Read data
loadData.Rout: data/NUHDSS_Wash.dta loadData.R

# Some cleaning
cleaning.Rout: cleaning.R

# Drop cases
complete.Rout: complete.R
# wash_codebook.csv: complete.Rout ;

# Select only variables for analysis
analysisdata.Rout: analysisdata.R

# Logistic PCA to create a single variable for WASH variables
logisticpca.Rout: logisticpca.R

# Descriptives plots
descriptives.Rout: descriptives.R

# households.Rout: households.R

# Simulating Response variable
simulateResponse.Rout: simulateResponse.R
simulatePoisson.Rout: simulatePoisson.R

# Fit simple GLM to recapture beta 
simpleGlm.Rout: simpleGlm.R

# Fit a more complex glmer based on long-format data
complexGlmer.Rout: complexGlmer.R

# Fit poisson GMLER
poissonGlmer.Rout: poissonGlmer.R

# Try MCMCglmm
binaryMcmcglmm.Rout: binaryMcmcglmm.R

# Some experimenting
oneGlmer.Rout: oneGlmer.R
oneMcmcglmm.Rout: oneMcmcglmm.R

# Extract summary stats of complexGlmer
glmerStats.Rout: glmerStats.R
mcmcglmmStats.Rout: mcmcglmmStats.R

# Summary plots
glmerSummary.Rout: glmerSummary.R
poissonSummary.Rout: poissonSummary.R
mcmcglmmSummary.Rout: mcmcglmmSummary.R

## Report
wash_report.html: wash_report.rmd

## Presentation
wash_presentation.pdf: wash_presentation.tex

######################################################################

Ignore += *.rda

clean: 
	rm -f *Rout.*  *.Rout .*.RData .*.Rout.* .*.wrapR.* .*.Rlog *.RData *.wrapR.* *.Rlog *.rdeps *.rda .*.rdeps .*.rda

######################################################################

### Makestuff

-include $(ms)/texdeps.mk
-include $(ms)/pandoc.mk
-include $(ms)/git.mk
-include $(ms)/visual.mk
-include $(ms)/stepR.mk
