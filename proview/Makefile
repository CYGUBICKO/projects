## Target
current: target
-include target.mk

## Makestuff setup
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
	ls $@


######################################################################

Ignore += proviewinfo/* reproviewinfostudentapplication/* ../aphrc/*

#Ignore += *.aux *.bbl *.blg *.log *.out *.synctex.gz
######################################################################

### Makestuff rules

-include $(ms)/git.mk
-include $(ms)/visual.mk

