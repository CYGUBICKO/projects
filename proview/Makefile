## Target
current: target
-include target.mk

## Makestuff setup
Sources += Makefile notes.md
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

subdirs += ../aphrc
Ignore += ${subdirs}

######################################################################

### Makestuff rules

-include $(ms)/git.mk
-include $(ms)/visual.mk

