library(reshape2)
library(dplyr)

# Extract value labels
df <- hh_df 
val_labels <- AtribLst(df, attrC="labels", isNullC=NA)
val_labels <- val_labels[!is.na(val_labels)]
# Extract varaible labels
var_labels <- AtribLst(df, attrC="label", isNullC="")

# Generate codebook from attributes
codebook <- (var_labels
	%>% melt()
  	%>% rename(description = value, variable = L1)
  	%>% select(variable, description)
  	%>% mutate(description = ifelse(description == ""|is.na(description)
		, as.character(variable)
      , as.character(description)
		)
	)
)

codebook
