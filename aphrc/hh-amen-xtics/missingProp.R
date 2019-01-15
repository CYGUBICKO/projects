library(dplyr)
library(tibble)
library(openxlsx)

#### ---- Missingness -----

## Proportion per variable merged with codebook
miss_prop_df <- (working_df 
	%>% missPropFunc()
  	%>% left_join(codebook, by = "variable")
  	%>% select(variable, description, miss_count, miss_prop)
  	%>% arrange(desc(miss_prop))
)

# Save the output
target_name <- paste0(file_prefix, "miss_prop_summary", ".xlsx")
saveXlsx(miss_prop_df, target_name)

