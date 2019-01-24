# This script uses missPropFunc to summarize the missingness for every variable. The output is saved as a .xlsx and also passed for reporting.

library(haven)
library(dplyr)
library(tibble)
library(openxlsx)
library(DT)

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
## write.csv(csvname, 

## Formated output
miss_prop_df_html <- datatable(miss_prop_df)

## Drop variables with no data
miss_vars <- (miss_prop_df
  %>% filter(miss_prop==100)
  %>% select(variable)
)

vars_droped <- as.character(miss_vars[["variable"]])
no_vars_droped <- length(vars_droped)

working_df <- (working_df
  %>% select(-c(vars_droped))
)

## Objects to report
# miss_prop_df_html
# no_vars_droped

# rdnosave 
save(file=rdaname
	, working_df
	, miss_prop_df
	, miss_prop_df_html
	, no_vars_droped
)


