# Add variable labels to tables or graphs
# The function depends on:
## library(expss)
## library(dplyr)

addVarlabs <- function(df = df
	, cleanlabs_df = cleanlabs_df
	, variables = variables){
	for (var in variables){
		temp_df <- (cleanlabs_df
			%>% filter(variable %in% var)
		)
		temp_lab <- pull(temp_df, description)
		var_lab(df[, var]) <- temp_lab
	}
}
