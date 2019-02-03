# Convert some variable labels to variables
# The function picks labels from the codebook file
# Depends on library(dplyr)
# vars_vec: Variables to extract their labels

vartoLabs <- function(vars_vec = vars_vec){
	%>% filter(variable %in% vars_vec)
	%>% mutate(description = gsub(".*: ", "", description)
   		, description = gsub(" |/", "_", description)
		)
	%>% pull(description)
}

