# Shorten the variable labels based on some pattern the label string

cleanVarlabs <- function(variables = variables, pattern = pattern){
	cleanvar_df <- (codebook
		%>% filter(variable %in% variables)
   	%>% mutate(description = gsub(pattern, "", description))
	)
}
