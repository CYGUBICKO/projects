#### ---- Define some important functions ----

## Extract codebook

ColAttr <- function(x, attrC, ifIsNull) {
	# Returns column attribute named in attrC, if present, else isNullC.
  	atr <- attr(x, attrC, exact = TRUE)
  	atr <- if (is.null(atr)) {ifIsNull} else {atr}
  	atr
}

AtribLst <- function(df, attrC, isNullC){
  	# Returns list of values of the col attribute attrC, if present, else isNullC
	lapply(df, ColAttr, attrC=attrC, ifIsNull=isNullC)
}

## Missingness function

missPropFunc <- function(df){
	n <- nrow(df)
  	df <- as.data.frame(df)
	miss_count <- apply(df, 2
		, function(x) sum(is.na(x)|x == ""|grepl("refuse|NIU|missi", x, ignore.case = TRUE)))
  	miss_df <- (miss_count
   	%>% as_tibble(rownames = NA)
    	%>% rownames_to_column("variable")
    	%>% rename(miss_count = value)
    	%>% mutate(miss_prop = miss_count/n)
    	%>% mutate(miss_prop = round(miss_prop, digits = 3) * 100)
	)
}

## Save some excel output

saveXlsx <- function(object, target_name){
	write.xlsx(object
		, target_name
	)
}

## Add variable labels to dataframe outputs

varLabs <- function(tab){
	name <- colnames(tab)[!colnames(tab) %in% "variable"]
  	lab <- (tab
   	%>% mutate(description = codebook$description[codebook$variable %in% variable])
    	%>% select(variable, description, name)
	)
}

## Add variable labels to crosstabs

extractLabs <- function(variable){
	as.character(codebook$description[codebook$variable %in% variable])
}

## Data summary functions

# Crosstab function
propFunc <- function(df = data.frame(), dots = NULL, ...){
	prop_df <- (df
		%>% group_by_(.dots = dots)
		%>% summarise_(n = ~n())
		%>% mutate(prop = prop.table(n))
	)
	prop_df
}

# Generate row, column and counts from multiple variables (at least 2)
tabsFunc <- function(df = data.frame(), vars = NULL, no_digits = 2, ...){
	# The first oject in vars is the key column folowed by next ... such 
	# that var1>var1>var1>......
	# The last object on vars will be used as a binding var, i.e, on the raw.

	# Counts
	count_df <- (df
		%>% propFunc(vars)
		%>% select(-prop) 
		%>% spread(last(vars), n)
	)

	# Row percentages
	rowperc_df <- (df
		%>% propFunc(vars)
		%>% mutate(Percent = round(prop*100, no_digits))
		%>% select(-n, -prop) 
		%>% spread(last(vars), Percent)
	)

	# Column percentages
	levs <- levels(factor(df[[last(vars)]]))
		all_vars <- c(first(vars), levs)
		colperc_df <- count_df
		colperc_df[, all_vars] <- (colperc_df[, all_vars]
		%>% group_by_(first(vars))
		%>% mutate_at(levs, function(i){round((i/sum(i)) * 100, no_digits)})
	)

	# Save long format of column percentages for ploting
	colperc_long_df <- (rowperc_df
		%>% gather("var", "Percent", levs)
		%>% rename_(.dots = setNames("var", last(vars)))
	)

	out <- list(count_df = count_df
		, rowperc_df = rowperc_df
		, colperc_df = colperc_df
		, colperc_long_df = colperc_long_df
		, levs = levs
	)
	return(out)
}

### Format tabs and print 'nice' html tables
#formatTabs <- function(tabin, unit = "%", levs, no_digits = 2, col_label = NULL, cap = NULL){
#	# levs object is from the tab function
#	if (!unit %in% c("%", "n")){
#	stop("Unit is either % or n")
#	}
#	ztab <- (tabin
#		%>% as.data.frame()
#		%>% ztable(digits = no_digits
#			, caption = cap
#		)
#	)
#	if (unit %in% "%"){
#		ztab <- (ztab
#			%>% addcgroup(cgroup = c('', paste0(col_label, " (%)"))
#				, n.cgroup = c(length(levs), length(levs))
#			)
#		)
#	}
#	if (unit %in% "n"){
#		ztab <- (ztab
#			%>% addcgroup(cgroup = c('', paste0(col_label, " (n)"))
#			, n.cgroup = c(length(levs), length(levs))
#			)
#		)
#	}
#	ztable2flextable(ztab)
#}
#

# Recode the categories to fewer ones

recodeLabs <- function(df, var, pattern, replacement, insert = TRUE){
	if (insert){
		new_var <- paste0(var, "_new")
		df[, new_var] <- df[, var]
		for (p in 1:length(patterns)){
			df[, new_var] <- apply(df[, new_var], 2
			, function(x){ifelse(grepl(patterns[[p]], x), replacements[[p]], x)})
		}
	} else{
		for (p in 1:length(patterns)){
			df[, var] <- apply(df[, var], 2
			  , function(x){ifelse(grepl(patterns[[p]], x), replacements[[p]], x)})
		}
	}
	return(df)
}

## Extract issues function
extractIssues <- function(df, var, pattern, tab_vars){
	vals <- pull(df, var)
	issues <- grep(pattern, vals, value = TRUE, ignore.case = TRUE)
	issues_df <- (df
		%>% filter(.data[[var]] %in% issues)
	)
	issues_tab <- (issues_df
		%>% tabsFunc(vars = tab_vars)
	)
	return(
		list(issues_df = issues_df, issues_tab = issues_tab)
	)
}
