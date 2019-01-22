#### ---- These customized table functions. At least they look nice!!! ---

# library(memisc)
# library(dplyr)

groupSummary <- function(df = df
	, grouped_vars = grouped_vars
	, tab_vars = tab_vars
	, patterns = patterns
	, replacements = replacements
	, labels = NULL
	, type = c("wide", "listed")
	){
	tab_list <- list()
  	for(var in 1:length(grouped_vars)){
		# Clean the variables
		df <- recodeLabs(df
    		, grouped_vars[[var]]
    		, patterns
    		, replacements
 		)
		# Update codebook
  		new_var <- paste0(grouped_vars[[var]], "_new")
		issue_labs <- extractLabs(grouped_vars[[var]])
  		codebook_update <- data.frame(variable = new_var
			, description = paste0(issue_labs, "(new)")
		)
		codebook <- rbind(codebook, codebook_update)
		summary_vars <- c(tab_vars, new_var)
		tab_df <- (df
			%>% select(summary_vars)
		)
		if (length(labels) > 0){
			tab_labels <- c(
				labels[(length(grouped_vars) + 1):length(labels)]
				, labels[[var]]
			)
    		tab_df <- (tab_df
				%>% rename_(.dots = setNames(summary_vars, tab_labels))
    		)
		}
		tab <- (tab_df
			%>% table()
			%>% as.data.frame()
			%>% filter(Freq>0)
		)
    	tab <- xtabs(Freq ~ ., drop.unused.levels = TRUE, tab)
    	tab <- addmargins(tab, margin = c(1, length(tab_vars) + 1)
      	, FUN = list(Total = sum)
      	, quiet = TRUE
     	)
    	tab <- ftable(tab)
    	tab_list[[new_var]] <- tab
	}
  	if ("wide" %in% type){
   	tabs <- do.call("cbind", tab_list)
  	} else {
   	tabs <- tab_list
  	}
	result <- list(tabs = tabs
			, codebook = codebook
			, working_df_updated = df
		)
}
