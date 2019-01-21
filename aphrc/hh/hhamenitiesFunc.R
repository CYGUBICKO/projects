### This function generates a table and a plot of cases which seems to be missing based on the labels

hhaissuesFunc <- function(df, var, pattern, tab_vars, legend_title){
# Issues
issues_summary <- (df
	%>% extractIssues(var, pattern, tab_vars)
)
  
# Summarize issues
issues_tab <- issues_summary$issues_tab
issue_labs <- extractLabs(var)
issues_counts <- issues_tab$count_df
issues_counts_html <- datatable(issues_counts
	, caption = issue_labs
)
  
# Issue summary plot function
issues_df <- issues_summary$issues_df
issues_plot_df <- (issues_df
	%>% propFunc(tab_vars)
   %>% rename_(new_var = var)
)
  
issues_count_plot <- (ggplot(issues_plot_df
	, aes(x = hha_intvwyear
			, y = n
      	, group = new_var
      	, color = new_var
    	)
	)
   + geom_line(linetype = 2
   	, size = 1
   )
   + geom_point(aes(y = n))
   + labs(title = issue_labs
   	, x = "Years"
      , y = "# of cases"
  	)
   + guides(color = guide_legend(title = legend_title))
   + theme(plot.title = element_text(hjust = 0.5)
		, legend.position = 'bottom'
      , axis.ticks = element_blank()
   )
   + facet_wrap(~hha_slumarea)
  )
  return(
    list(issues_counts_html = issues_counts_html
         , issues_count_plot = issues_count_plot
    )
  )
}

## This function plots the proportions after cases with missing values or any other isses has been removed.

hhcleanedFunc <- function(df
	, var
	, patterns
	, replacements
  	, tab_vars
  	, legend_title
  	, y_limits = c(0,1)
	){
	df_updated <- (df
   	%>% recodeLabs(var, patterns, replacements)
	)
	# Update codebook
  	issue_labs <- extractLabs(var)
  	codebook_update <- data.frame(variable = var
		, description = paste0(issue_labs, "(new)")
	)
  codebook <- rbind(codebook, codebook_update)
	summary_df <- (df_updated
   	%>% propFunc(tab_vars)
   	%>% drop_na()
   	%>% rename_(new_var = paste0(var, "_new"))
	)
	prop_plot <- (ggplot(summary_df
   	, aes(x = hha_intvwyear
			, y = prop
        	, group = new_var
        	, color = new_var
			)
		)
   	+ geom_line(linetype = 2
  		 	, size = 1
		)
   	+ geom_point(aes(y = prop))
   	+ labs(title = issue_labs
   		, x = "Years"
      	, y = "Proportion"
		)
   	+ scale_y_continuous(labels = percent, limits = y_limits)
   	+ guides(color = guide_legend(title = legend_title))
   	+ theme(plot.title = element_text(hjust = 0.5)
			, legend.position = 'bottom'
      	, axis.ticks = element_blank()
		)
   	+ facet_wrap(~hha_slumarea)
	)
	return(
		list(
      	working_df_updated = df_updated
      	, codebook = codebook
      	, prop_plot = prop_plot
		)
	)
}


# This function plots the mean after cases with missing values or any other isses has been removed.

hhmeanFunc <- function(df
	, var
	, patterns
	, replacements
  	, tab_vars
  	, legend_title
	){
	df_updated <- (df
   	%>% recodeLabs(var, patterns, replacements)
	)
	# Update codebook
  	issue_labs <- extractLabs(var)
  	codebook_update <- data.frame(variable = var
		, description = paste0(issue_labs, "(new)")
	)
	codebook <- rbind(codebook, codebook_update)
	summary_df <- (df_updated
		%>% rename_(new_var = paste0(var, "_new"))
	  	%>% mutate(new_var = as.numeric(new_var))
    	%>% group_by_(.dots = tab_vars)
    	%>% summarise(mean = mean(new_var, na.rm = TRUE))
	  	%>% drop_na()
	)
	
	mean_plot <- (
   ggplot(summary_df, aes(x = hha_intvwyear
      , y = mean
      , group = hha_slumarea
      , color = hha_slumarea
	)
   )
   + geom_line(linetype = 2
      , size = 1
   )
   + geom_point(aes(y = mean))
   + labs(title = issue_labs
      , x = "Years"
      , y = "Mean"
   )
   + guides(color = guide_legend(title = legend_title))
   + theme(plot.title = element_text(hjust = 0.5))
	)
	return(
		list(
      	working_df_updated = df_updated
      	, codebook = codebook
      	, mean_plot = mean_plot
		)
	)
}

