## This function plots the proportions after cases with missing values or any other isses has been removed.

demographTabs <- function(df
	, var
	, patterns
	, replacements
  	, tab_vars
  	, legend_title
  	, y_limits = c(0,1)
	){
	all_vars <- c(var, tab_vars)
	df_updated <- (df
   		%>% recodeLabs(all_vars, patterns, replacements)
	)
	# Update codebook
  	issue_labs <- extractLabs(var)
  	codebook_update <- data.frame(variable = var
		, description = paste0(issue_labs, "(new)")
	)
  codebook <- rbind(codebook, codebook_update)
	summary_df <- (df_updated
   	%>% propFunc(all_vars)
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
   	#+ facet_wrap(~hha_slumarea)
	)
	return(
		list(
      	working_df_updated = df_updated
      	, codebook = codebook
      	, prop_plot = prop_plot
		)
	)
}

