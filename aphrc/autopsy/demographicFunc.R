## This function plots the proportions after cases with missing values or any other isses has been removed.

demographTabs <- function(df
	, var
	, patterns
	, replacements
  	, tab_vars
  	, legend_title
  	, y_limits = c(0,1)
	, xaxis_order
	){
	
	df_updated <- (df
		%>% recodeLabs(var, patterns, replacements)
	)
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
	   , aes_string(x = "new_var"
			, y = "prop"
	  		, group = tab_vars[1]
	   	, color = tab_vars[1]
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
		+ scale_x_discrete(limits =  xaxis_order)
	   + guides(color = guide_legend(title = legend_title))
	   + theme(plot.title = element_text(hjust = 0.5)
			, legend.position = 'right'
	      , axis.ticks = element_blank()
		)
	)

	return(
		list(
      	working_df_updated = df_updated
      	, codebook = codebook
      	, prop_plot = prop_plot
		)
	)
}

save.image("demographicFunc.rda")
