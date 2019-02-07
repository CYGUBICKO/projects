## This function plots the proportions after cases with missing values or any other isses has been removed.

demographProps <- function(df
  	, tab_vars
  	, legend_title
  	, y_limits = c(0,1)
	, xaxis_order
	){
	
	var <- last(tab_vars)
	issue_labs <- extractLabs(var)
	summary_df <- (df
	   %>% propFunc(tab_vars)
	   %>% drop_na()
	   %>% rename_(new_var = var)
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
	   + ggtitle(issue_labs)
		+ ylab("Proportions")
	   + scale_y_continuous(labels = percent, limits = y_limits)
		+ scale_x_discrete(limits =  xaxis_order)
	   + guides(color = guide_legend(title = legend_title))
		+ theme(plot.title = element_text(hjust = 0.5, size = 8))
	)

	return(
		list(
      	prop_plot = prop_plot
		)
	)
}



## This function plots the counts after cases with missing values or any other isses has been removed.

demographCounts <- function(df
  	, tab_vars
  	, legend_title
	, xaxis_order
	){
	
	var <- last(tab_vars)
	issue_labs <- extractLabs(var)
	summary_df <- (df
	   %>% propFunc(tab_vars)
	   %>% drop_na()
	   %>% rename_(new_var = var)
	)
	count_plot <- (ggplot(summary_df
	   , aes_string(x = "new_var"
			, y = "n"
	  		, group = tab_vars[1]
	   	, color = tab_vars[1]
		)
		)
	   + geom_line(linetype = 2
	   	, size = 1
		)
	   + geom_point(aes(y = n))
	   + ggtitle(issue_labs)
		+ ylab("Counts")
		+ scale_x_discrete(limits =  xaxis_order)
	   + guides(color = guide_legend(title = legend_title))
		+ theme(plot.title = element_text(hjust = 0.5, size = 8))
	)
	return(list(count_plot = count_plot))
}


## Plot boxplot for continous variables
# yvar - is the continous variable
# colvar - Variable to use in colouring or legend
# xvar - x variable

demographicMean <- function(df, xvar, yvar, colvar){
	issue_labs <- extractLabs(yvar)
	mean_plot <- (ggplot(df, aes_string(x = xvar))
		+ geom_boxplot(aes_string(y = yvar, colour = colvar), outlier.colour = NULL)
		+ scale_colour_brewer(palette = "Dark2")
		+ ggtitle(issue_labs)
  		+ theme(plot.title = element_text(hjust = 0.5, size = 8))
	)
	return(list(mean_plot = mean_plot))
}

save.image("demographicFunc.rda")
