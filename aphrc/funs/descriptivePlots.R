## This function plots the proportions after cases with missing values or any other issues has been removed.

propPlot <- function(df
  	, tab_vars
  	, legend_title
  	, y_limits = c(0,1)
	){
	
	var <- last(tab_vars)
	issue_labs <- extractLabs(var)
	summary_df <- (df
	   %>% propFunc(tab_vars)
	   %>% drop_na()
	)
	
	prop_plot <- (ggplot(summary_df
	   , aes_string(x = first(tab_vars)
			, y = "prop"
	  		, group = var
	   	, color = var
		)
		)
	   + geom_line()
		+ geom_point(aes(y = prop))
	   + ggtitle(issue_labs)
		+ ylab("Proportions")
	   + scale_y_continuous(labels = percent, limits = y_limits)
	   + guides(color = guide_legend(title = legend_title))
		+ theme(plot.title = element_text(hjust = 0.5))
	)

	return(
		list(
      	prop_plot = prop_plot
		)
	)
}



## This function plots the counts after cases with missing values or any other issues has been removed.

countPlot <- function(df
  	, tab_vars
  	, legend_title
	){
	
	var <- last(tab_vars)
	issue_labs <- extractLabs(var)
	summary_df <- (df
	   %>% propFunc(tab_vars)
	   %>% drop_na()
	)
	count_plot <- (ggplot(summary_df
	   , aes_string(x = first(tab_vars)
			, y = "n"
	  		, group = var
	   	, color = var
		)
		)
	   + geom_line()
	   + geom_point(aes(y = n))
	   + ggtitle(issue_labs)
		+ ylab("Counts")
	   + guides(color = guide_legend(title = legend_title))
		+ theme(plot.title = element_text(hjust = 0.5))
	)
	return(list(count_plot = count_plot))
}


## Plot boxplot for continous variables
# yvar - is the continous variable
# colvar - Variable to use in colouring or legend
# xvar - x variable

meanPlot <- function(df, xvar, yvar, colvar){
	issue_labs <- extractLabs(yvar)
	mean_plot <- (ggplot(df, aes_string(x = xvar))
		+ geom_boxplot(aes_string(y = yvar, colour = colvar), outlier.colour = NULL)
#		+ geom_violin(aes_string(y = yvar, colour = colvar), na.rm = TRUE)
		+ scale_colour_brewer(palette = "Dark2")
		+ ggtitle(issue_labs)
  		+ theme(plot.title = element_text(hjust = 0.5))
	)
	return(list(mean_plot = mean_plot))
}
