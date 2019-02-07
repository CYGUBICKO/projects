# A multiple response (binary) tabulation functions
## Depends on
# * cleanVarlabs()
# * addVarlabs
# * library(expss)
# df - dataframe
# rowvars - Multiple/binary (group) variables
# colvars - Variables which breaks table by column

multiresTabs <- function(df, rowvars, colvars, pattern = ".*: "){
	clean_labs_df <- cleanVarlabs(colvars, pattern = pattern)
  	labelled_df <- addVarlabs(df, clean_labs_df)
  	row_df <- select(df, rowvars)
  	column_df <- select(labelled_df, colvars)
	# Counts
	multicount_tab <- cro_cases(mdset(column_df), list(total(label = "Total"), row_df))
	multicount_tab <- (multicount_tab
		%>% drop_c()
		%>% drop_r()
	)
  	# Column percent
	multicolper_tab <- cro_cpct(mdset(column_df), list(total(label = "Total"), row_df))
  	multicolper_tab <- (multicolper_tab
   	%>% drop_c() 
    	%>% drop_r()
	)
  	multicolper_tab_long <- (multicolper_tab
    	%>% as_tibble()
    	%>% filter(!row_labels %in% grep("Total", row_labels, value = TRUE))
    	%>% arrange(desc(Total))
    	%>% gather(col_var, col_per, -1)
	)

	# Row percent
	multirowper_tab <- cro_rpct(mdset(column_df), list(row_df))
  	multirowper_tab <- (multirowper_tab
   	%>% drop_c() 
    	%>% drop_r()
	)
  	multirowper_tab_long <- (multirowper_tab
    	%>% as_tibble()
    	%>% filter(!row_labels %in% grep("Total", row_labels, value = TRUE))
    	%>% gather(col_var, row_per, -1)
	)
	return(
		list(multicount_tab = multicount_tab
			, multicolper_tab_wide = multicolper_tab
			, multicolper_tab_long = multicolper_tab_long
			, multirowper_tab_wide = multirowper_tab
			, multirowper_tab_long = multirowper_tab_long
		)
	)
}

# Plot multiple response tables

multiresPlot <- function(multires_df, legend_title, plot_title = NULL){
yvar <- grep("_per", names(multires_df), value = TRUE)
multires_df <- (multires_df
	%>% rename_(.dots = setNames(yvar, "new_var"))
)
multi_plot <- (ggplot(multires_df, aes(x = reorder(row_labels, -new_var), y = new_var/100,  fill = col_var))
	+ geom_bar(stat="identity", width = 1, alpha = 0.5, position = position_dodge(width = 0.7))
	+ scale_y_continuous(labels = percent)
	+ ggtitle(plot_title)
 	+ guides(fill = guide_legend(title = legend_title))
	+ theme(plot.title = element_text(hjust = 0.5, size = 8))
	+ coord_flip()
	)
	multi_plot
}

save.image("multiresFuncs.rda")
