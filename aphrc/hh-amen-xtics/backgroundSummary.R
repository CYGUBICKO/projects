library(dplyr)
library(tibble)
library(ggplot2)
library(tidyr)
library(ztable)
library(ggthemes)

## Set theme
theme_set(theme_minimal())
#theme_set(theme_bw())


### Background information
# 1.7. Number of rooms used
hha_sleeprooms_thisstructure_issues <- grep("other|refuse|NIU|missi"
	, working_df$hha_sleeprooms_thisstructure
	, value = TRUE
)
hha_sleeprooms_thisstructure_issues_df <- (working_df
	%>% filter(hha_sleeprooms_thisstructure %in% c(hha_sleeprooms_thisstructure_issues, 0))
)
vars <- c("hha_intvwyear", "hha_sleeprooms_thisstructure", "hha_slumarea")
hha_sleeprooms_thisstructure_issues_tab <- (hha_sleeprooms_thisstructure_issues_df
	%>% tabsFunc(vars = vars)
)

hha_sleeprooms_thisstructure_labs <- extractLabs("hha_sleeprooms_thisstructure")
hha_sleeprooms_thisstructure_issues_counts <- hha_sleeprooms_thisstructure_issues_tab$count_df
formatTabs(hha_sleeprooms_thisstructure_issues_counts
	, levs = hha_sleeprooms_thisstructure_issues_tab$levs
	, unit = "n"
	, col_label = hha_sleeprooms_thisstructure_labs
	, no_digits = 2
)

# Plot
q1_7_plot_df <- propFunc(hha_sleeprooms_thisstructure_issues_df, vars)
hha_sleeprooms_thisstructure_count_plot <- (
	ggplot(q1_7_plot_df, aes(x = hha_intvwyear
			, y = n
			, group = hha_sleeprooms_thisstructure
			, color = hha_sleeprooms_thisstructure
		)
	 )
	+ geom_line(linetype = 2
		, size = 1
	)
	+ geom_point(aes(y = n))
	+ labs(title = hha_sleeprooms_thisstructure_labs
			, x = "Years"
			, y = "# of cases"
	)
	+ guides(color = guide_legend(title = "Rooms in the structure"))
	+ theme(plot.title = element_text(hjust = 0.5))
	+ facet_wrap(~hha_slumarea)
)
hha_sleeprooms_thisstructure_count_plot 

## Summary 1.7 without issues
working_df_updated <- (working_df
	%>% mutate(hha_sleeprooms_thisstructure_new = ifelse(!hha_sleeprooms_thisstructure %in% c(hha_sleeprooms_thisstructure_issues, 0)
		, hha_sleeprooms_thisstructure, NA
		)
	)
)

# Update codebook
codebook_update <- data.frame(variable = "hha_sleeprooms_thisstructure_new"
	, description = paste0(hha_sleeprooms_thisstructure_labs, "(new)")
)

codebook <- rbind(codebook, codebook_update)

vars <- c("hha_intvwyear", "hha_slumarea")
hha_sleeprooms_thisstructure_summary <- (working_df_updated
	%>% group_by_(.dots = vars)
	%>% summarise(mean = mean(hha_sleeprooms_thisstructure_new, na.rm = TRUE))
)

hha_sleeprooms_thisstructure_mean_plot <- (
	ggplot(hha_sleeprooms_thisstructure_summary, aes(x = hha_intvwyear
		, y = mean
		, group = hha_slumarea
		, color = hha_slumarea
	)
	)
	+ geom_line(linetype = 2
		, size = 1
	)
	+ geom_point(aes(y = mean))
	+ labs(title = extractLabs("hha_sleeprooms_thisstructure_new")
		, x = "Years"
		, y = "Mean"
	)
	+ guides(color = guide_legend(title = "Average number of rooms"))
	+ theme(plot.title = element_text(hjust = 0.5))
)
hha_sleeprooms_thisstructure_mean_plot 
