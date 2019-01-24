library(dplyr)
library(tibble)
library(ggplot2)
library(tidyr)
library(ggthemes)
library(DT)

## Set theme
#theme_set(theme_minimal())
theme_set(theme_bw())

issueText <- "other|refuse|NIU|missi"

### Background information
# 1.7. Number of rooms used
hha_sleeprooms_total_issues <- grep("other|refuse|NIU|missi"
	, working_df[["hha_sleeprooms_total"]]
	, value = TRUE
)
hha_sleeprooms_total_issues_df <- (working_df
	%>% filter(hha_sleeprooms_total %in% c(hha_sleeprooms_total_issues, 0))
)

sleeprooms_issues <- (working_df
	%>% filter(
		(grepl(issueText, hha_sleeprooms_total))
		| (hha_sleeprooms_total==0)
	)
)

vars <- c("hha_intvwyear", "hha_slumarea", "hha_sleeprooms_total")
hha_sleeprooms_total_issues_tab <- (hha_sleeprooms_total_issues_df
	%>% tabsFunc(vars = vars)
)

hha_sleeprooms_total_labs <- extractLabs("hha_sleeprooms_total")
hha_sleeprooms_total_issues_counts <- hha_sleeprooms_total_issues_tab[["count_df"]]
hha_sleeprooms_total_issues_counts_html <- datatable(hha_sleeprooms_total_issues_counts
	, caption = hha_sleeprooms_total_labs
)

# Plot
q1_7_plot_df <- propFunc(hha_sleeprooms_total_issues_df, vars)
hha_sleeprooms_total_count_plot <- (
	ggplot(q1_7_plot_df, aes(x = hha_intvwyear
			, y = n
			, group = hha_sleeprooms_total
			, color = hha_sleeprooms_total
		)
	 )
	+ geom_line(linetype = 2
		, size = 1
	)
	+ geom_point(aes(y = n))
	+ labs(title = hha_sleeprooms_total_labs
			, x = "Years"
			, y = "# of cases"
	)
	+ guides(color = guide_legend(title = "Rooms in the structure"))
	+ theme(plot.title = element_text(hjust = 0.5))
	+ facet_wrap(~hha_slumarea)
)
#hha_sleeprooms_total_count_plot 

## Summary 1.7 without issues
working_df_updated <- (working_df
	%>% mutate(hha_sleeprooms_total_new = ifelse(!hha_sleeprooms_total %in% c(hha_sleeprooms_total_issues, 0)
		, hha_sleeprooms_total, NA
		)
	)
)

# Update codebook
codebook_update <- data.frame(variable = "hha_sleeprooms_total_new"
	, description = paste0(hha_sleeprooms_total_labs, "(new)")
)

codebook <- rbind(codebook, codebook_update)

vars <- c("hha_intvwyear", "hha_slumarea")
hha_sleeprooms_total_summary <- (working_df_updated
	%>% group_by_(.dots = vars)
	%>% summarise(mean = mean(hha_sleeprooms_total_new, na.rm = TRUE))
)

hha_sleeprooms_total_mean_plot <- (
	ggplot(hha_sleeprooms_total_summary, aes(x = hha_intvwyear
		, y = mean
		, group = hha_slumarea
		, color = hha_slumarea
	)
	)
	+ geom_line(linetype = 2
		, size = 1
	)
	+ geom_point(aes(y = mean))
	+ labs(title = extractLabs("hha_sleeprooms_total_new")
		, x = "Years"
		, y = "Mean"
	)
	+ guides(color = guide_legend(title = "Average number of rooms"))
	+ theme(plot.title = element_text(hjust = 0.5))
)
#hha_sleeprooms_total_mean_plot 


### ---- Total number of rentable houses -----

hha_rentablerooms_total_issues <- grep("other|refuse|NIU|missi"
	, working_df_updated$hha_rentablerooms_total
	, value = TRUE
)
hha_rentablerooms_total_issues_df <- (working_df_updated
	%>% filter(hha_rentablerooms_total %in% hha_rentablerooms_total_issues)
)
vars <- c("hha_intvwyear", "hha_slumarea", "hha_rentablerooms_total")
hha_rentablerooms_total_issues_tab <- (hha_rentablerooms_total_issues_df
	%>% tabsFunc(vars = vars)
)

hha_rentablerooms_total_labs <- extractLabs("hha_rentablerooms_total")
hha_rentablerooms_total_issues_counts <- hha_rentablerooms_total_issues_tab$count_df
hha_rentablerooms_total_issues_counts_html <- datatable(hha_rentablerooms_total_issues_counts
	, caption = hha_rentablerooms_total_labs
)

# Plot
q1_10_plot_df <- propFunc(hha_rentablerooms_total_issues_df, vars)
hha_rentablerooms_total_count_plot <- (
	ggplot(q1_10_plot_df, aes(x = hha_intvwyear
			, group = hha_rentablerooms_total
			, y = n
			, color = hha_rentablerooms_total
		)
	)
	+ geom_line(linetype = 2
		, size = 1
	)
	+ geom_point(aes(y = n))
	+ labs(title = hha_rentablerooms_total_labs
		, x = "Years"
		, y = "# of cases"
	)
	+ guides(color = guide_legend(title = "Rental rooms available"))
	+ theme(plot.title = element_text(hjust = 0.5))
	+ facet_wrap(~hha_slumarea)
)

## Summary 1.10 without issues
working_df_updated <- (working_df_updated
	%>% mutate(hha_rentablerooms_total_new = ifelse(!hha_rentablerooms_total %in% c(hha_rentablerooms_total_issues, 0)
			, hha_rentablerooms_total, NA
		)
	)
)

# Update codebook
codebook_update <- data.frame(variable = "hha_rentablerooms_total_new"
	, description = paste0(hha_rentablerooms_total_labs, "(new)")
)

codebook <- rbind(codebook, codebook_update)

vars <- c("hha_intvwyear", "hha_slumarea")
hha_rentablerooms_total_summary <- (working_df_updated
	%>% group_by_(.dots = vars)
	%>% summarise(mean = mean(hha_rentablerooms_total_new, na.rm = TRUE))
)

hha_rentablerooms_total_mean_plot <- (
	ggplot(hha_rentablerooms_total_summary, aes(x = hha_intvwyear
			, y = mean
			, group = hha_slumarea
			, color = hha_slumarea
		)
	)
		+ geom_line(linetype = 2
		, size = 1
	)
	+ geom_point(aes(y = mean))
	+ labs(title = extractLabs("hha_rentablerooms_total_new")
		, x = "Years"
		, y = "Mean"
	)
	+ guides(color = guide_legend(title = "Average number of rentable rooms available"))
	+ theme(plot.title = element_text(hjust = 0.5))
)

### ---- Total number of people in this household -----

hha_numpeople_total_issues <- grep("other|refuse|NIU|missi"
	, working_df_updated$hha_numpeople_total
	, value = TRUE
)
hha_numpeople_total_issues_df <- (working_df_updated
	%>% filter(hha_numpeople_total %in% hha_numpeople_total_issues)
)
vars <- c("hha_intvwyear", "hha_slumarea", "hha_numpeople_total")
hha_numpeople_total_issues_tab <- (hha_numpeople_total_issues_df
	%>% tabsFunc(vars = vars)
)

hha_numpeople_total_labs <- extractLabs("hha_numpeople_total")
hha_numpeople_total_issues_counts <- hha_numpeople_total_issues_tab$count_df
hha_numpeople_total_issues_counts_html <- datatable(hha_numpeople_total_issues_counts
	, caption = hha_numpeople_total_labs
)

# Plot
q1_12_plot_df <- propFunc(hha_numpeople_total_issues_df, vars)
hha_numpeople_total_count_plot <- (
	ggplot(q1_12_plot_df, aes(x = hha_intvwyear
			, y = n
			, group = hha_numpeople_total
			, color = hha_numpeople_total
		)
	)
	+ geom_line(linetype = 2
		, size = 1
	)
	+ geom_point(aes(y = n))
	+ labs(title = hha_numpeople_total_labs
		, x = "Years"
		, y = "# of cases"
	)
	+ guides(color = guide_legend(title = "Number of people in this household"))
	+ theme(plot.title = element_text(hjust = 0.5))
	+ facet_wrap(~hha_slumarea)
)

## Summary 1.12 without issues
working_df_updated <- (working_df_updated
	%>% mutate(hha_numpeople_total_new = ifelse(!hha_numpeople_total %in% c(hha_numpeople_total_issues, 0)
			, hha_numpeople_total, NA
		)
	)
)

# Update codebook
codebook_update <- data.frame(variable = "hha_numpeople_total_new"
	, description = paste0(hha_numpeople_total_labs, "(new)")
)

codebook <- rbind(codebook, codebook_update)

vars <- c("hha_intvwyear", "hha_slumarea")
hha_numpeople_total_summary <- (working_df_updated
	%>% group_by_(.dots = vars)
	%>% summarise(mean = mean(hha_numpeople_total_new, na.rm = TRUE))
)

hha_numpeople_total_mean_plot <- (
	ggplot(hha_numpeople_total_summary, aes(x = hha_intvwyear
			, y = mean
			, group = hha_slumarea
			, color = hha_slumarea
		)
	)
	+ geom_line(linetype = 2
		, size = 1
	)
	+ geom_point(aes(y = mean))
	+ labs(title = extractLabs("hha_numpeople_total_new")
		, x = "Years"
		, y = "Mean"
	)
	+ guides(color = guide_legend(title = "Average number of people in the household"))
	+ theme(plot.title = element_text(hjust = 0.5))
)

## Objects to report
#hha_sleeprooms_total_issues_counts_html
#hha_sleeprooms_total_count_plot
hha_sleeprooms_total_mean_plot

#hha_rentablerooms_total_issues_counts_html
#hha_rentablerooms_total_count_plot
hha_rentablerooms_total_mean_plot

#hha_numpeople_total_issues_counts_html
#hha_numpeople_total_count_plot
hha_numpeople_total_mean_plot



save(file=rdaname
   , working_df_updated
#   , codebook
#   , missPropFunc
#   # Global functions
#   , saveXlsx
#   , varLabs
#   , extractLabs
#   , propFunc
#   , tabsFunc
#   , recodeLabs
#   , extractIssues
#   , file_prefix
#   # Working df chunk
#   , miss_prop_df
#   , miss_prop_df_html
#   , no_vars_droped
#   # Missing values chunk
#   , miss_dist_plot
#   # ID variables
#   , id_dup_dis
	# Background information
	, hha_sleeprooms_total_issues_counts_html
	, hha_sleeprooms_total_count_plot
	, hha_sleeprooms_total_mean_plot
	, hha_rentablerooms_total_issues_counts_html
	, hha_rentablerooms_total_count_plot
	, hha_rentablerooms_total_mean_plot
	, hha_numpeople_total_issues_counts_html
	, hha_numpeople_total_count_plot
	, hha_numpeople_total_mean_plot
)




#rm(list = ls()[!ls() %in% c(
#			"working_df_updated"
#			, "codebook"
#			# Functions
#			, "missPropFunc"
#			, "saveXlsx"
#			, "varLabs"
#			, "extractLabs"
#			, "propFunc"
#			, "tabsFunc"
#			, "recodeLabs"
#			, "extractIssues"
#			# missProp.Rout
#			, "miss_prop_df_html"
#			, "no_vars_droped"
#			, "file_prefix"
#			# idVars.Rout
#			, "id_dup_dis"
#			# backgroundSummary
#			, "hha_sleeprooms_total_issues_counts_html"
#			, "hha_sleeprooms_total_count_plot"
#			, "hha_sleeprooms_total_mean_plot"
#			, "hha_rentablerooms_total_issues_counts_html"
#			, "hha_rentablerooms_total_count_plot"
#			, "hha_rentablerooms_total_mean_plot"
#			, "hha_numpeople_total_issues_counts_html"
#			, "hha_numpeople_total_count_plot"
#			, "hha_numpeople_total_mean_plot"
#		)
#	]
#)
