#### ---- Project: APHRC Verbal Autopsy Data ----
#### ---- Task: Descriptives ----
#### ---- Sub-Task: Demographics ----
#### ---- By: Steve and Jonathan ----
#### ---- Date: 2019 Jan 29 (Tue) ----

library(tidyr)
library(dplyr)
library(expss)
library(ggplot2)
library(scales)

load("demographicFunc.rda")
load("globalFunctions.rda")
load("completeVA.rda")
load("multiresFuncs.rda")

theme_set(theme_bw()+
theme(panel.spacing=grid::unit(0,"lines")))

#### ---- 1. Deaths per year ----

tab_vars <- c("slumarea", "yeardeath")
legend_title <- "EA"
xaxis_order <- as.character(c(2002:2016))

deathyear_summary <- demographCounts(df = working_df
  	, tab_vars = tab_vars
  	, legend_title = legend_title
	, xaxis_order
)
deathyear_plot <- deathyear_summary[["count_plot"]]

deathyear_plot <- (deathyear_plot 
	+ xlab("Years")
)
deathyear_plot

#### ---- 2. Age groups ----

tab_vars <- c("slumarea", "agegroupdeath")
y_limits <- c(0, 0.2)
legend_title <- "EA"
xaxis_order <- pull(working_df, agegroupdeath) %>% levels()

age_group_summary <- demographProps(df = working_df
  	, tab_vars = tab_vars
  	, legend_title = legend_title
  	, y_limits = y_limits
	, xaxis_order
)
age_group_plot <- age_group_summary[["prop_plot"]]
age_group_plot <- (age_group_plot 
	+ theme(axis.text.x=element_text(angle=90))
	+ xlab("Age groups")
)
age_group_plot


#### ---- 3. Absolute ages -----

xvar <- "yeardeath"
yvar <- "agedeath_years"
colvar <- "slumarea"
abs_age_summary <- demographicMean(df = working_df
	, xvar = xvar 
	, yvar = yvar
	, colvar = colvar
)

abs_age_plot <- abs_age_summary[["mean_plot"]]
abs_age_plot <- abs_age_plot + xlab("Year of death") + ylab("Age at death")
abs_age_plot

#### ---- 4. General Symptoms -----
# Variables for every section have been grouped and totals calculated in cleaning script

#### ---- 4.1 Total number of symptoms -----

xvar <- "yeardeath"
yvar <- "total_symptoms"
colvar <- "slumarea"
abs_symptoms_summary <- demographicMean(df = working_df
	, xvar = xvar 
	, yvar = yvar
	, colvar = colvar
)

abs_symptoms_plot <- abs_symptoms_summary[["mean_plot"]]
abs_symptoms_plot <- abs_symptoms_plot + xlab("Year of death") + ylab("Number of symptoms")
abs_symptoms_plot

#### ---- 4.1 Distribution of symptoms ----

colvars <- "slumarea"
rowvars <- grouped_vars[["sec1_var"]]
symptoms_dist <- (working_df
 %>% multiresTabs(colvars, rowvars)
)
symptoms_count_tab <- symptoms_dist[["multicount_tab"]]
symptoms_colper_tab <- symptoms_dist[["multicolper_tab_long"]]
symptoms_rowper_tab <- symptoms_dist[["multirowper_tab_long"]]

legend_title <- "EA/Total"
plot_title <- "Prevalence"
cols <- c("Total" = "red4", "korogocho" = "green4", "viwandani" = "blue4")
symptoms_plot <- (symptoms_colper_tab
 %>% multiresPlot(legend_title, plot_title)
 + scale_fill_manual(
 	values = cols
	, breaks = c("Total", "korogocho", "viwandani")
	, labels = c("Total", "korogocho", "viwandani")
 )
 + ylab("Percentage")
 + xlab("Symptoms")
)

symptoms_plot
