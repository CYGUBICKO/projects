#### ---- Project: APHRC Verbal Autopsy Data ----
#### ---- Task: Descriptives ----
#### ---- Sub-Task: General Symptoms ----
#### ---- By: Steve and Jonathan ----
#### ---- Date: 2019 Jan 29 (Tue) ----

library(tidyr)
library(dplyr)
library(expss)
library(ggplot2)
library(scales)
library(DT)

load("demographicFunc.rda")
load("globalFunctions.rda")
load("completeVA.rda")
load("multiresFuncs.rda")

theme_set(theme_bw()+
	theme(panel.spacing=grid::unit(0,"lines")))

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


#### ---- Other forms of illness before death ----

colvars <- "slumarea"
rowvars <- grouped_vars[["b4dth_var"]]
b4dth_dist <- (working_df
 %>% multiresTabs(colvars, rowvars)
)
b4dth_count_tab <- b4dth_dist[["multicount_tab"]]
b4dth_colper_tab <- b4dth_dist[["multicolper_tab_long"]]
b4dth_rowper_tab <- b4dth_dist[["multirowper_tab_long"]]

legend_title <- "EA/Total"
plot_title <- "Prevalence"
cols <- c("Total" = "red4", "korogocho" = "green4", "viwandani" = "blue4")
b4dth_plot <- (b4dth_colper_tab
 %>% multiresPlot(legend_title, plot_title)
 + scale_fill_manual(
 	values = cols
	, breaks = c("Total", "korogocho", "viwandani")
	, labels = c("Total", "korogocho", "viwandani")
 )
 + ylab("Percentage")
 + xlab("Symptoms")
)
b4dth_plot



# Save objects
symptoms_saved_tabs <- sapply(grep("_tab$", ls(), value = TRUE), get)
symptoms_saved_plots <- sapply(grep("_plot$", ls(), value = TRUE), get)

save(file = "generalsymptoms.rda"
	, symptoms_saved_tabs
	, symptoms_saved_plots
)
