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
load("underfiveVA.rda")

#### ---- 1. Age ----

var <- "agegroupdeath"
tab_vars <- c("slumarea", "agegroupdeath_new")
patterns <- c(
   "don't know|refuse|NIU|missi"
)
replacements <- c(
   NA
)
y_limits <- c(0, 0.5)
legend_title <- "EA"
xaxis_order <- c("neonate:0-28 days", "infant:<1 Year", "child:1-4")

age_summary <- demographTabs(df = underfive_df
	, var = var
	, patterns = patterns
	, replacements = replacements
  	, tab_vars = tab_vars
  	, legend_title = legend_title
  	, y_limits = y_limits
	, xaxis_order
)
age_plot <- age_summary[["prop_plot"]]
age_plot
