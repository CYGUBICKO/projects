library(dplyr)
library(tibble)
library(ggplot2)
library(tidyr)
library(ggthemes)
library(DT)
library(scales)

## Set theme
#theme_set(theme_minimal())
theme_set(theme_bw())

###################################################################

### ----- Source of Water -----
## Source of drinking water issues

var <- "hha_drinkwatersource"
pattern <- "don't know|refuse|NIU|missi"
tab_vars <- c("hha_intvwyear", "hha_slumarea", "hha_drinkwatersource")
hha_drinkwatersource_issues <- hhaissuesFunc(working_df_updated
	, var
	, pattern
	, tab_vars
	, "Sources of drinking water"
)
hha_drinkwatersource_issues_count <- hha_drinkwatersource_issues$issues_counts_html
hha_drinkwatersource_issues_plot <- hha_drinkwatersource_issues$issues_count_plot
hha_drinkwatersource_issues_plot 

## Summary of cleaned variable
## Collapse sources of drinking water to fewer categories
patterns <- c("buy from:"
  	, "piped:"
  	, "surface|rainwater|well:"
  	, "other"
	, "don't know|refuse|NIU|missi"
)
replacements <- c("Buying"
  	, "Piped"
  	, "Surface/Rain/Well"
  	, "Others"
  	, NA
)
tab_vars <- c("hha_intvwyear", "hha_slumarea", "hha_drinkwatersource_new")
hha_drinkwatersource_clean <- hhcleanedFunc(working_df_updated
  	, var
  	, patterns
  	, replacements
  	, tab_vars
  	, legend_title = "Sources of drinking water"
	, y_limits = c(0, 1)
)

working_df_updated <- hha_drinkwatersource_clean$working_df_updated
codebook <- hha_drinkwatersource_clean$codebook
hha_drinkwatersource_clean_prop_plot <- hha_drinkwatersource_clean$prop_plot
hha_drinkwatersource_clean_prop_plot 
##################################################################

#### ---- 2.2 Kind of toilet facility < 5 years ----
## Issues
var <- "hha_toilet_2to4yrs"
pattern <- "don't know|refuse|NIU|missi|no members"
tab_vars <- c("hha_intvwyear", "hha_slumarea", "hha_toilet_2to4yrs")
hha_toilet_2to4yrs_issues <- hhaissuesFunc(working_df_updated
	, var
	, pattern
	, tab_vars
	, "Toilet facilities"
)

hha_toilet_2to4yrs_issues_count <- hha_toilet_2to4yrs_issues$issues_counts_html
hha_toilet_2to4yrs_issues_plot <- hha_toilet_2to4yrs_issues$issues_count_plot
hha_toilet_2to4yrs_issues_plot 

## Summary of cleaned variable
var <- "hha_toilet_2to4yrs"
tab_vars <- c("hha_intvwyear", "hha_slumarea", "hha_toilet_2to4yrs_new")
patterns <- c("flush:"
   , "traditional pit latrine:"
   , "ventilated improved pit latrine:"
   , "other"
   , "don't know|refuse|NIU|missi|no members"
)

replacements <- c("flush:"
   , "traditional pit latrine:"
   , "ventilated improved pit latrine:"
   , "other"
   , NA
)

hha_toilet_2to4yrs_clean <- hhcleanedFunc(working_df_updated
  	, var
  	, patterns
  	, replacements
  	, tab_vars
  	, legend_title = "Toilet facilities"
	, y_limits = c(0, 0.15)
)

working_df_updated <- hha_toilet_2to4yrs_clean$working_df_updated
codebook <- hha_toilet_2to4yrs_clean$codebook
hha_toilet_2to4yrs_clean_prop_plot <- hha_toilet_2to4yrs_clean$prop_plot

hha_toilet_2to4yrs_clean_prop_plot 
