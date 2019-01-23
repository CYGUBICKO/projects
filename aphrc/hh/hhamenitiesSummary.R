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

hha_drinkwatersource_issues_count <- hha_drinkwatersource_issues[["issues_counts_html"]]
hha_drinkwatersource_issues_plot <- hha_drinkwatersource_issues[["issues_count_plot"]]

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

hha_drinkwatersource_clean_prop_plot <- hha_drinkwatersource_clean[["prop_plot"]]

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

hha_toilet_2to4yrs_issues_count <- hha_toilet_2to4yrs_issues[["issues_counts_html"]]
hha_toilet_2to4yrs_issues_plot <- hha_toilet_2to4yrs_issues[["issues_count_plot"]]

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

hha_toilet_2to4yrs_clean_prop_plot <- hha_toilet_2to4yrs_clean[["prop_plot"]]

#### ---- 2.3 Kind of toilet facility aged >= 5 years ----

## Issues
var <- "hha_toilet_5plusyrs"
pattern <- "don't know|refuse|NIU|missi|no members"
tab_vars <- c("hha_intvwyear", "hha_slumarea", "hha_toilet_5plusyrs")
hha_toilet_5plusyrs_issues <- hhaissuesFunc(working_df_updated
	, var
   , pattern
   , tab_vars
   , "Toilet facilities (>5 years)"
)

hha_toilet_5plusyrs_issues_count <- hha_toilet_5plusyrs_issues[["issues_counts_html"]]
hha_toilet_5plusyrs_issues_plot <- hha_toilet_5plusyrs_issues[["issues_count_plot"]]

## Summary of cleaned variable
var <- "hha_toilet_5plusyrs"
tab_vars <- c("hha_intvwyear", "hha_slumarea", "hha_toilet_5plusyrs_new")
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

hha_toilet_5plusyrs_clean <- hhcleanedFunc(working_df_updated
   , var
   , patterns
   , replacements
   , tab_vars
   , legend_title = "Toilet facilities (> 5 years)"
   , y_limits = c(0, 1)
)

hha_toilet_5plusyrs_clean_prop_plot <- hha_toilet_5plusyrs_clean[["prop_plot"]]
## Objects to report
#hha_toilet_5plusyrs_issues_count
#hha_toilet_5plusyrs_issues_plot
#hha_toilet_5plusyrs_clean_prop_plot


#### ---- 2.4 Pay for toylet facility ----

## Issues
var <- "hha_toilet_paytouse"
pattern <- "don't know|refuse|NIU|missi|no members|other"
tab_vars <- c("hha_intvwyear", "hha_slumarea", "hha_toilet_paytouse")
legend_title <- "Pay"
hha_toilet_paytouse_issues <- hhaissuesFunc(working_df_updated
   , var
   , pattern
   , tab_vars
   , legend_title
)

hha_toilet_paytouse_issues_count <- hha_toilet_paytouse_issues[["issues_counts_html"]]
hha_toilet_paytouse_issues_plot <- hha_toilet_paytouse_issues[["issues_count_plot"]]

## Summary of cleaned variable
var <- "hha_toilet_paytouse"
tab_vars <- c("hha_intvwyear", "hha_slumarea", "hha_toilet_paytouse_new")
legend_title <- "Pay"
patterns <- c("other"
   , "don't know|refuse|NIU|missi|no members"
)

replacements <- c(NA
   , NA
)

hha_toilet_paytouse_clean <- hhcleanedFunc(working_df_updated
   , var
   , patterns
   , replacements
   , tab_vars
   , legend_title
   , y_limits = c(0, 1)
)

hha_toilet_paytouse_clean_prop_plot <- hha_toilet_paytouse_clean[["prop_plot"]]
## Objects to report
#hha_toilet_paytouse_issues_count
#hha_toilet_paytouse_issues_plot
#hha_toilet_paytouse_clean_prop_plot


#### ---- 2.5 Pattern of payment ----

## Issues
var <- "hha_toilet_paypattern"
pattern <- "don't know|refuse|NIU|missi"
legend_title <- "Pattern"
tab_vars <- c("hha_intvwyear", "hha_slumarea", "hha_toilet_paypattern")
hha_toilet_paypattern_issues <- hhaissuesFunc(working_df_updated
   , var
   , pattern
   , tab_vars
   , legend_title
)

hha_toilet_paypattern_issues_count <- hha_toilet_paypattern_issues[["issues_counts_html"]]
hha_toilet_paypattern_issues_plot <- hha_toilet_paypattern_issues[["issues_count_plot"]]

## Summary of cleaned variable
var <- "hha_toilet_paypattern"
legend_title <- "Pattern"
y_limits = c(0, 0.60)
tab_vars <- c("hha_intvwyear", "hha_slumarea", "hha_toilet_paypattern_new")
patterns <- c(
   "other"
   , "don't know|refuse|NIU|missi"
)

replacements <- c(
   "other"
   , NA
)

hha_toilet_paypattern_clean <- hhcleanedFunc(working_df_updated
   , var
   , patterns
   , replacements
   , tab_vars
   , legend_title
   , y_limits
)

hha_toilet_paypattern_clean_prop_plot <- hha_toilet_paypattern_clean[["prop_plot"]]
## Objects to report
#hha_toilet_paypattern_issues_count
#hha_toilet_paypattern_issues_plot
#hha_toilet_paypattern_clean_prop_plot

#### ---- 2.6 Main material on the floor ----

## Issues
var <- "hha_floormaterial"
pattern <- "don't know|refuse|NIU|missi"
legend_title <- "Material"
tab_vars <- c("hha_intvwyear", "hha_slumarea", "hha_floormaterial")
hha_floormaterial_issues <- hhaissuesFunc(working_df_updated
   , var
   , pattern
   , tab_vars
   , legend_title
)

hha_floormaterial_issues_count <- hha_floormaterial_issues[["issues_counts_html"]]
hha_floormaterial_issues_plot <- hha_floormaterial_issues[["issues_count_plot"]]

## Summary of cleaned variable
var <- "hha_floormaterial"
legend_title <- "Material"
y_limits = c(0, 0.85)
tab_vars <- c("hha_intvwyear", "hha_slumarea", "hha_floormaterial_new")
patterns <- c(
   "natural:"
   , "rudimentary:"
   , "finished:"
   , "other"
   , "don't know|refuse|NIU|missi"
)

replacements <- c(
   "natural"
   , "rudimentary"
   , "finished"
   , "other"
   , NA
)

hha_floormaterial_clean <- hhcleanedFunc(working_df_updated
   , var
   , patterns
   , replacements
   , tab_vars
   , legend_title
   , y_limits
)

hha_floormaterial_clean_prop_plot <- hha_floormaterial_clean[["prop_plot"]]
## Objects to report
#hha_floormaterial_issues_count
#hha_floormaterial_issues_plot
#hha_floormaterial_clean_prop_plot

#### ---- 2.7 Main material on the roof ----

## Issues
var <- "hha_roofmaterial"
pattern <- "don't know|refuse|NIU|missi"
legend_title <- "Material"
tab_vars <- c("hha_intvwyear", "hha_slumarea", "hha_roofmaterial")
hha_roofmaterial_issues <- hhaissuesFunc(working_df_updated
   , var
   , pattern
   , tab_vars
   , legend_title
)

hha_roofmaterial_issues_count <- hha_roofmaterial_issues[["issues_counts_html"]]
hha_roofmaterial_issues_plot <- hha_roofmaterial_issues[["issues_count_plot"]]

## Summary of cleaned variable
var <- "hha_roofmaterial"
legend_title <- "Material"
y_limits = c(0, 1)
tab_vars <- c("hha_intvwyear", "hha_slumarea", "hha_roofmaterial_new")
patterns <- c(
   "natural:"
   , "rudimentary:"
   , "finished:"
   , "don't know|refuse|NIU|missi"
)

replacements <- c(
   "natural"
   , "rudimentary"
   , "finished"
   , NA
)

hha_roofmaterial_clean <- hhcleanedFunc(working_df_updated
   , var
   , patterns
   , replacements
   , tab_vars
   , legend_title
   , y_limits
)

hha_roofmaterial_clean_prop_plot <- hha_roofmaterial_clean[["prop_plot"]]
## Objects to report
#hha_roofmaterial_issues_count
#hha_roofmaterial_issues_plot
#hha_roofmaterial_clean_prop_plot


#### ---- 2.8 Main material on the wall ----

## Issues
var <- "hha_wallmaterial"
pattern <- "don't know|refuse|NIU|missi"
legend_title <- "Material"
tab_vars <- c("hha_intvwyear", "hha_slumarea", "hha_wallmaterial")
hha_wallmaterial_issues <- hhaissuesFunc(working_df_updated
   , var
   , pattern
   , tab_vars
   , legend_title
)

hha_wallmaterial_issues_count <- hha_wallmaterial_issues[["issues_counts_html"]]
hha_wallmaterial_issues_plot <- hha_wallmaterial_issues[["issues_count_plot"]]

## Summary of cleaned variable
var <- "hha_wallmaterial"
legend_title <- "Material"
y_limits = c(0, 0.85)
tab_vars <- c("hha_intvwyear", "hha_slumarea", "hha_wallmaterial_new")
patterns <- c(
   "natural:"
   , "rudimentary:"
   , "finished:"
   , "don't know|refuse|NIU|missi"
)

replacements <- c(
   "natural"
   , "rudimentary"
   , "finished"
   , NA
)

hha_wallmaterial_clean <- hhcleanedFunc(working_df_updated
   , var
   , patterns
   , replacements
   , tab_vars
   , legend_title
   , y_limits
)

hha_wallmaterial_clean_prop_plot <- hha_wallmaterial_clean[["prop_plot"]]
## Objects to report
#hha_wallmaterial_issues_count
#hha_wallmaterial_issues_plot
#hha_wallmaterial_clean_prop_plot


#### ---- 2.9 Where does your/this household do most of its cooking ----

## Issues
var <- "hha_wherecooks"
pattern <- "don't know|refuse|NIU|missi"
legend_title <- "Place to cook"
tab_vars <- c("hha_intvwyear", "hha_slumarea", "hha_wherecooks")
hha_wherecooks_issues <- hhaissuesFunc(working_df_updated
   , var
   , pattern
   , tab_vars
   , legend_title
)

hha_wherecooks_issues_count <- hha_wherecooks_issues[["issues_counts_html"]]
hha_wherecooks_issues_plot <- hha_wherecooks_issues[["issues_count_plot"]]

## Summary of cleaned variable
var <- "hha_wherecooks"
legend_title <- "Place to cook"
y_limits = c(0, 0.9)
tab_vars <- c("hha_intvwyear", "hha_slumarea", "hha_wherecooks_new")
patterns <- c(
   "don't know|refuse|NIU|missi"
)

replacements <- c(
   NA
)

hha_wherecooks_clean <- hhcleanedFunc(working_df_updated
   , var
   , patterns
   , replacements
   , tab_vars
   , legend_title
   , y_limits
)

hha_wherecooks_clean_prop_plot <- hha_wherecooks_clean[["prop_plot"]]
## Objects to report
#hha_wherecooks_issues_count
#hha_wherecooks_issues_plot
#hha_wherecooks_clean_prop_plot

#### ---- 2.10 Main source of cooking fuel used by the household ----

## Issues
var <- "hha_cookingfuel"
pattern <- "don't know|refuse|NIU|missi"
legend_title <- "Cooking fuel"
tab_vars <- c("hha_intvwyear", "hha_slumarea", "hha_cookingfuel")
hha_cookingfuel_issues <- hhaissuesFunc(working_df_updated
   , var
   , pattern
   , tab_vars
   , legend_title
)

hha_cookingfuel_issues_count <- hha_cookingfuel_issues[["issues_counts_html"]]
hha_cookingfuel_issues_plot <- hha_cookingfuel_issues[["issues_count_plot"]]

## Summary of cleaned variable
var <- "hha_cookingfuel"
legend_title <- "Place to cook"
y_limits = c(0, 1)
tab_vars <- c("hha_intvwyear", "hha_slumarea", "hha_cookingfuel_new")
patterns <- c(
   "don't know|refuse|NIU|missi"
)

replacements <- c(
   NA
)

hha_cookingfuel_clean <- hhcleanedFunc(working_df_updated
   , var
   , patterns
   , replacements
   , tab_vars
   , legend_title
   , y_limits
)

hha_cookingfuel_clean_prop_plot <- hha_cookingfuel_clean[["prop_plot"]]
## Objects to report
#hha_cookingfuel_issues_count
hha_cookingfuel_issues_plot
hha_cookingfuel_clean_prop_plot

