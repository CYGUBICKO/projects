#### ---- Project: NUHDSS - Household Amenities Characteristics Study ----
#### ---- Cygu et al., see https://github...
#### ---- Task: Data cleaning 
#### ---- Date: 2019 Jan 17 (Thu)

rm(list = ls())

#library(RcmdrMisc)
library(memisc)
library(haven)
library(reshape2)
library(dplyr)
library(tibble)
library(tidyr)
library(openxlsx)
library(DT)
library(ggplot2)
library(scales)


theme_set(theme_bw())

##### ---- Load the functions ----
source("globalFunctions.R")
source("hhamenitiesFunc.R")
source("tabFunctions.R")

#### ---- Load dataset ----
# Uses loadData function (loadData.R)

df_name <- "NUHDSS_hhamenitiescharacteristics_anon"
file_extension <- "dta"
df_folder <- "data"
df_outname <- "hh_working_df"

load_df <- loadData(df_name
   , file_extension
   , df_folder
   , df_outname
   , re_run = FALSE
)

working_df <- load_df[["working_df"]]
codebook <- load_df[["codebook"]]
rm("load_df", "df_name", "file_extension", "df_folder", "df_outname")

#### ---- Missingness -----
# Uses missProFunc and drops vars with 100% empty entries (workingDF.R)

## Proportion per variable merged with codebook
miss_prop_df <- (working_df
   %>% missPropFunc()
   %>% left_join(codebook, by = "variable")
   %>% select(variable, description, miss_count, miss_prop)
   %>% arrange(desc(miss_prop))
)


## Formated output
miss_prop_df_html <- datatable(miss_prop_df)

## Drop variables with no data
miss_vars <- (miss_prop_df
  %>% filter(miss_prop==100)
  %>% select(variable)
)

vars_droped <- as.character(miss_vars$variable)
no_vars_droped <- length(vars_droped)

working_df <- (working_df
  %>% select(-c(vars_droped))
)

## Objects to report
# miss_prop_df_html
# no_vars_droped

## Vizualize proportion of missingness
miss_dist <- (miss_prop_df
  %>% filter(miss_prop>0)
)

miss_dist_plot <- (
   ggplot(miss_dist, aes(x = reorder(description, -miss_prop)
      , y = miss_prop, label = miss_prop))
      + geom_point(stat = "identity"
         , fill = "black"
         , size = 6
         )
      + geom_segment(aes(y = 0
            , x = description
            , yend = miss_prop
            , xend = description)
            , color = "black"
         )
      + geom_text(color="white", size=2)
      + coord_flip()
      + labs(x = "Variables"
            , y = "% Missingness"
         )
)

## Objects to report
# miss_dist_plot

## Remove uncessary objects
rm(list = ls()[!ls() %in% c(
         "working_df"
         , "codebook"
         # Functions
         , "groupSummary"
         , "hhmeanFunc"
         , "missPropFunc"
         , "saveXlsx"
         , "varLabs"
         , "extractLabs"
         , "propFunc"
         , "tabsFunc"
         , "recodeLabs"
         , "extractIssues"
         , "hhaissuesFunc"
         , "hhcleanedFunc"
         # workingDf and missProp
         , "miss_prop_df_html"
         , "no_vars_droped"
         , "file_prefix"
      )
   ]
)

#### ---- Data Exploration ----

## Duplicate IDs

# Duplicate IDs
id_vars <- grep("id", names(working_df), value = TRUE, ignore.case = TRUE)
id_df <- (working_df
   %>% select(id_vars)
   %>% sapply(function(x)sum(duplicated(x) & (!is.na(x)|x!="")))
   %>% enframe(name = "variable")
   %>% mutate(prop_dup = round(value/nrow(working_df), digits = 3) * 100)
   %>% rename(dup_count = value)
)
      
id_dup_dis <- (id_df
   %>% varLabs()
   %>% as.data.frame()
)           

id_dup_dis <- datatable(id_dup_dis)

## Objects to report
# id_dup_dis
         
# Keep necessary files only
rm(list = ls()[!ls() %in% c("working_df"
         , "codebook"
         # Functions
         , "groupSummary"
         , "hhmeanFunc"
         , "missPropFunc"
         , "saveXlsx"
         , "varLabs"
         , "extractLabs"
         , "propFunc"
         , "tabsFunc"
         , "recodeLabs"
         , "extractIssues"
         , "hhaissuesFunc"
         , "hhcleanedFunc"
         # missProp.Rout
         , "miss_prop_df_html"
         , "no_vars_droped"
         , "file_prefix"
         # idVars.Rout
         , "id_dup_dis"
      )
   ]
)


# Duplicate IDs
id_vars <- grep("id", names(working_df), value = TRUE, ignore.case =     
+ TRUE)
id_df <- (working_df
   %>% select(id_vars)
   %>% sapply(function(x)sum(duplicated(x) & (!is.na(x)|x!="")))
   %>% enframe(name = "variable")
   %>% mutate(prop_dup = round(value/nrow(working_df), digits = 3) *     
+ 100)
   %>% rename(dup_count = value)
)
      
id_dup_dis <- (id_df
   %>% varLabs()
   %>% as.data.frame()
)           

id_dup_dis <- datatable(id_dup_dis)

## Objects to report
# id_dup_dis
         
# Keep necessary files only

rm(list = ls()[!ls() %in% c("working_df"
         , "codebook"
         # Functions
         , "groupSummary"
         , "hhmeanFunc"
         , "missPropFunc"
         , "saveXlsx"
         , "varLabs"
         , "extractLabs"
         , "propFunc"
         , "tabsFunc"
         , "recodeLabs"
         , "extractIssues"
         , "hhaissuesFunc"
         , "hhcleanedFunc"
         # missProp.Rout
         , "miss_prop_df_html"
         , "no_vars_droped"
         , "file_prefix"
         # idVars.Rout
         , "id_dup_dis"
      )
   ]
)

################## BACKGROUND INFORMATION ##########################

#### ---- 1.7. Number of rooms used ----
issueText <- "other|refuse|NIU|missi"
hha_sleeprooms_total_issues <- grep("other|refuse|NIU|missi"
   , working_df$hha_sleeprooms_total
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


#### ---- Total number of rentable houses -----

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

#### ---- Total number of people in this household -----

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
#hha_sleeprooms_total_mean_plot

#hha_rentablerooms_total_issues_counts_html
#hha_rentablerooms_total_count_plot
#hha_rentablerooms_total_mean_plot

#hha_numpeople_total_issues_counts_html
#hha_numpeople_total_count_plot
#hha_numpeople_total_mean_plot

rm(list = ls()[!ls() %in% c(
         "working_df_updated"
         , "codebook"
         # Functions
         , "groupSummary"
         , "hhmeanFunc"
         , "missPropFunc"
         , "saveXlsx"
         , "varLabs"
         , "extractLabs"
         , "propFunc"
         , "tabsFunc"
         , "recodeLabs"
         , "extractIssues"
         , "hhaissuesFunc"
         , "hhcleanedFunc"
         # missProp.Rout
         , "miss_prop_df_html"
         , "no_vars_droped"
         , "file_prefix"
         # idVars.Rout
         , "id_dup_dis"
         # backgroundSummary
         , "hha_sleeprooms_total_issues_counts_html"
         , "hha_sleeprooms_total_count_plot"
         , "hha_sleeprooms_total_mean_plot"
         , "hha_rentablerooms_total_issues_counts_html"
         , "hha_rentablerooms_total_count_plot"
         , "hha_rentablerooms_total_mean_plot"
         , "hha_numpeople_total_issues_counts_html"
         , "hha_numpeople_total_count_plot"
         , "hha_numpeople_total_mean_plot"
      )
   ]
)


################ SECTION TWO #############################################

#### ----- 2.1 Source of Water -----
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
## Objects to report
#hha_drinkwatersource_issues_count
#hha_drinkwatersource_issues_plot
#hha_drinkwatersource_clean_prop_plot
#hha_toilet_2to4yrs_issues_count
#hha_toilet_2to4yrs_issues_plot
#hha_toilet_2to4yrs_clean_prop_plot

rm(list = ls()[!ls() %in% c(
         "working_df_updated"
         , "codebook"
         # Functions
         , "groupSummary"
         , "hhmeanFunc"
         , "missPropFunc"
         , "saveXlsx"
         , "varLabs"
         , "extractLabs"
         , "propFunc"
         , "tabsFunc"
         , "recodeLabs"
         , "extractIssues"
         , "hhaissuesFunc"
         , "hhcleanedFunc"
         # missProp.Rout
         , "miss_prop_df_html"
         , "no_vars_droped"
         , "file_prefix"
         # idVars.Rout
         , "id_dup_dis"
         # backgroundSummary
         , "hha_sleeprooms_total_issues_counts_html"
         , "hha_sleeprooms_total_count_plot"
         , "hha_sleeprooms_total_mean_plot"
         , "hha_rentablerooms_total_issues_counts_html"
         , "hha_rentablerooms_total_count_plot"
         , "hha_rentablerooms_total_mean_plot"
         , "hha_numpeople_total_issues_counts_html"
         , "hha_numpeople_total_count_plot"
         , "hha_numpeople_total_mean_plot"
         # hhamenitiesSummary
         , "hha_drinkwatersource_issues_count"
         , "hha_drinkwatersource_issues_plot"
         , "hha_drinkwatersource_clean_prop_plot"
         , "hha_toilet_2to4yrs_issues_count"
         , "hha_toilet_2to4yrs_issues_plot"
         , "hha_toilet_2to4yrs_clean_prop_plot"
      )
   ]
)


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
#hha_cookingfuel_issues_plot
#hha_cookingfuel_clean_prop_plot


#### ---- 2.14 Who does most cooking in the household ----

## Issues
var <- "hha_whocooks"
pattern <- "don't know|refuse|NIU|missi"
legend_title <- "Who cooks"
tab_vars <- c("hha_intvwyear", "hha_slumarea", "hha_whocooks")
hha_whocooks_issues <- hhaissuesFunc(working_df_updated
   , var
   , pattern
   , tab_vars
   , legend_title
)

hha_whocooks_issues_count <- hha_whocooks_issues[["issues_counts_html"]]
hha_whocooks_issues_plot <- hha_whocooks_issues[["issues_count_plot"]]

## Summary of cleaned variable
var <- "hha_whocooks"
legend_title <- "Who cooks"
y_limits = c(0, 1)
tab_vars <- c("hha_intvwyear", "hha_slumarea", "hha_whocooks_new")
patterns <- c(
   "don't know|refuse|NIU|missi"
)

replacements <- c(
   NA
)

hha_whocooks_clean <- hhcleanedFunc(working_df_updated
   , var
   , patterns
   , replacements
   , tab_vars
   , legend_title
   , y_limits
)

hha_whocooks_clean_prop_plot <- hha_whocooks_clean[["prop_plot"]]
## Objects to report
#hha_whocooks_issues_count
#hha_whocooks_issues_plot
#hha_whocooks_clean_prop_plot


#### ---- 2.15 Average hours of cooking ----

## Issues
var <- "hha_howlongcooks"
pattern <- "don't know|refuse|NIU|missi"
legend_title <- "Hours of cooking"
tab_vars <- c("hha_intvwyear", "hha_slumarea", "hha_howlongcooks")
hha_howlongcooks_issues <- hhaissuesFunc(working_df_updated
   , var
   , pattern
   , tab_vars
   , legend_title
)

hha_howlongcooks_issues_count <- hha_howlongcooks_issues[["issues_counts_html"]]
hha_howlongcooks_issues_plot <- hha_howlongcooks_issues[["issues_count_plot"]]

## Summary of cleaned variable
var <- "hha_howlongcooks"
legend_title <- "Hours of cooking"
tab_vars <- c("hha_intvwyear", "hha_slumarea")
patterns <- c(
   "don't know|refuse|NIU|missi"
)

replacements <- c(
   NA
)

hha_howlongcooks_clean <- hhmeanFunc(working_df_updated
   , var
   , patterns
   , replacements
   , tab_vars
   , legend_title
)

hha_howlongcooks_clean_mean_plot <- hha_howlongcooks_clean[["mean_plot"]]
## Objects to report
#hha_howlongcooks_issues_count
#hha_howlongcooks_issues_plot
#hha_howlongcooks_clean_mean_plot


#### ---- 2.16 Household member ever smoked ----

## Issues
var <- "hha_membereversmoked"
pattern <- "don't know|refuse|NIU|missi"
legend_title <- "Ever Smoked"
tab_vars <- c("hha_intvwyear", "hha_slumarea", "hha_membereversmoked")
hha_membereversmoked_issues <- hhaissuesFunc(working_df_updated
   , var
   , pattern
   , tab_vars
   , legend_title
)

hha_membereversmoked_issues_count <- hha_membereversmoked_issues[["issues_counts_html"]]
hha_membereversmoked_issues_plot <- hha_membereversmoked_issues[["issues_count_plot"]]

## Summary of cleaned variable
var <- "hha_membereversmoked"
legend_title <- "Ever smoked"
y_limits = c(0, 0.75)
tab_vars <- c("hha_intvwyear", "hha_slumarea", "hha_membereversmoked_new")
patterns <- c(
   "don't know|refuse|NIU|missi"
)

replacements <- c(
   NA
)

hha_membereversmoked_clean <- hhcleanedFunc(working_df_updated
   , var
   , patterns
   , replacements
   , tab_vars
   , legend_title
   , y_limits
)

hha_membereversmoked_clean_prop_plot <- hha_membereversmoked_clean[["prop_plot"]]
working_df_updated <- hha_membereversmoked_clean[["working_df_updated"]]
## Objects to report
#hha_membereversmoked_issues_count
#hha_membereversmoked_issues_plot
#hha_membereversmoked_clean_prop_plot


#### ---- 2.17 Household member currently smoking ----

## Issues
var <- "hha_membercurrsmokes"
pattern <- "don't know|refuse|NIU|missi"
legend_title <- "Currently smoking"
tab_vars <- c("hha_intvwyear", "hha_slumarea", "hha_membercurrsmokes")
hha_membercurrsmokes_issues <- hhaissuesFunc(working_df_updated
   , var
   , pattern
   , tab_vars
   , legend_title
)

hha_membercurrsmokes_issues_count <- hha_membercurrsmokes_issues[["issues_counts_html"]]
hha_membercurrsmokes_issues_plot <- hha_membercurrsmokes_issues[["issues_count_plot"]]

## Summary of cleaned variable
var <- "hha_membercurrsmokes"
legend_title <- "Currently smoking"
y_limits = c(0, 0.75)
tab_vars <- c("hha_intvwyear", "hha_slumarea", "hha_membercurrsmokes_new")
patterns <- c(
   "don't know|refuse|NIU|missi"
)

replacements <- c(
   NA
)

hha_membercurrsmokes_clean <- hhcleanedFunc(working_df_updated
   , var
   , patterns
   , replacements
   , tab_vars
   , legend_title
   , y_limits
)

hha_membercurrsmokes_clean_prop_plot <- hha_membercurrsmokes_clean[["prop_plot"]]
working_df_updated <- hha_membercurrsmokes_clean[["working_df_updated"]]
## Objects to report
#hha_membercurrsmokes_issues_count
#hha_membercurrsmokes_issues_plot
#hha_membercurrsmokes_clean_prop_plot


#### ---- 2.18 Average number of smokers ----

## Issues
var <- "hha_numsmokers"
pattern <- "don't know|refuse|NIU|missi"
legend_title <- "Hours of cooking"
tab_vars <- c("hha_intvwyear", "hha_slumarea", "hha_numsmokers")
hha_numsmokers_issues <- hhaissuesFunc(working_df_updated
   , var
   , pattern
   , tab_vars
   , legend_title
)

hha_numsmokers_issues_count <- hha_numsmokers_issues[["issues_counts_html"]]
hha_numsmokers_issues_plot <- hha_numsmokers_issues[["issues_count_plot"]]

## Summary of cleaned variable
var <- "hha_numsmokers"
legend_title <- "Hours of cooking"
tab_vars <- c("hha_intvwyear", "hha_slumarea")
patterns <- c(
   "don't know|refuse|NIU|missi"
)

replacements <- c(
   NA
)

hha_numsmokers_clean <- hhmeanFunc(working_df_updated
   , var
   , patterns
   , replacements
   , tab_vars
   , legend_title
)

hha_numsmokers_clean_mean_plot <- hha_numsmokers_clean[["mean_plot"]]
working_df_updated <- hha_numsmokers_clean[["working_df_updated"]]
## Objects to report
#hha_numsmokers_issues_count
#hha_numsmokers_issues_plot
#hha_numsmokers_clean_mean_plot


#### ---- 2.19 What type of tobacco did/do the members of your household smoke ----

## Group rekated variables
smoked_vars <- grep("_smoke_", names(working_df_updated), value = T)
labs1 <- c("Filtered", "Unfiltered", "Roll", "Pipe", "Other")

patterns <- c(
   "don't know|refuse|NIU|missi"
)

replacements <- c(
   NA
)

tab_vars <- c("hha_intvwyear", "hha_slumarea")
labs2 <- c("Year", "Slum")
labels <- c(labs1, labs2)
## Summary - Count
hha_smoke_clean <- groupSummary(df = working_df_updated
  , grouped_vars = smoked_vars
  , tab_vars = tab_vars
  , patterns = patterns
  , replacements = replacements
  , labels = labels
)
working_df_updated <- hha_smoke_clean[["working_df_updated"]]
hha_smoke_show_html <- hha_smoke_clean[["tabs"]]
codebook <- hha_smoke_clean[["codebook"]]
#show_html(hha_smoke_show_html)

## Count the number tobacco smoked
smoked_vars_new <- paste0(smoked_vars, "_new")
msg_services_count <-apply(working_df_updated,1,
                                             function(x) sum(!is.na(x[smoked_vars_new]) & x[smoked_vars_new]=="yes"))


# Distribution
knitr::kable(india_df %>%
  group_by(msg_services_count) %>% 
    count() %>% ungroup() %>%
  mutate(percentage = (n/sum(n))*100))
