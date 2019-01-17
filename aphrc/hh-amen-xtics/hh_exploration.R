#### ---- Project: NUHDSS - Household Amenities Characteristics Study ----
#### ---- Task: Data cleaning 
#### ---- Date: 09/01/2019

library(haven)
library(reshape2)
library(dplyr)
library(tibble)
library(ggplot2)
library(tidyr)
library(ztable)
library(ggthemes)
library(scales)
library(DT)

## Set theme
theme_set(theme_minimal())
#theme_set(theme_bw())

#### ---- Define some important functions ----

## Extract codebook

ColAttr <- function(x, attrC, ifIsNull) {
  # Returns column attribute named in attrC, if present, else isNullC.
  atr <- attr(x, attrC, exact = TRUE)
  atr <- if (is.null(atr)) {ifIsNull} else {atr}
  atr
}

AtribLst <- function(df, attrC, isNullC){
  # Returns list of values of the col attribute attrC, if present, else isNullC
  lapply(df, ColAttr, attrC=attrC, ifIsNull=isNullC)
}

## Missingness function

missPropFunc <- function(df){
  n <- nrow(df)
  df <- as.data.frame(df)
  miss_count <- apply(df, 2, function(x) sum(is.na(x)|as.factor(x)==""|grepl("refuse|NIU|missi", x, ignore.case = TRUE)))
  miss_df <- (miss_count
    %>% as_tibble(rownames = NA)
    %>% rownames_to_column("variable")
    %>% rename(miss_count = value)
    %>% mutate(miss_prop = miss_count/n)
    %>% mutate(miss_prop = round(miss_prop, digits = 3) * 100)
  )
}


### ---- Data ----

hh_df <- read_dta("data/NUHDSS_hhamenitiescharacteristics_anon.dta")

# Extract value labels
val_labels <- AtribLst(hh_df, attrC="labels", isNullC=NA)
val_labels <- val_labels[!is.na(val_labels)]
# Extract varaible labels
var_labels <- AtribLst(hh_df, attrC="label", isNullC="")

# Generate codebook from attributes
codebook <- (var_labels 
  %>% melt()
  %>% rename(description = value, variable = L1)
  %>% select(variable, description)
  %>% mutate(description = ifelse(description == ""|is.na(description)
      , as.character(variable)
      , as.character(description)
    )
  )
)

## Convert values to labels
working_df <- (hh_df
          %>% as_factor()
          %>% as_tibble()
)


#### ---- Missingness -----

## Proportion per variable merged with codebook
miss_prop_df <- (working_df 
  %>% missPropFunc()
  %>% left_join(codebook, by = "variable")
  %>% select(variable, description, miss_count, miss_prop)
  %>% arrange(desc(miss_prop))
)

## Drop variables with no data
miss_vars <- (miss_prop_df
  %>% filter(miss_prop==100)
  %>% select(variable)
)
vars_droped <- as.character(miss_vars$variable)

working_df <- (working_df
  %>% select(-c(vars_droped))
)

## Vizualize proportion of missingness
miss_dist <- (miss_prop_df
  %>% filter(miss_prop>0)
)

print(
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


#### ---- Exploration ----

## ID Variables
# Duplicate ID
id_vars <- grep("id", names(working_df), value = TRUE, ignore.case = TRUE)
id_df <- (working_df
  %>% select(id_vars)
  %>% sapply(function(x)sum(duplicated(x) & (!is.na(x)|x!="")))
  %>% as_tibble(rownames = NA)
  %>% rownames_to_column("variable")
  %>% mutate(prop_dup = round(value/nrow(working_df), digits = 3) * 100)
  %>% rename(dup_count = value)
)

varLabs <- function(tab){
  name <- colnames(tab)[!colnames(tab) %in% "variable"]
  lab <- (tab
    %>% mutate(description = codebook$description[codebook$variable %in% variable])
    %>% select(variable, description, name)
  )
}

extractLabs <- function(variable){
  as.character(codebook$description[codebook$variable %in% variable])
}


# Background varibales
### droplevels
### all.equal

propFunc <- function(df = data.frame(), dots = NULL, ...){
  prop_df <- (df
    %>% group_by_(.dots = dots)
    %>% summarise_(n = ~n())
    %>% mutate(prop = prop.table(n))
  )
  prop_df
}


tabsFunc <- function(df = data.frame(), vars = NULL, no_digits = 2, ...){
  # The first oject in vars is the main folowed by next ... such 
  # that var1>var1>var1>......
  # The last object on vars will be used as a binding var, i.e, on the column.

  # Counts wide format
  count_df <- (df
    %>% propFunc(vars)
    %>% select(-prop) 
    %>% spread(last(vars), n)
  )
  
  # Row percentages
  rowperc_df <- (df
    %>% propFunc(vars)
    %>% mutate(Percent = round(prop*100, no_digits))
    %>% select(-n, -prop) 
    %>% spread(last(vars), Percent)
  )
  
  # Column percentages
  levs <- levels(factor(df[[last(vars)]]))
  all_vars <- c(first(vars), levs)
  colperc_df <- count_df
  colperc_df[, all_vars] <- (colperc_df[, all_vars]
    %>% group_by_(first(vars))
    %>% mutate_at(levs, function(i){round((i/sum(i)) * 100, no_digits)})
  )
  
  # Save long format of column percentages for ploting
  colperc_long_df <- (rowperc_df
    %>% gather("var", "Percent", levs)
    %>% rename_(.dots = setNames("var", last(vars)))
  )

  out <- list(count_df = count_df
    , rowperc_df = rowperc_df
    , colperc_df = colperc_df
    , colperc_long_df = colperc_long_df
    , levs = levs
  )
  return(out)
}

### Format tabs
formatTabs <- function(tabin
   , unit = "%", levs
   , no_digits = 2
   , col_label = NULL
   , cap = NULL){
  # levs object is from the tab function
  
  if (!unit %in% c("%", "n")){
    stop("Unit is either % or n")
  }
  ztab <- (tabin
    %>% as.data.frame()
    %>% ztable(digits = no_digits
    , caption = cap
    )
  )
  if (unit %in% "%"){
    ztab <- (ztab 
    %>% addcgroup(cgroup = c('', paste0(col_label, " (%)"))
      , n.cgroup = c(length(levs), length(levs))
      )
    )
  }
  if (unit %in% "n"){
    ztab <- (ztab 
      %>% addcgroup(cgroup = c('', paste0(col_label, " (n)"))
       , n.cgroup = c(length(levs), length(levs))
      )
    )
  }
  ztab
}


### Background information 
# 1.7. Number of rooms used
hha_sleeprooms_thisstructure_issues <- grep("other|refuse|NIU|missi"
                                            , working_df$hha_sleeprooms_thisstructure
                                            , value = TRUE
                                            )
hha_sleeprooms_thisstructure_issues_df <- (working_df
  %>% filter(hha_sleeprooms_thisstructure %in% c(hha_sleeprooms_thisstructure_issues, 0))
)
vars <- c("hha_intvwyear", "hha_slumarea", "hha_sleeprooms_thisstructure")
hha_sleeprooms_thisstructure_issues_tab <- (hha_sleeprooms_thisstructure_issues_df
  %>% tabsFunc(vars = vars)
)

hha_sleeprooms_thisstructure_labs <- extractLabs("hha_sleeprooms_thisstructure")
hha_sleeprooms_thisstructure_issues_counts <- hha_sleeprooms_thisstructure_issues_tab$count_df
tt <- formatTabs(hha_sleeprooms_thisstructure_issues_counts
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


### ---- Total number of rentable houses -----

hha_rentablerooms_total_issues <- grep("other|refuse|NIU|missi"
                                       , working_df_updated$hha_rentablerooms_total
                                       , value = TRUE
)
hha_rentablerooms_total_issues_df <- (working_df_updated
                                      %>% filter(hha_rentablerooms_total %in% hha_rentablerooms_total_issues)
)
vars <- c("hha_intvwyear", "hha_rentablerooms_total", "hha_slumarea")
hha_rentablerooms_total_issues_tab <- (hha_rentablerooms_total_issues_df
                                       %>% tabsFunc(vars = vars)
)

hha_rentablerooms_total_labs <- extractLabs("hha_rentablerooms_total")
hha_rentablerooms_total_issues_counts <- hha_rentablerooms_total_issues_tab$count_df
hha_rentablerooms_total_issues_counts_html <- formatTabs(hha_rentablerooms_total_issues_counts
                 , levs = hha_rentablerooms_total_issues_tab$levs
                 , unit = "n"
                 , col_label = hha_rentablerooms_total_labs
                 , no_digits = 2
)

# Plot
q1_10_plot_df <- propFunc(hha_rentablerooms_total_issues_df, vars)
hha_rentablerooms_total_count_plot <- (
  ggplot(q1_10_plot_df, aes(x = hha_intvwyear
                           , y = n
                           , group = hha_rentablerooms_total
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
vars <- c("hha_intvwyear", "hha_numpeople_total", "hha_slumarea")
hha_numpeople_total_issues_tab <- (hha_numpeople_total_issues_df
                                   %>% tabsFunc(vars = vars)
)

hha_numpeople_total_labs <- extractLabs("hha_numpeople_total")
hha_numpeople_total_issues_counts <- hha_numpeople_total_issues_tab$count_df
hha_numpeople_total_issues_counts_html <- formatTabs(hha_numpeople_total_issues_counts
                                                     , levs = hha_numpeople_total_issues_tab$levs
                                                     , unit = "n"
                                                     , col_label = hha_numpeople_total_labs
                                                     , no_digits = 2
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



# Recode the categories to fewer ones

recodeLabs <- function(df, var, pattern, replacement, insert = TRUE){
  if (insert){
    new_var <- paste0(var, "_new")
    df[, new_var] <- df[, var]
    for (p in 1:length(patterns)){
      df[, new_var] <- apply(df[, new_var], 2
        , function(x){ifelse(grepl(patterns[[p]], x), replacements[[p]], x)})
    }
  } else{
      for (p in 1:length(patterns)){
        df[, var] <- apply(df[, var], 2
          , function(x){ifelse(grepl(patterns[[p]], x), replacements[[p]], x)})
    }
  }
  return(df)
}

## Extract issues function
extractIssues <- function(df, var, pattern, tab_vars){
  vals <- pull(df, var)
  issues <- grep(pattern, vals, value = TRUE, ignore.case = TRUE)
  issues_df <- (df
    %>% filter(.data[[var]] %in% issues)
  )
  issues_tab <- (issues_df
    %>% tabsFunc(vars = tab_vars)
  )
  return(
    list(issues_df = issues_df, issues_tab = issues_tab)
  )
}

### ----- Source of Water -----
## Source of drinking water issues
var <- "hha_drinkwatersource"
pattern <- "don't know|refuse|NIU|missi"
tab_vars <- c("hha_intvwyear", "hha_slumarea", "hha_drinkwatersource")
hha_drinkwatersource_issues_summary <- (working_df_updated
  %>% extractIssues(var, pattern, tab_vars)
)

# Summarize issues
hha_drinkwatersource_issues_tab <-  hha_drinkwatersource_issues_summary$issues_tab
hha_drinkwatersource_labs <- extractLabs("hha_drinkwatersource")
hha_drinkwatersource_issues_counts <- hha_drinkwatersource_issues_tab$count_df
hha_drinkwatersource_issues_counts_html <- datatable(hha_drinkwatersource_issues_counts
  , caption = hha_drinkwatersource_labs
)

# Issue summary plot function
hha_drinkwatersource_issues_df <- hha_drinkwatersource_issues_summary$issues_df
hha_drinkwatersource_issue_plot_df <- propFunc(hha_drinkwatersource_issues_df, tab_vars)

q2_1_issues_count_plot <- (ggplot(hha_drinkwatersource_issue_plot_df
              , aes(x = hha_intvwyear
                , y = n
                , group = hha_drinkwatersource
                , color = hha_drinkwatersource
                )
    )
    + geom_line(linetype = 2
                , size = 1
    )
    + geom_point(aes(y = n))
    + labs(title = hha_drinkwatersource_labs
           , x = "Years"
           , y = "# of cases"
    )
    + guides(color = guide_legend(title = "Sources of drinking water"))
    + theme(plot.title = element_text(hjust = 0.5))
    + facet_wrap(~hha_slumarea)
    
)


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

## Summary of 2.1 without issues
working_df_updated <- (working_df_updated
  %>% recodeLabs(var, patterns, replacements)
)

# Update codebook
codebook_update <- data.frame(variable = var
  , description = paste0(hha_drinkwatersource_labs, "(new)")
)
codebook <- rbind(codebook, codebook_update)

tab_vars <- c("hha_intvwyear", "hha_slumarea", "hha_drinkwatersource_new")
hha_drinkwatersource_summary <- (working_df_updated
  %>% propFunc(tab_vars)
  %>% drop_na()
)

hha_drinkwatersource_prop_plot <- (
  ggplot(hha_drinkwatersource_summary, aes(x = hha_intvwyear
                                          , y = prop
                                          , group = hha_drinkwatersource_new
                                          , color = hha_drinkwatersource_new
  )
  )
  + geom_line(linetype = 2
              , size = 1
  )
  + geom_point(aes(y = prop))
  + labs(title = extractLabs("hha_drinkwatersource_new")
         , x = "Years"
         , y = "Mean"
  )
  + scale_y_continuous(labels = percent, limits = c(0,1))
  + guides(color = guide_legend(title = "Sources of drinking water"))
  + theme(plot.title = element_text(hjust = 0.5))
  + facet_wrap(~hha_slumarea)
)





#########

### ----- Toilet facility 2-4 years -----

## Issues
var <- "hha_toilet_2to4yrs"
pattern <- "don't know|refuse|NIU|missi|no members"
tab_vars <- c("hha_intvwyear", "hha_slumarea", "hha_toilet_2to4yrs")

hhaissuesFunc <- function(df, var, pattern, tab_vars, legend_title){
  # Issues
  issues_summary <- (df
     %>% extractIssues(var, pattern, tab_vars)
  )
  
  # Summarize issues
  issues_tab <- issue_summary$issues_tab
  issue_labs <- extractLabs(var)
  issues_counts <- issues_tab$count_df
  issues_counts_html <- datatable(issues_counts
     , caption = issue_labs
  )
  
  # Issue summary plot function
  issues_df <- issues_summary$issues_df
  issues_plot_df <- (issues_df
    %>% propFunc(tab_vars)
    %>% rename_(new_var = var)
  )
  
  issues_count_plot <- (ggplot(issues_plot_df
     , aes(x = hha_intvwyear
        , y = n
        , group = new_var
        , color = new_var
        )
     )
     + geom_line(linetype = 2
        , size = 1
     )
     + geom_point(aes(y = n))
     + labs(title = issue_labs
        , x = "Years"
        , y = "# of cases"
     )
     + guides(color = guide_legend(title = legend_title))
     + theme(plot.title = element_text(hjust = 0.5)
           , legend.position = 'bottom'
           , axis.ticks = element_blank()
           )
     + facet_wrap(~hha_slumarea)
  )
  return(
    list(issues_counts_html = issues_counts_html
         , issues_count_plot = issues_count_plot
    )
  )
}

hha_toilet_2to4yrs_issues <- hhaissuesFunc(working_df_updated, var, pattern, tab_vars, "Toilet facilities")



## Collapse sources of drinking water to fewer categories
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

## Summary of 2.1 without issues
working_df_updated <- (working_df_updated
   %>% recodeLabs(var, patterns, replacements)
)

# Update codebook
codebook_update <- data.frame(variable = var
   , description = paste0(hha_toilet_2to4yrs_labs, "(new)")
)
codebook <- rbind(codebook, codebook_update)

tab_vars <- c("hha_intvwyear", "hha_slumarea", "hha_toilet_2to4yrs_new")
hha_toilet_2to4yrs_summary <- (working_df_updated
   %>% propFunc(tab_vars)
   %>% drop_na()
)

hha_toilet_2to4yrs_prop_plot <- (ggplot(hha_toilet_2to4yrs_summary
   , aes(x = hha_intvwyear
      , y = prop
      , group = hha_toilet_2to4yrs_new
      , color = hha_toilet_2to4yrs_new
      )
   )
   + geom_line(linetype = 2
      , size = 1
   )
   + geom_point(aes(y = prop))
   + labs(title = extractLabs("hha_toilet_2to4yrs_new")
      , x = "Years"
      , y = "Proportion"
   )
   + scale_y_continuous(labels = percent, limits = c(0,0.15))
   + guides(color = guide_legend(title = "Toilet facilities < 5 yrs"))
   + theme(plot.title = element_text(hjust = 0.5)
           , legend.position = 'bottom'
           , axis.ticks = element_blank()
           )
   + facet_wrap(~hha_slumarea)
)

hhcleanedFunc <- function(df, var, patterns, replacements
  , tab_vars
  , legend_title
  , y_limits = c(0,1)
  ){
  working_df_updated <- (df
     %>% recodeLabs(var, patterns, replacements)
  )
  
  # Update codebook
  issue_labs <- extractLabs(var)
  codebook_update <- data.frame(variable = var
     , description = paste0(issue_labs, "(new)")
  )
  codebook <- rbind(codebook, codebook_update)
  summary_df <- (working_df_updated
     %>% propFunc(tab_vars)
     %>% drop_na()
     %>% rename_(new_var = paste0(var, "_new"))
  )
  prop_plot <- (ggplot(summary_df
     , aes(x = hha_intvwyear
        , y = prop
        , group = new_var
        , color = new_var
        )
     )
     + geom_line(linetype = 2
        , size = 1
     )
     + geom_point(aes(y = prop))
     + labs(title = issue_labs
        , x = "Years"
        , y = "Proportion"
     )
     + scale_y_continuous(labels = percent, limits = y_limits)
     + guides(color = guide_legend(title = legend_title))
     + theme(plot.title = element_text(hjust = 0.5)
             , legend.position = 'bottom'
             , axis.ticks = element_blank()
             )
     + facet_wrap(~hha_slumarea)
  )
  return(
    list(
      working_df_updated = working_df_updated
      , codebook = codebook
      , prop_plot = prop_plot
    )
  )
}

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


hha_drinkwatersource_issues_count
hha_toilet_2to4yrs_issues_plot
hha_toilet_2to4yrs_clean_prop_plot