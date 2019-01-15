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
  miss_count <- apply(df, 2, function(x) sum(is.na(x)|as.factor(x)==""))
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
  # The first oject in vars is the key column folowed by next ... such 
  # that var1>var1>var1>......
  # The last object on vars will be used as a binding var, i.e, on the raw.

  # Counts
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









## Collapse other ethnicity to others

working_df <- (working_df
  %>% mutate(hha_hhh_ethnicity_new = gsub("other.*"
      , "others"
      , hha_hhh_ethnicity)
    )
)

tt <- working_df %>% propFunc(c("hha_intvwyear", "hha_slumarea", "hha_hhh_ethnicity_new"))

tt <- working_df %>% propFunc(c("hha_intvwyear", "hha_slumarea", "hha_hhh_ethnicity_new"))


head(tt)

print(ggplot(tt , aes(x=hha_intvwyear, y= prop,group=hha_hhh_ethnicity_new,color=hha_hhh_ethnicity_new))
+ geom_line(linetype = 2
            , size = 2
            )
+ geom_po
+  scale_y_continuous(labels = percent, limits = c(0,0.5))
+ scale_fill_few("Medium")
+ facet_wrap(~hha_slumarea)
)


#################################################################

