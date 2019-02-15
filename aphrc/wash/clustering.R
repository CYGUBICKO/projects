#### ---- Project: APHRC Wash Data ----
#### ---- Task: Clustering ----
#### ---- By: Steve and Jonathan ----
#### ---- Date: 2019 Feb 13 (Wed) ----

library(tidyr)
library(dplyr)
library(ggplot2)

load("globalFunctions.rda")
load("descriptivePlots.rda")

#Since the clustering data contains both categorical and numeric variables, we can not therefore use the k-modes clustering. K-medoids clustering allows both numeric and categorical data.

#The goal is to find k representative objects which minimize the sum of the dissimilarities of the observations to their closest representative object.

#### ---- Determine k ----
factors <- function(x){
	factor(x, levels = c(0, 1), labels = c("Unimproved", "Improved"))
}

cluster_vars <- colnames(working_df)[!colnames(working_df) %in% c("total_wash_indicators", "wash_access_rate")]

cluster_df <- (working_df
	%>% select(cluster_vars)
	%>% mutate_at(wash_vars, funs(factors))
)

var_levels <- as.data.frame(sapply(cluster_df
		, function(x) length(levels(x))
	)
)
colnames(var_levels) <- "levels"
bin_vars <- row.names(var_levels)[var_levels[["levels"]]==2]
bin_var_pos <-  grep(bin_vars, colnames(cluster_df))

