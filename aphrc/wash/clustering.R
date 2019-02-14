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

factor_vars <- c("hhid_anon"
	, "slumarea", "gender", "ethnicity", "isbelowpovertyline"
)
ordered_factors_vars <- c("hhdhungerscale"
	, "wealthquintile", "cat_hhwatersource"
	, "cat_hhtoilettype", "cat_hhgarbagedisposal"
)
numeric_vars <- c("intvwyear"
	, "ageyears", "numpeople_total"
	, "expend_total_USD_per_centered"
)

cluster_df <- select
