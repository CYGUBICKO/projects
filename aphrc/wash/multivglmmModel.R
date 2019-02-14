#### ---- Project: APHRC Wash Data ----
#### ---- Task: Modelling ----
#### ---- Sub-task: multivariate glmer ----
#### ---- By: Steve and Jonathan ----
#### ---- Date: 2019 Feb 10 (Sun) ----

library(tidyr)
library(dplyr)
library(ggplot2)
library(lme4)
library(compiler)
library(parallel)
library(boot)

load("globalFunctions.rda")
load("analysisdata.rda")


# Melt the data along the 3 wash variables. Then fit a psedu-multivariate glmm.

patterns <- c("watersource", "toilettype", "garbagedis")
replacements <- c("Water source", "Toilet type", "Garbage disposal")
working_df <- (working_df 
	%>% gather(wash_variable, wash_variable_value, wash_vars)
	%>% recodeLabs("wash_variable", patterns, replacements, insert = FALSE)
	%>% mutate_at("wash_variable", factor)
	%>% mutate_at("intvwyear", as.numeric)
)

# Multi-variate GLMM

start_time <- Sys.time()

wash_multivgmler_model <- glmer(wash_variable_value ~ (ageyears + gender + ethnicity + numpeople_total + hhdhungerscale + isbelowpovertyline + wealthquintile + expend_total_USD_per_centered) * wash_variable + slumarea + intvwyear + (1 + intvwyear|hhid_anon:wash_variable)
	, data = working_df
	, family = binomial
	, control = glmerControl(optimizer = "bobyqa")
)

time_elapsed <- Sys.time() - start_time
print(time_elapsed)

save(file = "multivglmmModel.rda"
	, working_df
	, codebook
	, wash_multivgmler_model
)
