#### ---- Project: APHRC Wash Data ----
#### ---- Task: Modelling ----
#### ---- Sub-task: glmer ----
#### ---- By: Steve and Jonathan ----
#### ---- Date: 2019 Feb 10 (Sun) ----

library(tidyr)
library(dplyr)
library(ggplot2)
library(lme4)
library(compiler)
library(parallel)
library(boot)

load("descriptives.rda")

# Mixed effect logistic regression using glmer

working_df <- (working_df 
	%>% mutate(cat_wash = relevel(cat_wash, ref = "Unimproved"))
)

start_time <- Sys.time()

wash_gmler_model <- glmer(cat_wash ~ ageyears + gender + ethnicity + numpeople_total + hhdhungerscale + wealthquintile + expend_total_USD_per_centered + (1 | hhid_anon/slumarea/intvwyear)
	, data = working_df
	, family = binomial
	, control = glmerControl(optimizer = "bobyqa")
)

time_elapsed <- Sys.time() - start_time
print(time_elapsed)

save(file = "glmmmModel.rda"
	, working_df
	, codebook
	, wash_gmler_model
)
