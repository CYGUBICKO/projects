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

load("logisticpca.rda")

# Mixed effect logistic regression using glmer
working_df <- (working_df 
	%>% mutate_at("intvwyear", as.numeric)
)

start_time <- Sys.time()

wash_gmler_model <- glmer(cat_wash_num ~ ageyears + gender + ethnicity + numpeople_total + hhdhungerscale + isbelowpovertyline + wealthquintile + expend_total_USD_per_centered + slumarea + intvwyear + (1 + intvwyear|hhid_anon)
	, data = working_df
	, family = binomial
	, control = glmerControl(optimizer = "bobyqa")
)

time_elapsed <- Sys.time() - start_time
print(time_elapsed)

save(file = "glmmModel.rda"
	, working_df
	, codebook
	, wash_gmler_model
)
