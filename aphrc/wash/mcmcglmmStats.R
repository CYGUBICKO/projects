#### ---- Project: APHRC Wash Data ----
#### ---- Task: Simulation ----
#### ---- MCMCMglmr summary ----
#### ---- By: Steve and Jonathan ----
#### ---- Date: 2019 Apr 03 (Wed) ----

library(MCMCglmm)

#load("complexGlmer.rda")
load("multiMcmcglmm.rda")

# Incoming objects:
# * complexglmer_list - glmer fits per simulation
# * complexcoef_df - fixed effect coef per simulation
# * betas_df & betas - initial beta values for simulations
# * predictors 

nsims <- length(multimcmcglmm_list)

for (s in 1:nsims){
	print(summary(multimcmcglmm_list[[s]]))
}

