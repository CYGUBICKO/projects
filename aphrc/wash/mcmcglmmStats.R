#### ---- Project: APHRC Wash Data ----
#### ---- Task: Simulation ----
#### ---- MCMCMglmr summary ----
#### ---- By: Steve and Jonathan ----
#### ---- Date: 2019 Apr 03 (Wed) ----

#load("complexGlmer.rda")
load("binaryMcmcglmm.rda")

# Incoming objects:
# * complexglmer_list - glmer fits per simulation
# * complexcoef_df - fixed effect coef per simulation
# * betas_df & betas - initial beta values for simulations
# * predictors 

nsims <- length(mcmcglmm_list)

for (s in 1:nsims){
	print(summary(mcmcglmm_list[[s]]))
}

