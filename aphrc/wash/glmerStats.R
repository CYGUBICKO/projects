#### ---- Project: APHRC Wash Data ----
#### ---- Task: Simulation ----
#### ---- Complex glmer summary ----
#### ---- By: Steve and Jonathan ----
#### ---- Date: 2019 Mar 16 (Sat) ----

load("complexGlmer.rda")

# Incoming objects:
# * complexglmer_list - glmer fits per simulation
# * complexcoef_df - fixed effect coef per simulation
# * betas_df & betas - initial beta values for simulations
# * predictors 

nsims <- length(complexglmer_list)

summary(complexglmer_list[[1]])

