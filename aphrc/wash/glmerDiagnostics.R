#### ---- Project: APHRC Wash Data ----
#### ---- Task: Simulation ----
#### ---- Complex Diagnostics ----
#### ---- By: Steve and Jonathan ----
#### ---- Date: 2019 May 06 (Mon) ----

library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)
library(gridExtra)
library(lme4)
library(lattice)

theme_set(theme_bw() + theme(panel.spacing=grid::unit(0,"lines")))

load("complexGlmer.rda")

# Incoming objects:
# * complexglmer_list - glmer fits per simulation
# * complexcoef_df - fixed effect coef per simulation
# * betas_df & betas - initial beta values for simulations
# * predictors 

lme4_model <- complexglmer_list[[1]]
long_df <- complexdf_list[[1]]

# Residual plots
plot(lme4_model, type = c("p","smooth"))

plot(lme4_model, service ~ resid(.))

qqmath(ranef(lme4_model, condVar = TRUE))

# dotplot(ranef(lme4_model, condVar=TRUE), lattice.options=list(layout=c(1,3)))

save(file = "glmerDiagnostics.rda"
)

