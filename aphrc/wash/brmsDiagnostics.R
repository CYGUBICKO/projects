#### ---- Project: APHRC Wash Data ----
#### ---- Task: Simulation ----
#### ---- brms simulation plots ----
#### ---- By: Steve and Jonathan ----
#### ---- Date: 2019 May 13 (Mon) ----

library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)
library(brms)

theme_set(theme_bw() + theme(panel.spacing=grid::unit(0,"lines")))

load("brmsModel.rda")

# Incoming objects:
# * brmsmodel_list - glmer fits per simulation
# * brmscoef_df - fixed effect coef per simulation
# * betas_df & betas - initial beta values for simulations
# * predictors 

brmsmodel <- brmsmodel_list[[1]]

# Coefficient plots
print(stanplot(brmsmodel) 
	+ geom_point(data = betas_df, aes(x = betas, y = coef), colour = "red")
	+ scale_y_discrete(breaks = betas_df$coef
		, labels = gsub("b_|_Intercept|hhid_anon__", "", betas_df$coef)
	)
)

# Trace plots
plot(brmsmodel)

# Posterior predictive checks
pp_check(brmsmodel, resp = "service1")
pp_check(brmsmodel, resp = "service2")
pp_check(brmsmodel, resp = "service3")

# Marginal effect of predictors
marginal_effects(brmsmodel, "wealthindex", resp = "service1")
marginal_effects(brmsmodel, "wealthindex", resp = "service2")
marginal_effects(brmsmodel, "wealthindex", resp = "service3")

