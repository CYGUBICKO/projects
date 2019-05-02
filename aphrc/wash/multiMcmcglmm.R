#### ---- Project: APHRC Wash Data ----
#### ---- Task: Simulation ----
#### ---- Fit MCMCglmm to recapture betas ----
#### ---- By: Steve and Jonathan ----
#### ---- Date: 2019 Apr 02 (Tue) ----

library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)
library(MCMCglmm)

options(dplyr.width = Inf)

theme_set(theme_bw() + theme(panel.spacing=grid::unit(0,"lines")))

load("globalFunctions.rda")
load("simulateResponse.rda")

set.seed(7777)

# Objects in
# * sim_dflist
# * betas_df
# * betas
# * predictors

services <- c("service1", "service2", "service3")
nsims <- length(sim_dflist)


# Priors
priors <- list(G=list(G1 = (list(V = diag(3), nu = 0.002, alpha.mu = rep(0, 3), alpha.V  = diag(3)*625)))
	, R = list(V = diag(3), nu = 0.002)
)
#priors <- list(G=list(G1=(list(V=diag(3)*0.2, nu=1, alpha.mu = rep(0, 3), alpha.V  = diag(3)*1000))),R=list(V=diag(3)*0.009, nu=1))
#IJ <- (1/3) * (diag(3) + matrix(1, 3, 3))
#priors$B = list(mu = rep(0, 6), V = kronecker(IJ, diag(2)*1.7 + pi^2/3))
multimcmcglmmcoef_list <- list()
multimcmcglmm_list <- list()
for (s in 1:nsims){
   df <- (sim_dflist[[s]]
      %>% data.frame()
   )
	model <- MCMCglmm(cbind(service1, service2, service3) ~ wealthindex:trait + trait - 1
		, random = ~us(trait):hhid_anon
		, rcov = ~us(trait):units
		, family = c("categorical", "categorical", "categorical")		
		, data = df
 		, prior = priors
 		, trunc = TRUE
		, verbose = FALSE
   )
	model_summary <- summary(model)
   multimcmcglmmcoef_list[[s]] <- model_summary[["solutions"]][,1]
   multimcmcglmm_list[[s]] <- model
}

multimcmcglmmcoef_df <- Reduce(rbind, multimcmcglmmcoef_list) %>% as_tibble()
summary(multimcmcglmmcoef_df)
print(multimcmcglmmcoef_df)

save(file = "multiMcmcglmm.rda"
	, multimcmcglmm_list
   , multimcmcglmmcoef_df
	, predictors
	, betas_df
	, betas
)

