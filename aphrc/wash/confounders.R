#### ---- Project: APHRC Wash Data ----
#### ---- Task: Simulation ----
#### ---- Add confounders ----
#### ---- By: Steve and Jonathan ----
#### ---- Date: 2019 Mar 16 (Sat) ----

library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)

theme_set(theme_bw() + theme(panel.spacing=grid::unit(0,"lines")))
options(dplyr.width = Inf)

load("globalFunctions.rda")
load("simulateResponse.rda")

set.seed(7902)

# Objects in
# * sim_dflist
# * betas
# * predictors

nsims <- length(sim_dflist)

conf_dflist <- list()

# Confounder betas
beta1_conf <- 0.2 # Service1
beta2_conf <- 0.4 # Service2
beta3_conf <- 0.4 #Service13

for (s in 1:nsims){
	dat <- (sim_dflist[[s]]
		%>% group_by(hhid_anon)
		%>% mutate(conf_serv1 = rnorm(n = 1, mean = service1*beta1_conf, sd = 1)
			, conf_serv2 = rnorm(n = 1, mean = service2*beta2_conf, sd = 1)	
			, conf_serv3 = rnorm(n = 1, mean = service3*beta3_conf, sd = 1)	
		)
	)
	conf_dflist[[s]] <- dat
}

sapply(conf_dflist, summary)

print(conf_dflist)

save(file = "confounders.rda"
	, conf_dflist
	, betas
	, predictors
)

