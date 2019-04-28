#### ---- Project: APHRC Wash Data ----
#### ---- Task: Simulation ----
#### ---- Sub-task: Generate 'fake' response ----
#### ---- By: Steve and Jonathan ----
#### ---- Date: 2019 Mar 05 (Tue) ----

library(arm)
library(mvtnorm)
library(data.table)
library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)

load("globalFunctions.rda")
load("analysisdata.rda")

set.seed(7777)

theme_set(theme_bw() +
            theme(panel.spacing=grid::unit(0,"lines")))

# Simulate multivariate response.

nsims <- 100 # Number of simulations to run
sample_prop <- 0.2 # Prop of sample per hh
year <- 2013

# Predictor variable to simulate
predictors <- "wealthindex"

# Beta values
service1_int <- 0.4
service1_wealth <- 4 
service2_int <- 3
service2_wealth <- 2
service3_int <- 1
service3_wealth <- 3

# Covariance matrix
## Var(Services) = 1; Cov(Service1, Service2) = Cov(Service2, Service3) = 0.5 and Cov(Service1, Service3) = 0
covMat <- matrix(
	c(1, 0.5, 0
		, 0.5, 1, 0.5
		, 0, 0.5, 1
	), 3, 3
)

# Confounder service
serviceU_1 <- 0.1
serviceU_2 <- 0.1
serviceU_3 <- 0.1

sim_df <- (working_df
	%>% filter(intvwyear==year & runif(n())<sample_prop)
	%>% select("hhid_anon", predictors)
	%>% mutate(U = rnorm(n=n())
		, pred1 = serviceU_1*U + service1_wealth*wealthindex + service1_int
		, pred2 = serviceU_2*U + service2_wealth*wealthindex + service2_int
		, pred3 = serviceU_3*U + service3_wealth*wealthindex + service3_int
	)
	%>% droplevels()
)
print(sim_df)

people <- nrow(sim_df)

# Draw from a multivariate distribution
prednames <- grep("^pred[0-9]", names(sim_df), value = TRUE)
means <- (sim_df
	%>% select(prednames)
	%>% as.matrix()
)
mvt_samples <- t(apply(means, 1, function(m){
		rmvnorm(1, mean = m, sigma = covMat)
	})
)

sim_dflist <- list()
for (s in 1:nsims){
	dat <- (mvt_samples
		%>% apply(., 2, function(p){
			rbinom(people, 1, plogis(p))
		})
		%>% data.frame()
		%>% setnames(names(.), prednames)
	)
	sim_dflist[[s]] <- dat
}

# Extract beta values assigned in the simulation
betas <- sapply(grep("service[1-9]", ls(), value = TRUE), get)
betas_df <- (data.frame(betas) 
	%>% rownames_to_column("coef")
	%>% mutate(n = extract_numeric(coef)
		, coef = ifelse(grepl("_int$", coef)
			, paste0("serviceservice", n)
			, paste0("wealthindex:serviceservice", n)
		)	
	)
)
print(betas_df)

save(file = "simulateResponse.rda"
	, sim_df
	, sim_dflist
	, betas_df
	, predictors
	, betas
)

