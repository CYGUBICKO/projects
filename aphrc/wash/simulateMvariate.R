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

nsims <- 1 # Number of simulations to run
sample_prop <- 0.2 # Prop of sample per hh
year <- 2013

# Predictor variable to simulate
predictors <- "wealthindex"

# Beta values
service1_int <- 0.3
service1_wealth <- 0.4 
service2_int <- 0.2
service2_wealth <- 0.5
service3_int <- 0.1
service3_wealth <- 0.6
# Covariance matrix
## Var(Services) = 1; Cov(Service1, Service2) = Cov(Service2, Service3) = 0.5 and Cov(Service1, Service3) = 0
covMat <- matrix(
	c(4, 0.5, 0
		, 0.5, 2, 0
		, 0, 0, 3
	), 3, 3
)*0.02

# Confounder service
serviceU_1 <- 1
serviceU_2 <- 1
serviceU_3 <- 1

sim_df <- (working_df
	%>% group_by(intvwyear, hhid_anon)
	%>% filter(intvwyear %in% year & runif(n())<=sample_prop & !is.nan(wealthindex))
	%>% select(hhid_anon, predictors)
	%>% ungroup()
#	%>% mutate_at(predictors, scale)
#	%>% group_by(hhid_anon)
	%>% mutate(U1 = rnorm(n=n())
		, U2 = rnorm(n=n())
		, U3 = rnorm(n=n())
		, pred1 = serviceU_1*U1 + service1_wealth*wealthindex + service1_int
		, pred2 = serviceU_2*U2 + service2_wealth*wealthindex + service2_int
		, pred3 = serviceU_3*U3 + service3_wealth*wealthindex + service3_int
	)
#	%>% ungroup()
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
		rmvnorm(1, mean = scale(m), sigma = covMat)
	})
)

sim_dflist <- list()
for (s in 1:nsims){
	dat <- (mvt_samples
		%>% apply(., 2, function(p){
			rbinom(people, 1, plogis(p))
		})
		%>% data.frame()
		%>% setnames(names(.), c("service1", "service2", "service3"))
		%>% bind_cols(select(sim_df, c("hhid_anon", predictors)))
	)
	sim_dflist[[s]] <- dat
}

head(sim_dflist[[1]])

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

save(file = "simulateMvariate.rda"
	, sim_df
	, sim_dflist
	, betas_df
	, predictors
	, betas
)

