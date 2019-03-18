#### ---- Project: APHRC Wash Data ----
#### ---- Task: Simulation ----
#### ---- Sub-task: Generate 'fake' response ----
#### ---- By: Steve and Jonathan ----
#### ---- Date: 2019 Mar 05 (Tue) ----

library(arm)
library(dplyr)
library(tidyr)
library(ggplot2)

load("globalFunctions.rda")
load("analysisdata.rda")

set.seed(7902)

theme_set(theme_bw() +
            theme(panel.spacing=grid::unit(0,"lines")))

# Aim is to simulate the outcome variable so as to understand the underlying distribution.

nsims = 50 # Number of simulations to run
df_prop <- 0.2 # Prop of data to use

# Predictor variable to simulate
predictors <- "wealthindex"
n <- nrow(working_df)

# Beta values
beta1_int <- 2
beta1_wealth <- 4 
beta2_int <- 3
beta2_wealth <- 5
beta3_int <- 1
beta3_wealth <- 3
sim_df <- (working_df
	%>% select_("hhid_anon", predictors)
	%>% filter(runif(n)<df_prop)
	%>% mutate(
		pred1 = beta1_wealth*wealthindex + beta1_int
		, pred2 = beta2_wealth*wealthindex + beta2_int
		, pred3 = beta3_wealth*wealthindex + beta3_int
	)
)
summary(sim_df)

# Proportion of 1s per simulation
prop_df <- tibble(sims = 1:nsims
	, service1 = numeric(nsims)
	, service2 = numeric(nsims)
	, service3 = numeric(nsims)
)

people <- nrow(sim_df)
sim_dflist <- list()

for (i in 1:nsims){
	dat <- (sim_df
		%>% mutate(
			service1 = rbinom(people, 1, plogis(pred1))
			, service2 = rbinom(people, 1, plogis(pred2))
			, service3 = rbinom(people, 1, plogis(pred3))
		)
	)
	prop_df[i,2] <- mean(dat[["service1"]])
	prop_df[i,3] <- mean(dat[["service2"]])
	prop_df[i,4] <- mean(dat[["service3"]])
	sim_dflist[[i]] <- dat
}

summary(prop_df)

print(sim_dflist)

prop_plot <- (prop_df
	%>% gather(var, prop, -sims)
	%>% ggplot(aes(x = prop))
		+ geom_histogram(alpha = 0.4)
		+ facet_grid(~var, scales = "free")
)

print(prop_plot)

# sim_df: simulated predicted values
# sim_dflist: simulated predicted response variables per sim

betas <- sapply(grep("beta[1-9]", ls(), value = TRUE), get)

descriptive_saved_plots <- sapply(grep("_plot$", ls(), value = TRUE), get)
save(file = "simulateResponse.rda"
	, sim_df
	, sim_dflist
	, betas
	, predictors
)

