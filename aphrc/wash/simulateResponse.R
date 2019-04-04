#### ---- Project: APHRC Wash Data ----
#### ---- Task: Simulation ----
#### ---- Sub-task: Generate 'fake' response ----
#### ---- By: Steve and Jonathan ----
#### ---- Date: 2019 Mar 05 (Tue) ----

library(arm)
library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)

load("globalFunctions.rda")
load("analysisdata.rda")

set.seed(7902)

theme_set(theme_bw() +
            theme(panel.spacing=grid::unit(0,"lines")))

# Aim is to simulate the outcome variable so as to understand the underlying distribution.

nsims <- 100 # Number of simulations to run
sample_prop <- 0.6 # Prop of sample per hh
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

# Confounder service
serviceU_1 <- 0.1
serviceU_2 <- 0.1
serviceU_3 <- 0.1

sim_df <- (working_df
	%>% filter(intvwyear==year & runif(n())<sample_prop)
	%>% select_("hhid_anon", predictors)
	%>% mutate(U = rnorm(n=n())
		, pred1 = serviceU_1*U + service1_wealth*wealthindex + service1_int
		, pred2 = serviceU_2*U + service2_wealth*wealthindex + service2_int
		, pred3 = serviceU_3*U + service3_wealth*wealthindex + service3_int
	)
	%>% droplevels()
)
print(sim_df)

summary(sim_df)

# Proportion of 1s per simulation
service_prop <- tibble(sims = 1:nsims
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
	service_prop[i,2] <- mean(dat[["service1"]])
	service_prop[i,3] <- mean(dat[["service2"]])
	service_prop[i,4] <- mean(dat[["service3"]])
	sim_dflist[[i]] <- dat
}

summary(service_prop)

print(sim_dflist)

prop_plot <- (service_prop
	%>% gather(var, prop, -sims)
	%>% ggplot(aes(x = prop))
		+ geom_histogram()
		+ facet_grid(~var, scales = "free")
		+ labs(x = "Proportion of HHs that has access to services (WASH)"
			, y = "Count"
		)
)

print(prop_plot)
ggsave("prop_plot.pdf", prop_plot)

# sim_df: simulated predicted values
# sim_dflist: simulated predicted response variables per sim

betas <- sapply(grep("service[1-9]", ls(), value = TRUE), get)

# Extract beta values assigned in the simulation
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

