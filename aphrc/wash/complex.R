#### ---- Project: APHRC Wash Data ----
#### ---- Task: Simulation ----
#### ---- Sub-task: Generate 'fake' response ----
#### ---- By: Steve and Jonathan ----
#### ---- Date: 2019 Mar 05 (Tue) ----

library(arm)
library(dplyr)
library(tidyr)
library(ggplot2)
theme_set(theme_bw())

load("compdata.rda")

set.seed(7901)
nsims <- 200
beta1_int <- 3
beta1_wealth <- 5
beta2_int <- 3
beta2_wealth <- 5

complex <- (complex
	%>% mutate(
		pred1 = beta1_wealth*wealthindex + beta1_int
		, pred2 = beta2_wealth*wealthindex + beta2_int
	)
)

people <- nrow(complex)
datlist <- list()
propv <- numeric(nsims)
for (i in 1:nsims){
	dat <- (complex
		%>% mutate(
			serv1 = rbinom(people, 1, plogis(pred1))
			, serv2 = rbinom(people, 1, plogis(pred1))
		)
	)
	propv[[i]] <- mean(dat[["serv1"]])
	datlist[[i]] <- dat
}

warnings()

print(ggplot(tibble(prop=propv), aes(x=prop))
	+ geom_histogram()
)
