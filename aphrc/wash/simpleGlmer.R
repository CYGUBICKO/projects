#### ---- Project: APHRC Wash Data ----
#### ---- Task: Simulation ----
#### ---- Fit GLMER to recapture betas ----
#### ---- By: Steve and Jonathan ----
#### ---- Date: 2019 Mar 16 (Sat) ----

library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)
library(lme4)

theme_set(theme_bw() + theme(panel.spacing=grid::unit(0,"lines")))

load("globalFunctions.rda")
load("simulateResponse.rda")

set.seed(7902)

# Objects in
# * sim_dflist
# * betas
# * predictors

nsims <- length(sim_dflist)
model_form <- as.formula(service1 ~ wealthindex + (1|hhid_anon))

coef_list <- list()
glmer_list <- list()

for (s in 1:nsims){
	glmer_model <- glmer(model_form
		, data = sim_dflist[[s]]
      , family = binomial
      , control = glmerControl(optimizer = "nloptwrap")
	)
	coef_list[[s]] <- fixef(glmer_model)
	glmer_list[[s]] <- glmer_model
}
coef_df <- Reduce(rbind, coef_list) %>% as_tibble()
summary(coef_df)
print(coef_df)

# Extract beta values assigned in the simulation
betas_df <- (data.frame(betas) 
	%>% rownames_to_column("coef")
	%>% filter(grepl("beta1", coef)) # comment out/replace witht the correct beta
	%>% mutate(coef = ifelse(grepl("_int", coef), "(Intercept)", predictors))
)
print(betas_df)

glmer_beta_plot <- (coef_df
	%>% gather(coef, value)
	%>% ggplot(aes(x = value))
	+ geom_histogram()
  	+ geom_vline(data = betas_df, aes(xintercept = betas, color = coef)
     	, linetype="dashed"
  	)
  	+ labs(x = "Betas", y = "Desnsity")
  	+ ggtitle("Fitted vs random choice beta")
  	+ guides(colour = FALSE)
  	+ theme(plot.title = element_text(hjust = 0.5))
	+ facet_wrap(~coef, scales = "free")
)
print(glmer_beta_plot)

save(file = "simpleGlmer.rda"
	, glmer_beta_plot
)
