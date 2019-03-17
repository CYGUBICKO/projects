#### ---- Project: APHRC Wash Data ----
#### ---- Task: Simulation ----
#### ---- Fit GLM to recapture betas ----
#### ---- By: Steve and Jonathan ----
#### ---- Date: 2019 Mar 16 (Sat) ----

library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)
theme_set(theme_bw() + theme(panel.spacing=grid::unit(0,"lines")))

load("globalFunctions.rda")
load("simulateResponse.rda")

set.seed(7902)

# Objects in
# * sim_dflist
# * betas
# * predictors

response <- "service1"
sims <- length(sim_dflist)
coef_list <- list()
glm_list <- list()
for (s in 1:sims){
	model_form <- as.formula(paste0(response, "~ ", paste(predictors, collapse = "+")))
	glm_model <- glm(model_form, data = sim_dflist[[s]], family = "binomial")
	coef_list[[s]] <- coef(glm_model)
	glm_list[[s]] <- glm_model
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

beta_plot <- (coef_df
	%>% gather(coef, value)
	%>% ggplot(aes(x = value))
	+ geom_density(alpha = 0.3, fill = "lightgreen")
  	+ geom_vline(data = betas_df, aes(xintercept = betas, color = coef)
     	, linetype="dashed"
  	)
  	+ labs(x = "Betas", y = "Desnsity")
  	+ ggtitle("Fitted vs random choice beta")
  	+ guides(colour = FALSE)
  	+ theme(plot.title = element_text(hjust = 0.5))
	+ facet_wrap(~coef, scales = "free")
)
print(beta_plot)


