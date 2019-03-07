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

# Sample the dataset within predictor variable: Balancing 


# Model set up

nsims = 1000 # Number of simulations to run
df_prop <- 0.2 # Prop of data to use

predictors <- c("intvwyear"
	, "slumarea"
	, "ageyears"
	, "gender"
	, "ethnicity"
	, "numpeople_total"
	, "isbelowpovertyline"
  , "wealthquintile"
  , "expend_total_USD_per_centered"
)

#### ---- Water source ----

model_form <- as.formula(paste0("cat_hhwatersource", "~ "
		, paste(predictors, collapse = "+")
	)
)
print(model_form)

subset_df <- working_df %>% balPartition("cat_hhwatersource", prop = df_prop)
simulation_df <- subset_df[["train_df"]]
 
# Fake response object
yfake_obj_water <- (simulation_df
	%>% genfakeRes(model_form, ., nsims = nsims)
)
yfake_df_water <- yfake_obj_water[["yfake"]] %>% mutate(variable = "Water source")
yobs_prop_water <- yfake_obj_water[["yobs_prop"]]


#### ---- Toilet type ----

model_form <- as.formula(paste0("cat_hhtoilettype", "~ "
		, paste(predictors, collapse = "+")
	)
)

subset_df <- working_df %>% balPartition("cat_hhtoilettype", prop = df_prop)
simulation_df <- subset_df[["train_df"]]
 
# Fake response object
yfake_obj_toilet <- (simulation_df
	%>% genfakeRes(model_form, ., nsims = nsims)
)
yfake_df_toilet <- yfake_obj_toilet[["yfake"]] %>% mutate(variable = "Toilet type")
yobs_prop_toilet <- yfake_obj_toilet[["yobs_prop"]]

#### ---- Garbage disposal ----

model_form <- as.formula(paste0("cat_hhgarbagedisposal", "~ "
		, paste(predictors, collapse = "+")
	)
)

subset_df <- working_df %>% balPartition("cat_hhgarbagedisposal", prop = df_prop)
simulation_df <- subset_df[["train_df"]]
 
# Fake response object
yfake_obj_garbage <- (simulation_df
	%>% genfakeRes(model_form, ., nsims = nsims)
)
yfake_df_garbage <- yfake_obj_garbage[["yfake"]] %>% mutate(variable = "Garbage disposal")
yobs_prop_garbage <- yfake_obj_garbage[["yobs_prop"]]

yfake_df <- rbind(yfake_df_water, yfake_df_toilet, yfake_df_garbage)
head(yfake_df)

# Compare the results with the observed
cols <- c("Water source" = "red"
	, "Toilet type" = "blue"
	, "Garbage disposal" = "green"
)

yfake_glm_plot <- (ggplot(yfake_df, aes(x = yfake, fill = variable)) 
	+ geom_density(alpha = 0.3)
	+ scale_fill_manual(name = "Improved"
		, values = cols
		, breaks = c("Water source", "Toilet type", "Garbage disposal")
		, labels = c("Water source", "Toilet type", "Garbage disposal")
	)
	+ geom_segment(aes(x = yobs_prop_water, xend = yobs_prop_water, y = 0, yend = 5)
		, colour = "red", arrow=arrow(length=unit(0.3,"cm"), ends = "first")
	)
	+ geom_segment(aes(x = yobs_prop_toilet, xend = yobs_prop_toilet, y = 0, yend = 5)
		, colour = "blue", arrow=arrow(length=unit(0.3,"cm"), ends = "first")
	)
	+ geom_segment(aes(x = yobs_prop_garbage, xend = yobs_prop_garbage, y = 0, yend = 5)
		, colour = "green", arrow=arrow(length=unit(0.3,"cm"), ends = "first")
	)
	+ labs(x = "Prop. of fake 1s generated", y = "Desnsity")
	+ ggtitle("Compare proportion of fake 1s generated vs observed")
	+ theme(plot.title = element_text(hjust = 0.5))
)
print(yfake_glm_plot)

#### ---- Pseudo Multivariate model ----

patterns <- c("watersource", "toilettype", "garbagedis")
replacements <- c("Water source", "Toilet type", "Garbage disposal")
working_df_long <- (working_df 
	%>% gather(wash_variable, wash_variable_value, wash_vars)
	%>% recodeLabs("wash_variable", patterns, replacements, insert = FALSE)
	%>% mutate_at("wash_variable", factor)
)

model_form <- as.formula(paste0("wash_variable_value", "~ ("
		, paste(predictors, collapse = "+")
		, ")*"
		, "wash_variable"
	)
)

subset_df <- working_df_long %>% balPartition("wash_variable", prop = df_prop)
simulation_df <- subset_df[["train_df"]]
 
# Fake response object
yfake_obj_wash <- (simulation_df
	%>% genfakeRes(model_form, ., nsims = nsims)
)
long_labs <- yfake_obj_wash[["long_labs"]]
yfake_df_wash <- (yfake_obj_wash[["yfake"]] 
	%>% mutate(variable = long_labs)
)
yobs_prop_wash <- yfake_obj_wash[["yobs_prop"]]
model_summary_wash <- yfake_obj_wash[["model_summary"]]
model_summary_wash

# Vizualize
yfake_glm_wash_plot <- (ggplot(yfake_df_wash, aes(x = yfake, fill = variable)) 
	+ geom_density(alpha = 0.3)
	+ scale_fill_manual(name = "Improved"
		, values = cols
		, breaks = c("Water source", "Toilet type", "Garbage disposal")
		, labels = c("Water source", "Toilet type", "Garbage disposal")
	)
	+ labs(x = "Prop. of fake 1s generated", y = "Desnsity")
	+ ggtitle("Compare proportion of fake 1s generated (Psedo multi-variate)")
	+ theme(plot.title = element_text(hjust = 0.5))
)
print(yfake_glm_wash_plot)


sims_saved_plots <- sapply(grep("_plot$", ls(), value = TRUE), get)

save(file = "simulateResponse.rda"
	, model_summary_wash
	, yobs_prop_water 
	, yobs_prop_toilet
	, yobs_prop_garbage
	, yfake_glm_plot
	, yfake_glm_wash_plot
)

