#### ---- Project: APHRC Wash Data ----
#### ---- Task: Descriptives ----
#### ---- By: Steve and Jonathan ----
#### ---- Date: 2019 Feb 10 (Sun) ----

library(tidyr)
library(dplyr)
library(expss)
library(ggplot2)
library(scales)

load("globalFunctions.rda")
load("logisticpca.rda")
load("descriptivePlots.rda")


theme_set(theme_bw()+
theme(panel.spacing=grid::unit(0,"lines")))

# Some cleaning

factors <- function(x){
	factor(x, levels = c(1, 0), labels = c( "Improved", "Unimproved"))
}

working_df <- (working_df
	%>% mutate_at(c(wash_vars, "cat_wash"), funs(factors))
	%>% mutate(expend_total_USD_per_centered = scale(as.numeric(expend_total_USD_per), scale = FALSE))
	%>% mutate(cat_wash_num = ifelse(cat_wash=="Improved", 1, 0))
)

codebook <- updateCodebook("expend_total_USD_per_centered", "Centered total HH expenditure - New")
codebook <- updateCodebook("cat_wash_num", "0/1 Categorized composite WASH variable - New")

#### ---- 1. Water sources ----

tab_vars <- c("intvwyear", "slumarea", "cat_hhwatersource")
legend_title <- "Water sources"
water_plot <- (working_df
	%>% propPlot(tab_vars, legend_title)
)
water_plot <- (water_plot[["prop_plot"]] 
	+ facet_grid(~ slumarea)
	+ theme(axis.text.x=element_text(angle=90))
	+ scale_colour_brewer(palette="Dark2")
	+ labs(x = "Years"
		, title = "Water"
	)
	+ theme(legend.position = "bottom")
)
water_plot

#### ---- 2. Toilet type ----

tab_vars <- c("intvwyear", "slumarea", "cat_hhtoilettype")
legend_title <- "Toilets"
toilet_plot <- (working_df
	%>% propPlot(tab_vars, legend_title)
)
toilet_plot <- (toilet_plot[["prop_plot"]] 
	+ facet_grid(~ slumarea)
	+ theme(axis.text.x=element_text(angle=90))
	+ scale_colour_brewer(palette="Dark2")
	+ labs(x = "Years"
		, title = "Toilet types"
	)
	+ theme(legend.position = "bottom")
)
toilet_plot

#### ---- 3. Garbage disposal ----

tab_vars <- c("intvwyear", "slumarea", "cat_hhgarbagedisposal")
legend_title <- "Garbage"
garbage_plot <- (working_df
	%>% propPlot(tab_vars, legend_title)
)
garbage_plot <- (garbage_plot[["prop_plot"]] 
	+ facet_grid(~ slumarea)
	+ theme(axis.text.x=element_text(angle=90))
	+ scale_colour_brewer(palette="Dark2")
	+ labs(x = "Years"
		, title = "Garbage disposal"
	)
	+ theme(legend.position = "bottom")
)
garbage_plot

#### ---- 4. Logistic PCA Wash Variable ----

tab_vars <- c("intvwyear", "slumarea", "cat_wash")
legend_title <- "WASH"
cat_wash_plot <- (working_df
	%>% propPlot(tab_vars, legend_title)
)
cat_wash_plot <- (cat_wash_plot[["prop_plot"]] 
	+ facet_grid(~ slumarea)
	+ theme(axis.text.x=element_text(angle=90))
	+ scale_colour_brewer(palette="Dark2")
	+ labs(x = "Years"
		, title = "Overall WASH indicator from Logistic PCA"
	)
	+ theme(legend.position = "bottom")
)
cat_wash_plot


#### ---- 5. Access rate (improved/all items) ----
# This is the proportion of improved WASH indicators per case

prop_wash_plot <- (ggplot(working_df, aes(wash_access_rate, colour = slumarea))
	+ geom_density()
	+ scale_colour_brewer(palette = "Dark2")
	+ facet_wrap(~intvwyear)
	+ labs(title = "Proportion of WASH accessed"
		, x = "Proportion of WASH"
		, color = "Slum"
	)
	+ theme(plot.title = element_text(hjust = 0.5)
		, legend.position = "bottom"
	)
)
prop_wash_plot

#### ---- 6. Overall WASH indicator and socio-demo ----


#### ---- 6.1 Gender ----

tab_vars <- c("intvwyear", "slumarea", "gender", "cat_wash")
legend_title <- "WASH Indicators"
wash_gender_plot <- (working_df
	%>% propPlot(tab_vars, legend_title)
)
wash_gender_plot <- (wash_gender_plot[["prop_plot"]] 
	+ facet_grid(gender ~ slumarea)
	+ theme(axis.text.x=element_text(angle=90))
	+ scale_colour_brewer(palette="Dark2")
	+ labs(x = "Years"
		, title = "WASH and gender"
	)
	+ theme(legend.position = "bottom")
)
wash_gender_plot

#### ---- 6.2 Age ----

xvar <- "slumarea"
yvar <- "ageyears"
colvar <- "cat_wash"
wash_age_plot <- (working_df
	%>% meanPlot(xvar, yvar, colvar)
)
wash_age_plot <- (wash_age_plot[["mean_plot"]]
	+ facet_wrap(~intvwyear)
	+ coord_flip()
	+ labs(x = "Slum"
		, title = "WASH and age"
	)
	+ theme(legend.position = "bottom")
)
wash_age_plot

#### ---- 6.3 Ethnicity ----

tab_vars <- c("intvwyear", "slumarea", "ethnicity", "cat_wash")
legend_title <- "WASH Indicators"
wash_ethnicity_plot <- (working_df
	%>% propPlot(tab_vars, legend_title)
)
wash_ethnicity_plot <- (wash_ethnicity_plot[["prop_plot"]] 
	+ facet_grid(ethnicity ~ slumarea)
	+ theme(axis.text.x=element_text(angle=90))
	+ scale_colour_brewer(palette="Dark2")
	+ labs(x = "Years"
		, title = "WASH and ethnicity"
	)
	+ theme(legend.position = "bottom")
)
wash_ethnicity_plot

#### ---- 6.4 Total number of people in the HH ----

numpeople_totalplot <- (ggplot(working_df, aes(numpeople_total, colour = cat_wash))
	+ geom_density()
	+ scale_colour_brewer(palette = "Dark2")
	+ facet_grid(slumarea~intvwyear)
	+ labs(title = "WASH and HH size"
		, x = "HH size"
		, color = "WASH"
	)
	+ theme(axis.text.x=element_text(angle=90))
	+ theme(plot.title = element_text(hjust = 0.5)
		, legend.position = "bottom"
	)
)
numpeople_totalplot


#### ---- 6.5 Wealth index ----

tab_vars <- c("intvwyear", "slumarea", "wealthquintile", "cat_wash")
legend_title <- "WASH Indicators"
wash_wealthquintile_plot <- (working_df
	%>% propPlot(tab_vars, legend_title)
)
wash_wealthquintile_plot <- (wash_wealthquintile_plot[["prop_plot"]] 
	+ facet_grid(wealthquintile ~ slumarea)
	+ theme(axis.text.x=element_text(angle=90))
	+ scale_colour_brewer(palette="Dark2")
	+ labs(x = "Years"
		, title = "WASH and wealth"
	)
	+ theme(legend.position = "bottom")
)
wash_wealthquintile_plot


#### ---- 6.6 Poverty line ----

tab_vars <- c("intvwyear", "slumarea", "isbelowpovertyline", "cat_wash")
legend_title <- "WASH Indicators"
wash_isbelowpovertyline_plot <- (working_df
	%>% propPlot(tab_vars, legend_title)
)
wash_isbelowpovertyline_plot <- (wash_isbelowpovertyline_plot[["prop_plot"]] 
	+ facet_grid(isbelowpovertyline ~ slumarea)
	+ theme(axis.text.x=element_text(angle=90))
	+ scale_colour_brewer(palette="Dark2")
	+ labs(x = "Years"
		, title = "WASH and Poverty line"
	)
	+ theme(legend.position = "bottom")
)
wash_isbelowpovertyline_plot

#### ---- 6.7 Hunger scale ----

tab_vars <- c("intvwyear", "slumarea", "hhdhungerscale", "cat_wash")
legend_title <- "WASH Indicators"
wash_hhdhungerscale_plot <- (working_df
	%>% propPlot(tab_vars, legend_title)
)
wash_hhdhungerscale_plot <- (wash_hhdhungerscale_plot[["prop_plot"]] 
	+ facet_grid(hhdhungerscale ~ slumarea)
	+ theme(axis.text.x=element_text(angle=90))
	+ scale_colour_brewer(palette="Dark2")
	+ labs(x = "Years"
		, title = "WASH and hunger scale"
	)
	+ theme(legend.position = "bottom")
)
wash_hhdhungerscale_plot

#### ---- 6.8 Household Expenditure ----

expend_total_USD_per_centered_plot <- (ggplot(working_df, aes(x = expend_total_USD_per_centered, y = cat_wash_num, colour = slumarea))
	+ stat_sum(alpha = 0.25, na.rm = TRUE)
	+ facet_wrap(~intvwyear)
	+ geom_smooth(aes(lty = slumarea)
		, method = "gam"
		, method.args = list(family = "binomial")
		, formula = y~s(x, k = 20)
		, alpha = 0.1
		, na.rm = TRUE
	)
	+ labs(title = "WASH and HH Expenditure"
		, x = "Centered Expenditure (USSD)"
		, y = "WASH Indicator"
	)
	+ theme(plot.title = element_text(hjust = 0.5)
		, legend.position = "bottom"
	)
)
print(expend_total_USD_per_centered_plot)

descriptive_saved_plots <- sapply(grep("_plot$", ls(), value = TRUE), get)

save(file = "descriptives.rda"
	, working_df
	, codebook
	, descriptive_saved_plots
)
