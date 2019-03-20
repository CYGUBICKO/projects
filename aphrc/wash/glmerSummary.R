#### ---- Project: APHRC Wash Data ----
#### ---- Task: Simulation ----
#### ---- Complex glmer summary plots ----
#### ---- By: Steve and Jonathan ----
#### ---- Date: 2019 Mar 16 (Sat) ----

library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)

theme_set(theme_bw() + theme(panel.spacing=grid::unit(0,"lines")))

load("complexGlmer.rda")

# Incoming objects:
# * complexglmer_list - glmer fits per simulation
# * complexcoef_df - fixed effect coef per simulation
# * betas - initial beta values for simulations

# Extract beta values assigned in the simulation
betas_df <- (data.frame(betas) 
	%>% rownames_to_column("labels")
	%>% mutate(coef = ifelse(grepl("[1-9]_int", labels), paste0("serviceservice", extract_numeric(labels))
			, ifelse(grepl("_wealth", labels), "wealthindex", labels)
		)
	)
	%>% group_by(coef)
	%>% summarise(means = mean(betas))
)
print(betas_df)


complexglmer_plot <- (complexcoef_df
   %>% gather(coef, value)
   %>% ggplot(aes(x = value))
   + geom_histogram()
   + geom_vline(data = betas_df, aes(xintercept = means, color = coef)
      , linetype="dashed"
   )
   + labs(x = "Betas", y = "Desnsity")
   + ggtitle("Fixed effect estmates")
   + guides(colour = FALSE)
   + theme(plot.title = element_text(hjust = 0.5))
   + facet_wrap(~coef, scales = "free")
)
print(complexglmer_plot)

save(file = "glmerSummary.rda"
	, complexglmer_plot
)

