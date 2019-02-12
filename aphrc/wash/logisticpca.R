#### ---- Project: APHRC Wash Data ----
#### ---- Task: Logistic PCA ----
#### ---- By: Steve and Jonathan ----
#### ---- Date: 2019 Feb 9 (Sat) ----

library(dplyr)
library(logisticPCA)

load("globalFunctions.rda")
load("complete.rda")

## Conduct logistic PCA to create a single variable for the three wash variables:
# * cat_hhwatersource
# * cat_hhtoilettype
# * cat_hhgarbagedisposal

wash_vars <- grep("^cat_", colnames(working_df), value = TRUE)

## Convert Improved and Unimproved to 1 and 0, respectively:

patterns <- c("^improved", "unimproved")
replacements <- c(1, 0)
working_df <- (working_df
	%>% recodeLabs(wash_vars, patterns, replacements, insert = FALSE)
	%>% mutate_at(wash_vars, as.numeric)
	%>% rowsumFunc(wash_vars, "total_wash_indicators")
	%>% mutate(wash_access_rate = total_wash_indicators/(length(wash_vars)))
)

#### ---- 1. Cross-validation -----
# Determine the optimal number of Bernoulli saturated models?? (https://cran.r-project.org/web/packages/logisticPCA/vignettes/logisticPCA.html)

wash_vars_df <- select(working_df, wash_vars)
#logistic_cv <- cv.lpca(wash_vars_df, ks = 2, ms = seq(2, 14, 2))
#logistic_cv_plot <- plot(logistic_cv)
#logistic_cv_plot

### ---- 2. Logistic moldel ----

logistic_pca <- logisticPCA(wash_vars_df, k = 2, m = 0, main_effects = TRUE)

# Create the two categories (Unimproved, Improved)

score <- logistic_pca[["PCs"]][,2]
cats <- quantile(score, probs=seq(0, 1, by = 0.5), na.rm = TRUE)
working_df <- (working_df
	%>% mutate(wash_score = score
		, cat_wash = cut(wash_score
			, breaks = cats
			, include.lowest = TRUE
			, labels = c(0, 1)
			, ordered_result = TRUE
		)
	)
)


save(file = "logisticpca.rda"
	, working_df
	, codebook
	, wash_vars
	, logistic_pca
)

