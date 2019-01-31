#### ---- Project: APHRC Verbal Autopsy Data ----
#### ---- Task: Data cleaning and preparation ----
#### ---- By: Steve and Jonathan ----
#### ---- Date: 2019 Jan 29 (Tue) ----

library(dplyr)

#### ---- 1. Subset <5 dataset -----
# Subset dataset to include only under 4 cases and also select variables about under 5. 
# In addition, include general symptom question in section 1.
# * agedeath_years - age at death-years (completed)

femaleabove12_vars <- grep("vafem_", colnames(working_df), value = TRUE)
adultabove5_vars <- grep("va5_", colnames(working_df), value = TRUE)

under5_vals <- c("less than 1 year"
	, "1"
	, "2"
	, "3"
	, "4"
)

dropped_vars <- c(femaleabove12_vars, adultabove5_vars)
underfive_df <- (working_df
	%>% select(-c(dropped_vars))
	%>% filter(agedeath_years %in% under5_vals)
)

# rdnosave()
save(file = rdaname
	# Datasets
	, underfive_df
	, codebook
	# Functions
	, propFunc
	, recodeLabs
	, missPropFunc
	, extractIssues
	, saveXlsx
	, tabsFunc
	, varLabs
	, extractLabs
	# Objects to report
)
