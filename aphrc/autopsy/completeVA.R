#### ---- Project: APHRC Verbal Autopsy Data ----
#### ---- Task: Data cleaning and preparation ----
#### ---- By: Steve and Jonathan ----
#### ---- Date: 2019 Jan 29 (Tue) ----

library(data.table)
library(dplyr)

load("loadData.rda")

#### ---- 1. Complete cases ------
# Cases were droped based on the following variables. 
# * vadone - Was VA done?
# * intvwresult - Was there VA result?

va_result <- c("completed", "other")

complete_df <- (working_df
	%>% filter(vadone == "yes" & (intvwresult %in% va_result))
)
incomplete_df <- (working_df
	%>% filter(vadone != "yes" | (!intvwresult %in% va_result))
)

print(summary(working_df))

working_df <- complete_df

save(file = "completeVA.rda"
	, working_df
	, incomplete_df
	, codebook
)

# Save codebook to csv
write.csv(codebook, "va_codebook.csv")
