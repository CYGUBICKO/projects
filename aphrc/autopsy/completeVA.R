#### ---- Project: APHRC Verbal Autopsy Data ----
#### ---- Task: Data cleaning and preparation ----
#### ---- By: Steve and Jonathan ----
#### ---- Date: 2019 Jan 29 (Tue) ----

library(dplyr)

load("cleaningVA.rda")

#### ---- 1. Complete cases ------
# Cases were droped based on indicator variable (dropcase) which was generated from. 
# * vadone - Was VA done?
# * intvwresult - Was there VA result?
# * See cleaningVA.R

complete_df <- (working_df
	%>% filter(dropcase == 0)
)
incomplete_df <- (working_df
	%>% filter(dropcase == 1)
)


working_df <- complete_df

save.image("completeVA.rda")

# Save codebook to csv
write.csv(codebook, "va_codebook.csv")
