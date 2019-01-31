#### ---- Project: APHRC Verbal Autopsy Data ----
#### ---- Task: Data cleaning and preparation ----
#### ---- By: Steve and Jonathan ----
#### ---- Date: 2019 Jan 29 (Tue) ----

library(data.table)
library(dplyr)

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
working_df <- complete_df

# rdnosave()
save(file = rdaname
	, working_df
	, incomplete_df
	, codebook
)
