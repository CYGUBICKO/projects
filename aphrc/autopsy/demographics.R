#### ---- Project: APHRC Verbal Autopsy Data ----
#### ---- Task: Descriptives ----
#### ---- Sub-Task: Demographics ----
#### ---- By: Steve and Jonathan ----
#### ---- Date: 2019 Jan 29 (Tue) ----

library(tidyr)
library(dplyr)
library(expss)

#### ---- 1. Age ----

age_tab <- (underfive_df
	%>% tabsFunc(c("slumarea", "agegroupdeath"))
)


