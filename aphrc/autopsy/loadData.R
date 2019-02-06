
# This script uses the loadData function to generate .rds file.

library(data.table)
library(haven)
library(reshape2)
library(dplyr)
library(tibble)
library(tidyr)

source("../hh/globalFunctions.R")

df_name <- "verbalautopsy_2002-2015"
file_extension <- "dta"
df_folder <- "data"
df_outname <- "hh_working_df"

load_df <- loadData(df_name
	, file_extension
  	, df_folder
  	, df_outname
)

working_df <- load_df[["working_df"]]
codebook <- load_df[["codebook"]]


#### ---- 1. Shorten variable names ----
## Remove vau_*

old_names <- colnames(working_df)
new_names <- gsub("vau_*", "", old_names)
working_df <- (working_df
	%>% setnames(old_names, new_names)
)

codebook <- (codebook
	%>% mutate(variable = gsub("vau_*", "", variable))
)

save(file="loadData.rda"
	, working_df, codebook
)
