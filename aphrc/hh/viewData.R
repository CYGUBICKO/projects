

# This script uses the loadData function to generate .rds file.

library(haven)
library(reshape2)
library(dplyr)
library(tibble)
library(tidyr)

df_name <- "NUHDSS_hhamenitiescharacteristics_anon"
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

# rdsave(working_df, codebook)
