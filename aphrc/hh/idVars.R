library(dplyr)
library(tibble)
library(DT)

# Duplicate IDs
id_vars <- grep("id", names(working_df), value = TRUE, ignore.case = TRUE)
id_df <- (working_df
	%>% select(id_vars)
  	%>% sapply(function(x)sum(duplicated(x) & (!is.na(x)|x!="")))
  	%>% enframe(name = "variable")
  	%>% mutate(prop_dup = round(value/nrow(working_df), digits = 3) * 100)
  	%>% rename(dup_count = value)
)

id_dup_dis <- (id_df
	%>% varLabs()
	%>% as.data.frame()
)

id_dup_dis <- datatable(id_dup_dis)

## Objects to report
# id_dup_dis

# Keep necessary files only

rm(list = ls()[!ls() %in% c("working_df"
			, "codebook"
			# Functions
			, "missPropFunc"
			, "saveXlsx"
			, "varLabs"
			, "extractLabs"
			, "propFunc"
			, "tabsFunc"
			, "recodeLabs"
			, "extractIssues"
			# missProp.Rout
			, "miss_prop_df_html"
			, "no_vars_droped"
			, "file_prefix"
			# idVars.Rout
			, "id_dup_dis"
		)
	]
)
