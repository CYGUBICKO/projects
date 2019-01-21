# Objects to be reported

rm(list = ls()[!ls() %in% c(
			# missProp.Rout
			 "miss_prop_df_html"
			, "no_vars_droped"
			, "file_prefix"
			# idVars.Rout
			, "id_dup_dis"
			# backgroundSummary
			, "hha_sleeprooms_total_issues_counts_html"
			, "hha_sleeprooms_total_count_plot"
			, "hha_sleeprooms_total_mean_plot"
			, "hha_rentablerooms_total_issues_counts_html"
			, "hha_rentablerooms_total_count_plot"
			, "hha_rentablerooms_total_mean_plot"
			, "hha_numpeople_total_issues_counts_html"
			, "hha_numpeople_total_count_plot"
			, "hha_numpeople_total_mean_plot"
			# hhamenitiesSummary
			, "hha_drinkwatersource_issues_count"
			, "hha_drinkwatersource_issues_plot"
			, "hha_drinkwatersource_clean_prop_plot"
			, "hha_toilet_2to4yrs_issues_count"
			, "hha_toilet_2to4yrs_issues_plot"
			, "hha_toilet_2to4yrs_clean_prop_plot"
		)
	]
)

