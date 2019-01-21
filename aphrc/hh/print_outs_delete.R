## Objects to report
#hha_drinkwatersource_issues_count
#hha_drinkwatersource_issues_plot
#hha_drinkwatersource_clean_prop_plot

#hha_toilet_2to4yrs_issues_count
#hha_toilet_2to4yrs_issues_plot
#hha_toilet_2to4yrs_clean_prop_plot

#************************************************
#hha_toilet_5plusyrs_issues_count
#hha_toilet_5plusyrs_issues_plot
#hha_toilet_5plusyrs_clean_prop_plot

#hha_toilet_paytouse_issues_count
#hha_toilet_paytouse_issues_plot
#hha_toilet_paytouse_clean_prop_plot

#hha_toilet_paypattern_issues_count
#hha_toilet_paypattern_issues_plot
#hha_toilet_paypattern_clean_prop_plot

#hha_floormaterial_issues_count
#hha_floormaterial_issues_plot
#hha_floormaterial_clean_prop_plot

#hha_roofmaterial_issues_count
#hha_roofmaterial_issues_plot
#hha_roofmaterial_clean_prop_plot

#hha_wallmaterial_issues_count
#hha_wallmaterial_issues_plot
#hha_wallmaterial_clean_prop_plot

#hha_wherecooks_issues_count
#hha_wherecooks_issues_plot
#hha_wherecooks_clean_prop_plot

#hha_cookingfuel_issues_count
#hha_cookingfuel_issues_plot
#hha_cookingfuel_clean_prop_plot







rm(list = ls()[!ls() %in% c(
         "working_df_updated"
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
         , "hhaissuesFunc"
         , "hhcleanedFunc"
         # missProp.Rout
         , "miss_prop_df_html"
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

