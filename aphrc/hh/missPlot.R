library(dplyr)
library(ggplot2)

## Vizualize proportion of missingness
miss_dist <- (miss_prop_df
  %>% filter(miss_prop>0)
)

miss_dist_plot <- (
	ggplot(miss_dist, aes(x = reorder(description, -miss_prop)
		, y = miss_prop, label = miss_prop))
		+ geom_point(stat = "identity"
			, fill = "black"
         , size = 6
         )
  		+ geom_segment(aes(y = 0
      		, x = description
         	, yend = miss_prop
         	, xend = description)
         	, color = "black"
         )
  		+ geom_text(color="white", size=2)
  		+ coord_flip()
  		+ labs(x = "Variables"
				, y = "% Missingness"
			)
)

## Objects to report
#miss_dist_plot
save(file=rdaname
   , miss_dist_plot
)

