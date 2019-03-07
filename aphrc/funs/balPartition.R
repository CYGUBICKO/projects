# Random sampling is done within the levels of predictor variable to in order to achieve balanced the class distributions within the test and train sets.

balPartition <- function(df, variable, prop = 0.75){
	df <- as.data.frame(df)
	index_levs <- levels(as.factor(df[, variable]))
	sampled_index <- NULL
	df["temp_id"] <- 1:nrow(df)
	for (lev in index_levs){
		temp_df <- (df
			%>% filter(.data[[variable]]==lev)
		)
		sampled_index <- c(sampled_index, sample(temp_df[, "temp_id"], floor(nrow(temp_df) * prop)))
	}
	sampled_index <- as.numeric(sampled_index)
	df <- select(df, -temp_id)
	train_df <- df[sampled_index, ]
	test_df <- df[-sampled_index, ]
	out <- list(train_df = train_df
		, test_df = test_df
	)
}
