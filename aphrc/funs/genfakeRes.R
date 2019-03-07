# This functions uses arm package to generate 'fake' response variable for a generalised logistic model.

# formular - Model formular
# df - input dataframe
# nsims - Number of simulations to run

genfakeRes <- function(formular, data = data, nsims = 1000){
	model_fit <- glm(formular
		, data = data
		, family = "binomial"
	)
	model_summary <- summary(model_fit)
	# Extract data from the model object and create data for predictors
	model_df <- model_fit[["model"]]
	X <- (model_df
		%>% model.matrix(formular, .)
		%>% as.matrix()
		%>% na.omit()
	)
	n <- nrow(X)

	# Simulate the glm object 
	model_sim <- sim(model_fit, nsims)
	
	# y_rep holds the 'fake' response
	y_rep <- array(NA, c(nsims, n))
	for (s in 1:nsims){
		xb <- X %*% model_sim@coef[s, ]
		p <- 1/(1 + exp(-xb))
		y_rep[s, ] <- rbinom(n, 1, p)
	}

	# Compute the proportion of 1s in the fake response
	sim_prop <- apply(y_rep, 2, function(x){
			p <- sum(x == 1)/length(x)
			return(p)
		}
	)
	yfake_df <- data.frame(yfake = sim_prop)

	# Proportion of 1s in the observed data
	res <- all.vars(formular)[[1]]
	inter <- last(all.vars(formular)) # In case ther is interaction in the model
	yobs_prop <- sum(data[, res]==1)/length(data[, res])
	out <- list(yfake = yfake_df
		, yobs_prop = yobs_prop
		, model_summary = model_summary
		, long_labs = model_df[[inter]]
	)
}
