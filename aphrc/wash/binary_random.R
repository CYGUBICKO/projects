library(splines)
n <- 1e5
p_0 <- 0.5
beta_x <- 0.5
beta_z <- 1e-0
seed <- 403

set.seed(seed)
beta_0 <- qlogis(p_0)
print(beta_0)

x <- rnorm(n)
z <- rnorm(n)

ran <- seq(-3, 3, length.out=201)
pfun <- function(beta_0, beta_x, beta_z){

	o <- beta_0 + beta_x*x + beta_z*z
	res <- rbinom(n, size=1, prob=plogis(o))
	smod <- glm(res ~ ns(x, 4), family="binomial")
	return(predict(smod
		, newdat=data.frame(x=ran)
	))
}

plot(ran, pfun(beta_0, beta_x, 0), type="l")

for(beta_z in 1:3){
	lines(ran, pfun(beta_0, beta_x, beta_z))
}
