## Simulate data for multiple regression

set.seed(20180928)
treat <- rep(c(-1,1), times=40)
covar <- rnorm(length(treat), mean=3, sd=2)

intercept <- 3
treat_b <- 0.5
covar_b <- 0.1

obs <- intercept + treat_b+treat + covar_b*covar + rnorm(length(treat), 0, sqrt(0.6))

data <- data.frame(treatment=paste("treatment", (treat+1)/2+1), covariate=covar, observation=obs)
write.csv(data, file="bayesian-data-analysis-in-r/data/regression_data.csv")
