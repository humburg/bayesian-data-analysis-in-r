library(BayesFactor)

data <- read.csv("bayesian-data-analysis-in-r/data/regression_data.csv")

boxplot(data$observation ~ data$treatment)
plot(data$covariate, data$observation)

## Main effects only
bf_main <- lmBF(observation ~ treatment + covariate, data=data)
bf_main

## Main effects and interaction
bf_full <- lmBF(observation ~ treatment*covariate, data=data)
bf_full

## Compare the two models
## Weak evidence in favour of main effects only
bf_main/bf_full

## Simple regression with treatment effect only
bf_simple <- lmBF(observation ~ treatment, data=data)
bf_simple

## Very weak evidence in favour of including covariate in model
bf_simple/bf_main

## Obtain estimates from the posterior
post_main <- posterior(bf_main, iterations = 100000, thin=100)
summary(post_main)
plot(post_main)

post_full <- posterior(bf_full, iterations = 100000, thin=100)
summary(post_full)

## compare to frequentist model
lm_fit <- lm(observation ~ treatment + covariate, data=data)
summary(lm_fit)
