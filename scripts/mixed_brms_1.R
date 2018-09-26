## Analyse simulated response/reaction time data
library(brms)

respRT <- read.csv("data/mixed_sim1.csv")

## Model 1: random intercept only
## Need save_all_pars = TRUE if we want to compute BAyes factors
m0 <- brm(Rsp ~ RT + (1|Sbj), data=respRT, family=bernoulli(), save_all_pars = TRUE)
m0

plot(m0)
plot(marginal_effects(m0))
#launch_shinystan(m0)

## Model 2: Uncorrelated random slopes
m1 <- brm(Rsp ~ RT + (1|Sbj) + (0+RT|Sbj), data=respRT, family=bernoulli(), save_all_pars = TRUE)
m1

plot(m1)
plot(marginal_effects(m1))

## Compute Bayes factor comparing the two models
bf01 <- bayes_factor(m0, m1, log=TRUE)
bf01

## Model 3: Correlated random slopes
m2 <- brm(Rsp ~ RT + (1+RT|Sbj), data=respRT, family=bernoulli(), save_all_pars = TRUE)
m2

plot(m2)
plot(marginal_effects(m2))

bf21 <- bayes_factor(m2, m1, log=TRUE)
