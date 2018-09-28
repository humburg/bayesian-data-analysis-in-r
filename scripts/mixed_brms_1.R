## Analyse simulated response/reaction time data
library(brms)
library(ggplot2)

respRT <- read.csv("data/mixed_sim1.csv")

ggplot(respRT, aes(x=factor(Rsp), y=RT)) + geom_boxplot()


## Model 1: random intercept only
## Need save_all_pars = TRUE if we want to compute BAyes factors
m0 <- brm(Rsp ~ RT + (1|Sbj), data=respRT, family=bernoulli(), save_all_pars = TRUE)
m0

plot(m0)
plot(marginal_effects(m0))
pp_check(m0)

## Model 2: Uncorrelated random slopes
m1 <- brm(Rsp ~ RT + (1|Sbj) + (0+RT|Sbj), data=respRT, family=bernoulli(), save_all_pars = TRUE)
m1

plot(m1)
pp_check(m1)

## Compute Bayes factor comparing the two models
bf01 <- bayes_factor(m0, m1, log=TRUE)
bf01

## Model 3: Correlated random slopes
m2 <- brm(Rsp ~ RT + (1+RT|Sbj), data=respRT, family=bernoulli(), save_all_pars = TRUE)
m2

plot(m2)
pp_check(m2)

bf21 <- bayes_factor(m2, m1, log=TRUE)


## Custom priors
get_prior(Rsp ~ RT + (1+RT|Sbj), data=respRT, family=bernoulli())
prior_b <- set_prior("normal(0, 10)", class='b')
prior_sd <- set_prior("student_t(1, 0, 10)", class='sd')

m3 <- brm(Rsp ~ RT + (1+RT|Sbj), data=respRT, family=bernoulli(), prior=c(prior_b, prior_sd), save_all_pars = TRUE)
m3

plot(m3)
pp_check(m3)
