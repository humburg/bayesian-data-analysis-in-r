## Analyse simulated response/reaction time data
library(brms)
library(ggplot2)

respRT <- read.csv("data/mixed_sim2.csv")

respRT$Cond <- factor(respRT$Cond)
respRT$Rsp <- factor(respRT$Rsp)

## Plot data
ggplot(respRT, aes(x=factor(Cond), y=RT, fill=factor(Rsp))) + geom_boxplot()


## Model 1: random intercept only
## Need save_all_pars = TRUE if we want to compute BAyes factors
m0 <- brm(Rsp ~ RT + Cond + (1|Sbj), data=respRT, family=bernoulli(), 
          save_all_pars = TRUE)
m0

plot(m0)
plot(marginal_effects(m0))

## Model 2: Uncorrelated random slopes
m1 <- brm(Rsp ~ RT + Cond + (1|Sbj) + (0+RT|Sbj) + (0+Cond|Sbj), data=respRT, 
          family=bernoulli(), save_all_pars = TRUE)
m1

plot(m1)
plot(marginal_effects(m1))

## Compute Bayes factor comparing the two models
bf01 <- bayes_factor(m0, m1, log=TRUE)
bf01

## Model 3: Correlated random slopes
m2 <- brm(Rsp ~ RT*Cond + (1|Sbj) + (0+RT+Cond|Sbj), data=respRT,
          family=bernoulli(), save_all_pars = TRUE)

## diagnostics
plot(m2)
launch_shinystan(m2)

bf20 <- bayes_factor(m2, m0, log=TRUE)

