library(BayesFactor)

## Simulated data for 2x2 design, 20 observations per group
d <- read.csv(file="data/anova_data.csv")

boxplot(observation ~ group+treatment, data=d)

## frequentist analysis
## suggests strong effect of treatment and group and presence of interaction
summary(aov(observation ~ group*treatment, data=d))

## Bayesian anova
bf <- anovaBF(observation ~ group*treatment, data=d)

## Bayes facters of all sub models compared to intercept only model
bf

## Compute Bayes factors against model with both main effects
plot(bf[c(1,2,4)]/bf[3])

## Bayes factor comparing model with and without interaction
## Some evidence to suggest presence of interaction
bf[4]/bf[3]

## sample from posterior to obtain estimates
chains <- posterior(max(bf), iterations = 10000, thin=10)
summary(chains)
