library(BayesFactor)
library(HDInterval)

## Simulated data for 2x2 design, 20 observations per group
d <- read.csv(file="data/anova_data.csv")

boxplot(observation ~ group+treatment, data=d)

## frequentist analysis
## suggests strong effect of treatment and group and presence of interaction
summary(aov(observation ~ group*treatment, data=d))

## Bayesian anova
bf <- anovaBF(observation ~ group*treatment, data=d)

## Bayes factors of all sub models compared to intercept only model
bf

## Compute Bayes factors against model with both main effects
plot(bf[c(1,2,4)]/bf[3])

## Bayes factor comparing model with and without interaction
## Strong evidence to suggest presence of interaction
bf[4]/bf[3]

## sample from posterior to obtain estimates
chains <- posterior(max(bf), iterations = 100000, thin=100)
summary(chains)

## compute HDI
hdi_anova <- hdi(chains)

## Plot posterior density and HDI
plot(density(chains[,'treatment-treatment 1']))
segments(hdi_anova[1, 'treatment-treatment 1'], 0, hdi_anova[2, 'treatment-treatment 1'], 0, lwd=2)
