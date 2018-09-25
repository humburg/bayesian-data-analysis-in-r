library(BayesFactor)
library(HDInterval)
library(ggplot2)

data <- read.csv("bayesian-data-analysis-in-r/data/weight_perception.csv")

data <- data[!is.na(data$Weight),]

ggplot(data, aes(y=Weight, x=factor(Age), fill=Gender)) + geom_boxplot()
ggplot(data, aes(x=Height, y=Weight)) + geom_bin2d() + facet_wrap(vars(Gender))

## Full model
bf_full <- lmBF(Weight ~ Height*Age*Gender, data=data)
bf_full

## Main effects only
bf_main <- lmBF(Weight ~ Height+Age+Gender, data=data)
bf_main

## Strong evidence in favour of full model
bf_main/bf_full

## Remove age from interaction
bf_amain <- lmBF(Weight ~ Age + Height*Gender, data=data)
bf_amain

## Good evidence in favour of simpler model
bf_amain/bf_full

post_amain <- posterior(bf_amain, iterations = 100000, thin=100)
summary(post_amain)

## compute HDI
hdi_amain <- hdi(post_amain)

## Plot posterior density and HDI
plot(density(post_amain[,'Height:Gender-Female']))
segments(hdi_amain[1, 'Height:Gender-Female'], 0, hdi_amain[2, 'Height:Gender-Female'], 0, lwd=2)

