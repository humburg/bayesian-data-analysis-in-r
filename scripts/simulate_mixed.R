## simulated accuracy and reaction time data based on Davidson and Martin (2013).
## Accuracy individual item responses are generated with a negative relationship
## between reaction time and accuracy.

## Simulation 1
library(arm)

set.seed(20180828)

genData <- function(n){
  ## standardized log-transformed RTs
  x1 <- rnorm(n, 0, 1)
  
  ## Intercepts
  ## Population level intercept
  b0_pop <- 2
  ## Group level sd of intercepts (subject random effect)
  b0_sd <- 1
  ## Subject specific intercept
  b0 <- rnorm(1, b0_pop, b0_sd)
  
  ## Negative effect of RT on accuracy
  ## Group level sd of slopes
  b1_sd <- 1
  b1 <- rnorm(1, -b0, b1_sd)
  r1 <- rbinom(n, 1, invlogit(b0 + b1*x1))
  
  data.frame(RT=x1, Rsp=r1)
}

## Set parameters
Nsbj <- 16
Nrsp <- 100

## Generate response times and responses
respRT <- replicate(Nsbj, genData(Nrsp), simplify=FALSE)
respRT <- do.call(rbind, respRT)
respRT$Sbj <- paste0('s',rep(1:Nsbj, each=Nrsp))

write.csv(respRT, file="bayesian-data-analysis-in-r/data/mixed_sim1.csv")