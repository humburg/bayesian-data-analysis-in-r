## simulated accuracy and reaction time data based on Davidson and Martin (2013).
## Accuracy individual item responses are generated with a negative relationship
## between reaction time and accuracy.

## Simulation 1
library(arm)

set.seed(20180829)

genData <- function(n){
  ## standardized log-transformed RTs
  x1 <- rnorm(2*n, 0, 1)
  
  ## Condition for each item
  x2 <- rep(c(0, 1), each=n)
  
  ## Intercepts
  ## Population level intercept
  b0_pop <- 0
  ## Group level sd of intercepts (subject random effect)
  b0_sd <- 1
  ## Subject specific intercept
  b0 <- rnorm(1, b0_pop, b0_sd)
  
  ## Negative effect of RT on accuracy
  ## Population level effect of RT
  b1_pop <- -2
  ## Group level sd of slopes
  b1_sd <- 1
  b1 <- rnorm(1, -b1_pop, b1_sd)
  
  ## Effect of condition is correlated with individual
  ## RT/accuracy trade-off
  b2 <- rnorm(1, b1, 1)
  
  r1 <- rbinom(n, 1, invlogit(b0 + b1*x1 + b2*x2))
  
  data.frame(RT=x1, Rsp=r1, Cond=x2)
}

## Set parameters
Nsbj <- 16
Nrsp <- 100

## Generate response times and responses
respRT <- replicate(Nsbj, genData(Nrsp), simplify=FALSE)
respRT <- do.call(rbind, respRT)
respRT$Sbj <- paste0('s',rep(1:Nsbj, each=Nrsp*2))

write.csv(respRT, file="data/mixed_sim2.csv")