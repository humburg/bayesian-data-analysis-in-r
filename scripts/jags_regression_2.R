library(rjags)

d <- read.csv("data/regression_data.csv")

jags <- jags.model('jags/multiple_regression.jags',
                   data = list('x1' = d$treatment,
                               'x2' = d$covariate,
                               'y' = d$observation,
                               'N' = nrow(d)),
                   n.chains = 4,
                   n.adapt = 1000)

update(jags, 1000)

samples <- coda.samples(jags,
                        c('a', 'b1', 'b2'),
                        1000)


summary(lm(observation ~ treatment + covariate, data=d))


## With hyper priors
jags_hyp <- jags.model('jags/multiple_regression_hyper.jags',
                   data = list('x1' = d$treatment,
                               'x2' = d$covariate,
                               'y' = d$observation,
                               'N' = nrow(d)),
                   n.chains = 4,
                   n.adapt = 1000)

update(jags_hyp, 1000)

samples2 <- coda.samples(jags,
                        c('a', 'b1', 'b2'),
                        1000)


