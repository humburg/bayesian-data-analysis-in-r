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

jags.samples(jags,
             c('a', 'b1', 'b2'),
             1000)


