library(rjags)

d <- read.csv("data/regression_data.csv")

jags <- jags.model('jags/simple_regression.jags',
                   data = list('x' = d$treatment,
                               'y' = d$observation,
                               'N' = nrow(d)),
                   n.chains = 4,
                   n.adapt = 1000)

update(jags, 1000)

jags.samples(jags,
             c('a', 'b'),
             1000)


