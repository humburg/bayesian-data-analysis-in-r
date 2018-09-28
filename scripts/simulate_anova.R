## simulate data from a 2x2 design

set.seed(20180928)
group <- rep(c(-1,1), each=40)
treat <- rep(c(-1,1), times=40)

intercept <- 5
group_b <- 0.5
treat_b <- 0.6
interact_b <- 0.3

obs <- intercept + group_b*group + treat_b*treat + 
      interact_b*group*treat + rnorm(length(group), 0, sqrt(0.6))

data <- data.frame(group=paste("group", (group+1)/2+1), treatment=paste("treatment", (treat+1)/2+1), observation=obs)
write.csv(data, file="data/anova_data.csv")
