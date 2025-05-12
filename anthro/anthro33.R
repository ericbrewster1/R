# EAD f24 Final Question 6

library(FamilyRank)

set.seed(500)

x <- rbinorm(n=1000000, mean1=10, mean2=20, sd1=3, sd2=1, prop=.7)

hist(x)

# H0: type I error of t-test for this nonnormal distribution is > than 5%
# determine the type 1 error rate of a conventional 2-sample t-test
# sample 1 should be N=20 and sample 2 should be N=35

type1Error <- 0
tCI <- list()

for(i in 1:1000) {
  group1 = sample(x, size = 20)
  group2 = sample(x, size = 35)
  
  tTest <- t.test(group1, group2)
  
  tCI[[i]] <- tTest$conf.int
  
  if(tTest$p.value < 0.05) {
    type1Error <- type1Error + 1
  }
}

type1Error / 1000

# Type 1 error rate is 5.2%


# H1: Monte Carlo resampling of difference in sample means yields 
# narrower confidence intervals than those based on t-distribution 
# calculate CI of mean differences between samples using monte carlo approach
# again sample 1 is N=20 and sample 2 is N=35  

set.seed(500)
meanDifferences <- numeric(1000)

for(i in 1:1000) {
  group1 <- sample(x, size = 20)
  group2 <- sample(x, size = 35)
  
  meanDifferences[i] <- mean(group1) - mean(group2)
}

quantile(meanDifferences, probs = c(0.025, 0.975))

mean(sapply(tCI, function(ci) ci[1]))
mean(sapply(tCI, function(ci) ci[2]))

# T-test confidence interval average (from the 1000 trials) is (-2.892051, 3.043918)
# Monte Carlo resampling gives interval of (-2.750024  2.988999)
# Monte Carlo gives a smaller (although not by much) confidence interval


