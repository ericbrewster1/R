# w10.2 probability of events: binomial & negative binomial resampling
# Type ?rbinom, ?rnbinom for information
# 
# The probability of a 1000 year flood is 0.001 in any given year

# use a resampling procedure or rbinom(), rnbinom() to answer:

# Riddle 1. How often can we expect thousand year floods in adjacent years
#           over a single millennium?

set.seed(4)
adjacents <- 0

for(i in 1:10000) {
  yearsData <- rbinom(1000, 1, 0.001)
  for(j in 1:999) {
    if(yearsData[j] == 1 && yearsData[j + 1] == 1) {
      adjacents <- adjacents + 1
    }
  }
}

adjacents
adjacents / 10000

# In 10000 trials of millenniums, there were adjacent floods in 12 of them - 0.0012 percent of the trials, near the original probability



# Riddle 2. What's the median wait time for seeing a 1000 year flood?

set.seed(4)
waitTimes <- numeric(10000)

for(i in 1:10000) {
  yearsData <- rbinom(1000, 1, 0.001)
  
  waitTime <- which(yearsData == 1)
  
  if(length(waitTime) == 0) {
    waitTimes[i] <- 1000
  }
  else {
    waitTimes[i] <- waitTime[1]
  }
}

median(waitTimes)
hist(waitTimes)


# The median number wait time for seeing a thousand year flood is 680 years. 
# There are many simulations with no flood, captured in the 1000 year block
# Shifting the rbinom up to 5000/10000 observations (years) gets a simlar result near 680



# Riddle 3. What's the probability for seeing at least 
#           2 1000-year floods in the same century?

set.seed(4)

floodsPerCentury <- numeric(10000)

for(i in 1:10000) {
  floodsPerCentury[i] <- sum(rbinom(100, 1, 0.001))
}

mean(floodsPerCentury >= 2)
# 0.0043
