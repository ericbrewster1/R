# Final Question 5
# simulating Neanderthal extinction

N <- 15000    # estimated neanderthal effective population size (Rogers et al. 2017)

g <- 20       # estimated group size(Hayden 2012)

N.g <- rep(20,750)   # all Neanderthal groups

low.p.c <- c(1, rep(0,99))  # improbable contact
hi.p.c <- c(1, rep(0,9))    # more likely contact

death.p <- 0.1      # prob of surviving infection after contact


# H0: In 5000 years neanderthals will never go extinct from disease
#     if the P of contact = 0.01 (lo.p.c)

# H1: Neanderthals will almost certainly perish by 40,000 years ago 
#     if the P of contact is = 0.1 (hi.p.c)


simulateExtinction <- function(probContact) {
  pop <- N.g
  extinct <- rep(FALSE, 750)
  
  for(year in 1:5000) {
    contact <- rbinom(750, size = 1, prob = probContact)
    
    for (i in 1:750) {
      if(!extinct[i] && contact[i] == 1) {
        deaths <- rbinom(1, size = pop[i], prob = death.p)
        pop[i] <- pop[i] - deaths
        if(pop[i] <= 0) {
          pop[i] <- 0
          extinct[i] <- TRUE
        }
      }
    }
  }
  
  list(finalPop = sum(pop), extinctGroups = sum(extinct))
}


set.seed(12345)

# bootstrap both

resultLow <- list()
resultHigh <- list()

# Doing 1000 iterations takes too long
for (i in 1:100) {
  resultLow[[i]] <- simulateExtinction(0.01)
  resultHigh[[i]] <- simulateExtinction(0.1)
}

# Note: used https://www.rdocumentation.org/packages/memisc/versions/0.99.31.8/topics/Sapply for help with sapply instead of for loop

### Low percentage of contact
 
mean(sapply(resultLow, function(x) x$finalPop))
mean(sapply(resultLow, function(x) x$extinctGroups))

sum(sapply(resultLow, function(x) x$finalPop == 0))


### High percentage of contact

mean(sapply(resultHigh, function(x) x$finalPop))
mean(sapply(resultHigh, function(x) x$extinctGroups))

sum(sapply(resultHigh, function(x) x$finalPop == 0))


# With the low percentage of contact, average final population was near 100, ~660 went extinct,
# and none of the 100 simulations had full extinction

# In high percentage of contact, average final population was 0, 750 groups went extinct
# That is, full extinction in each of the 100 simulations
