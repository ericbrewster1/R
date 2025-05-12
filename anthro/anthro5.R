# w3.2
# Which is more likely in terms of birth order?
# B G B G B G
# G B B G B G
# G G G G G G

# Step 1: define three objects 
# one for each birth order under evaluation
# a vector of length 2 to resample boys and girls from (only valid if P = 0.5)

order1 <- c("B", "G", "B", "G", "B", "G")
order2 <- c("G", "B", "B", "G", "B", "G")
order3 <- c("G", "G", "G", "G", "G", "G")

resampleVector <- c("B", "G")

# Step 2: create and run a simulation a bunch of times
#         use a for loop and/or the replicate() function
#         use identical() function to find matches of birth orders

count1 = 0
count2 = 0
count3 = 0

set.seed(88970542)
for (i in 1: 1000000) {
  simOrder <- sample(resampleVector, 6, replace = TRUE)
  if(identical(simOrder, order1)) {
    count1 <- count1 + 1
  }
  if(identical(simOrder, order2)) {
    count2 <- count2 + 1
  }
  if(identical(simOrder, order3)) {
    count3 <- count3 + 1
  }
}
count1
count2
count3

# Step 3:  how likely is GGGGGG among ALL possible birth orders?
#         use the permutations() function from the gtools package

library(gtools)
allPermutations <- permutations(2, 6, resampleVector, repeats.allowed = TRUE)
allPermutationVector <- apply(allPermutations, 1, paste, collapse = '') # convert each perm to set of characters
(matchesG <- sum(allPermutationVector == "GGGGGG") / length(allPermutationVector))

# 1/64 chance, or 1.5625% chance


# Step 4: If the true probability of a girl is not 0.5
#         but 0.49, what is the revised P of GGGGGG?
#         use the dbinom() function or run another simulation

dbinom(6, 6, 0.49)
# 1.384% chance

# Step 5: Could you detect the difference between these Ps 
#         in 100 birth order samples of 6 babies each?

simCounts <- function(probabilityGirl) {
  countG <- 0
  for (i in 1: 100) {
    simOrder <- sample(resampleVector, 6, replace = TRUE, prob = c(1 - probabilityGirl, probabilityGirl))
    if(identical(simOrder, order3)) {
      countG <- countG + 1
    }
  }
  return(countG)
}

set.seed(25)
(countG05 <- simCounts(0.5))
(countG049 <- simCounts(0.49))

# Cannot detect a difference in 100 birth order samples

