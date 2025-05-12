#Is CAPHIL caseload randomly distributed throughout the year?

library(MASS)
library(boot)
library(CircStats)  # use circular package if desired as well
library(circular)

# cases aggregated over 5 years by month (jan through dec)

caphil<-c(36,29,34,33,31,23,29,12,30,19,24,26)

# Step 1. Run a chi-square test for goodness of fit

chisq.test(caphil)

# Step 2. convert counts to observations in radians
#         e.g., have 36 values for January,  
#         29 for February, etc. Each month is a unique direction

angles <- 2 * pi * (1:12 - 1) / 12

circularData <- unlist(mapply(rep, angles, caphil)) # unlist converts list to vector, mapply replicates angles based on caphil values

circularData <- circular(circularData)

plot.circular(circularData)

# Step 3. Determine if the data conform to 
# 1)uniform distribution, 2) von Mises distribution

rayleigh.test(circularData, mu = NULL)
pp.plot(circularData)
# Significant p-value and data points that appear to conform to Von-Mises


# Step 4. Is the mean "direction" significant?

mle.vonmises.bootstrap.ci(circularData,mu=NULL,bias=FALSE,alpha=0.05,reps=1000) 
# Yes, the mean direction is significant

# Step 5. If the answer to step 4 is yes, 
#         what is the mean direction?

mean(circularData)
# The mean among the values in circularData is 0.929 radians, refelcted in the roughly (0.2, 1.6) CI for the mean direction
# This mean direction indicates that CAPHIL cases are more concentrated in the first few months of the year


