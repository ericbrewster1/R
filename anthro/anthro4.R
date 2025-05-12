# w3.1 estimate the variance of a population

library(openintro)   # package required to access the data

ncbirths     # sample from 2004 births: type ?ncbirths for information

# There are two variables of interest: 
# weight (at birth, in pounds)
# the factor habit: "smoker", "nonsmoker"

# Step 1 estimate the variance of weight in the population from this sample

(varWeight <- var(ncbirths$weight))

# Step 2 estimate variance in weight of babies
# born to smoking and nonsmoking mothers separately

(varSmoker <- var(ncbirths$weight[ncbirths$habit == 'smoker'], na.rm = TRUE))
(varNonSmoker <- var(ncbirths$weight[ncbirths$habit == 'nonsmoker'], na.rm = TRUE))

# Step 3
# Is the variance difference between habits a sampling error?
# (i.e., smaller N for smokers)
# Or a real difference in the variation in birthweight?

(numNonSmokers <- sum(ncbirths$habit == 'nonsmoker', na.rm = TRUE))
(numSmokers <- sum(ncbirths$habit == 'smoker', na.rm = TRUE))

# sample the nonsmoker sample to the N of the smokers
# do this 10 or more times and record the results
# do you think the difference in variance between 
# smokers/nonsmokers is meaningful?

set.seed(88970542)
samplingNonsmokers <- replicate(20, var(sample(ncbirths$weight[ncbirths$habit == 'nonsmoker'], 126, replace=TRUE), na.rm = TRUE))
(avgNonSmokerVar <- mean(samplingNonsmokers))
avgNonSmokerVar <- as.numeric(avgNonSmokerVar)
# Taking 20 samples of 126 nonsmoking weights, the average variance is 2.2176, about 0.1 away from the non-smoker variance found above.
avgNonSmokerVar/ sqrt(126)
# Given that the average variance divided by the root of the number of samples is less than the difference between the 20 sample nonsmoker and the smoker variance,
# I think the difference between the two is meaningful.