# w6.3 exercise in pseudoreplication
# resampling can be done to any sample size desired
# resampling from a smaller to larger N is "pseudoreplication"
# 
# resampling from larger to smaller N is generally OK
# but you will necessarily lose power

# In this exercise, you will perform a bootstrap analysis of 
# correlation of body mass and center of mass 

# Step 1. Grab the data

x <- read.csv("rhesus.csv")

# Step 2. Get the correlation of body mass (Mass) to center of mass (CM). 
#         Save the p-value and correlation coefficients as objects

cor.test(x$Mass, x$CM)
(pValue <- cor.test(x$Mass, x$CM)$p.value)
(corCoeff <- cor.test(x$Mass, x$CM)$estimate)

# Step 3. Now bootstrap the data for the pairs of observations. Resample
# with replacement to a 5x larger N (35). Keep track of the correlation and P-values.

pValues <- numeric(1000)
corCoeffs <- numeric(1000)

for(i in 1:1000) {
  indices <- sample(1:7, 35, replace = TRUE)
  massResample <- x$Mass[indices]
  cmResample <- x$CM[indices]
  
  pValues[i] <- cor.test(massResample, cmResample)$p.value
  corCoeffs[i] <- cor.test(massResample, cmResample)$estimate
}

# Step 4. Compare the original correlation to the bootstrapped correlations

(corCoeff <- cor.test(x$Mass, x$CM)$estimate)
mean(corCoeffs)
# Correlation does not change very much


# Step 5  Compare the P-values from the bootstrap procedure to   
#         the P-value based on the original variates. 

(pValue <- cor.test(x$Mass, x$CM)$p.value)
mean(pValues)
# The p-value decreases to near 0


# Step 6. Comment on the effects of pseudoreplication

# The correlation remains similar, but significantly (artificially) decreases the p-value
# This occurs because the repeated data points are treated independently when they should not be (these are the same data points repeated)
# The sample size is artificially increased, mimicking the same trend from the original (much smaller) data


