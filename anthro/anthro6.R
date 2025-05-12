# w3.3  CI for a median
# Ungar's microwear data
# Comparing results from sample sizes with different N
# Asfc: microwear complexity
# Lsar: microwear anisotropy (directionality of wear)

x <- read.csv("hominin microwear fabrics.csv")

# Step 1. get a distribution of resampled medians 
#         for Lsar in afarensis 

lsar <- x[x$Taxon == "afarensis", ]

set.seed(88970542)
medianLsar <- numeric(100)
for(i in 1:100) {
  resample <- sample(lsar$Lsar, replace = TRUE)
  medianLsar[i] <- median(resample)
}

hist(medianLsar)

# Step 2. Specify the 95% confidence interval

quantile(medianLsar, probs = c(0.025, 0.975))
# 95% CI is (0.00223, 0.00405)

# Step 3. Do the three anamensis specimens fall within that interval?

anamensis <- x[x$Taxon == "anamensis", ]
# Yes, all three of the specimens do fall within that interval

# Step 4. Interpret. H0: fabrics are the same in afarensis and anamensis

# The three anamensis values (by extension, their medians when resampled) fall within the 95% confidence interval
# Thus, there is not significant evidence for the alternative hypothesis that the fabrics are different
# Here, we fail to reject the null
