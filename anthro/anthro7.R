# exercise w4.1

# Demonstration of bootstrap estimation

# Step 1: create normal, skewed and uniform distributions

# use the following code to generate your distributions

norml <- rbeta(10000,5,5)     # a normal distribution
lskew <- rbeta(10000,5,2)     # skewed, long left tail
rskew <- rbeta(10000,2,5)     # skewed, long right tail
unifo <- rbeta(10000,1,1)     # uniform distribution

# Step 2: plot histograms of each

hist(norml)
hist(lskew)
hist(rskew)
hist(unifo)

# Step 3: calculate mean, median and standard deviation 
#         for each distribution, save as objects

(meanNorm <- mean(norml))
(medianNorm <- median(norml))
(sdNorm <- sd(norml))

(meanLskew <- mean(lskew))
(medianLskew <- median(lskew))
(sdLskew <- sd(lskew))

(meanRskew <- mean(rskew))
(medianRskew <- median(rskew))
(sdRskew <- sd(rskew))

(meanUnifo <- mean(unifo))
(medianUnifo <- median(unifo))
(sdUnifo <- sd(unifo))


# Step 4: conduct bootstrap resampling estimates 
#         for each statistic for each distribution.

meanNormBoot <- NULL
medianNormBoot <- NULL
sdNormBoot <- NULL
for(i in 1:1000) {meanNormBoot[i] <- mean(sample(norml, 10000, replace = TRUE))}
for(i in 1:1000) {medianNormBoot[i] <- median(sample(norml, 10000, replace = TRUE))}
for(i in 1:1000) {sdNormBoot[i] <- sd(sample(norml, 10000, replace = TRUE))}

meanLskewBoot <- NULL
medianLskewBoot <- NULL
sdLskewBoot <- NULL
for(i in 1:1000) {meanLskewBoot[i] <- mean(sample(lskew, 10000, replace = TRUE))}
for(i in 1:1000) {medianLskewBoot[i] <- median(sample(lskew, 10000, replace = TRUE))}
for(i in 1:1000) {sdLskewBoot[i] <- sd(sample(lskew, 10000, replace = TRUE))}

meanRskewBoot <- NULL
medianRskewBoot <- NULL
sdRskewBoot <- NULL
for(i in 1:1000) {meanRskewBoot[i] <- mean(sample(rskew, 10000, replace = TRUE))}
for(i in 1:1000) {medianRskewBoot[i] <- median(sample(rskew, 10000, replace = TRUE))}
for(i in 1:1000) {sdRskewBoot[i] <- sd(sample(rskew, 10000, replace = TRUE))}

meanUnifoBoot <- NULL
medianUnifoBoot <- NULL
sdUnifoBoot <- NULL
for(i in 1:1000) {meanUnifoBoot[i] <- mean(sample(unifo, 10000, replace = TRUE))}
for(i in 1:1000) {medianUnifoBoot[i] <- median(sample(unifo, 10000, replace = TRUE))}
for(i in 1:1000) {sdUnifoBoot[i] <- sd(sample(unifo, 10000, replace = TRUE))}

#             
# Step 5: summarize results by graphical means
#         e.g., histograms, boxplots or other means

hist(meanNormBoot)
hist(medianNormBoot)
hist(sdNormBoot)

hist(meanLskewBoot)
hist(medianLskewBoot)
hist(sdLskewBoot)

hist(meanRskewBoot)
hist(medianRskewBoot)
hist(sdRskewBoot)

hist(meanUnifoBoot)
hist(medianUnifoBoot)
hist(sdUnifoBoot)

# Step 6: Answer the following:
#         Does the distribution of the bootstrap estimate reflect the
#         distribution from which it was created?
#         i.e., do skewed distributions yield skewed bootstrap estimates

# No, the distribution of the estimate does not reflect the distribution from which it was created.
# Instead, the distributions appear generally normal, centered around the true statistic.

#         
# Step 7: based on your bootstrap procedures calculate a confidence
#         interval (CI) for the standard deviation for each distribution.
#         Does the average standard deviation of the skewed and uniform 
#         distributions fall within the CI of the normal distribution?

quantile(sdNormBoot, probs = c(0.025, 0.975))
quantile(sdLskewBoot, probs = c(0.025, 0.975))
quantile(sdRskewBoot, probs = c(0.025, 0.975))
quantile(sdUnifoBoot, probs = c(0.025, 0.975))

# No, the average standard deviation of the skewed and uniform distributions does not fall within the CI of the normal.

