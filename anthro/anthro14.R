# exercise 6.2
# Data from Isler 2008
# body mass and basal metabolic rate in 45 primate species

g <- c(1021,4670,557,356,138,733,225,500,850,3750,3400,8500,3000,
       7100,9580,6225,9500,14317,16900,10450,34150,16200,1900,
       60500,173,113,2590,300,54,3350,2136,2719,2510,3512,693,
       262,172,275,68,1039,950,206,284,1160,964)  #body mass in g

bmr <- c(456,2000,278,154,89,382,234,450,592,1605,1799,3392,1068,
         3458,4192,2239,2940,5784,5147,2978,9000,4941,1071,12457,
         149,77,1384,195,42,670,345,435,490,604,225,216,137,198,
         63,523,412,131,128,273,327) # bmr in mlO2/hr

# Step 1 calculate a rank correlation for the sample

cor.test(g, bmr)
# Correlation coefficient of 0.9637337 (very close to 1, nearly perfectly positive)


# Step 2 calculate a confidence interval for rho (the Spearman coefficient)

rho <- numeric(1000)

for(i in 1:1000) {
  indices <- sample(1:45, replace = TRUE)
  rho[i] <- cor(g[indices], bmr[indices], method = "spearman")
}

quantile(rho, c(0.025, 0.975))


# Step 3 resample by shuffling bmr and leave g as is, 
# and estimate rho as in step 2

# Comment on the resulting estimates of rho and what that tells you

rho <- numeric(1000)

for(i in 1:1000) {
  bmrShuffle <- sample(bmr, replace = TRUE)
  rho[i] <- cor(g, bmrShuffle, method = "spearman")
}

quantile(rho, c(0.025, 0.975))

# The shuffled bmr values yield a 95% confidence interval including 0 (no correlation), far from the near 1 correlation above
# This shows that the correlation from the spearman coefficient is significant- correlation likely not by chance
