# exercise w6.1
# Data from Isler (2008)
# x is gestation length (days) in 27 primate species
# y is lactation length (days) in the same species

x <- c (133,186,152,144,137,129,171,176,167,158,232,178,158,62,60,
        159,123,136,102,135,124,110,132,134,168,191,193)

y <- c(76,502,65,77,90,90,243,183,264,334,1702,68,213,45,45,183,
       183,180,146,75,84,53,140,115,170,145,150)


# Step 1: Resample from the data to create a confidence interval
# for the regression estimate for the slope. Also resample the data 
# to create a confidence interval for the correlation coefficient

slopes <- numeric(1000)
correlation <- numeric(1000)

for(i in 1:1000) {
  indices <- sample(1:27, replace = TRUE)
  xResample <- x[indices]
  yResample <- y[indices]
  
  fit <- lm(yResample ~ xResample)
  slopes[i] <- coef(fit)[2]
  correlation[i] <- cor(xResample, yResample)
}

quantile(slopes, c(0.025, 0.975))
quantile(correlation, c(0.025, 0.975))


# Step 2: Evaluate if the covariance of x and y is positive, negative, or zero

cov(x, y)
# Covariance is positive

# Step 3: Does the observed slope indicate a linear relationship between x and y?

summary(lm(y ~ x))
# The observed slope (4.845) has a p-value 0.0015, indicating a linear relationship between x and y
# From covariance and slope, as x increases, so too does y


# Step 4: calculate a percent prediction error for each data point in the original data set.
# This involves subtracting the fitted value from the observed value, then divide that difference 
# by the fitted value, then multiply by 100 to get a percentage. 
# Report the maximum (most positive) and minimum most negative) prediction errors.
# NOTE: this should not be done on log-transformed data as the resulting percentages are misleading.

predictedValues <- predict(lm(y ~ x))
errors <- ((y - predictedValues) / predictedValues) * 100
max(errors)
min(errors)

# Step 5: comment on the ability of x to predict y in this sample

# X seems to be able to predict Y well given the results above.
# First, the correlation coefficient has a 95% CI that tends be near (0.40, 0.80), indicating a positive correlation
# This is reinforced by the positive covariance and positive slope
# Also, the slope has a small p-value of 0.0015, indicating that it is very predicitve of Y
