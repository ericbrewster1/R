# Titanic survivors: did age matter?

library(resampledata)

head(Titanic)
t<-Titanic

# Step 1. Provide a graphic depiction of survivorship with respect to age.

library(ggplot2)

t <- as.data.frame(t)

ggplot(t, aes(x = factor(Survived), y = Age, fill = factor(Survived))) +
  geom_boxplot() +
  labs(x = "Survived (0 = No, 1 = Yes)", y = "Age", title = "Age Distribution of Titanic Survivors") +
  scale_fill_manual(values = c("orange", "blue"), labels = c("Nonsurvivor", "Survivor"))


# Step 2. Run a logistic regression to estimate the effect of age 
#         on survivorship. Interpret the model.

lrModel <- glm(Survived ~ Age, data = t, family = binomial)
summary(lrModel)
exp(coef(lrModel))
# In this model, the p-value for age is 0.00109, showing significant correlation between age and survivorship
# For each extra year of age, survivorship decreases by roughly 2.235% (1 - 0.9765)


# Step 3. Conduct a bootstrap procedure to estimate the mean age of 
#         survivors and nonsurvivors. Calculate a probability from this 
#         procedure that reflects the results of the logistic regression

set.seed(1)
t <- as.data.frame(t)

R <- 1000
bootMeans <- matrix(NA, nrow = R, ncol = 2) 

for (i in 1:R) {
  sampledIndices <- sample(1:nrow(t), replace = TRUE)
  sampledData <- t[sampledIndices, ]

  meanSurvivorAge <- mean(sampledData$Age[sampledData$Survived == 1], na.rm = TRUE)
  meanNonsurvivorAge <- mean(sampledData$Age[sampledData$Survived == 0], na.rm = TRUE)

  bootMeans[i, ] <- c(meanSurvivorAge, meanNonsurvivorAge)
}

meanSurvivorAge <- mean(bootMeans[, 1], na.rm = TRUE)
meanSurvivorAge <- mean(bootMeans[, 2], na.rm = TRUE)

meanSurvivorAge
meanNonsurvivorAge

probabilityGreater <- mean(bootMeans[, 1] > bootMeans[, 2])
probabilityGreater

# The probability of the the mean age of nonsurvivors being younger (based on bootstrap) is 0.001.
# This reflects the logistic regression, where lower age was significantly tied to survivorship

