# resampling a multiple regression
# unpublished data from Tigara collection AMNH
# Chin = thickness of the chin in midline (dependent response variable)
# Alv = depth of midline alveolar recess (predictor variable)
# BiC = bicanine breadth (predictor variable)
# RamHt = ramus height (predictor variable)
# BgonBr = gonion-gonion chord (predictor variable)
# ML = mandibular length (predictor variable)
# all units in mm

x <- read.csv("chins.csv")

# Step 1. Inspect data for collinearity, if r < 0.8 among pairs, OK 

cor(x[, c("Alv", "BiC", "RamHt", "BgonBr", "ML")])
# r < 0.8 among all pairs


# Step 2. Run an additive model for all included predictors
#         Model males and females separately

glmMale <- lm(Chin ~ Alv + BiC + RamHt + BgonBr + ML, data = x[x$Sex == "M", ])
glmFemale <- lm(Chin ~ Alv + BiC + RamHt + BgonBr + ML, data = x[x$Sex == "F", ])

summary(glmMale)
summary(glmFemale)

# Step 3. Remove the predictor with the highest P (most n.s) and rerun
#         (This predictor should be the same in both models)

# Removed BgonBr

glmMale2 <- lm(Chin ~ Alv + BiC + RamHt + ML, data = x[x$Sex == "M", ])
glmFemale2 <- lm(Chin ~ Alv + BiC + RamHt + ML, data = x[x$Sex == "F", ])

summary(glmMale2)
summary(glmFemale2)

# Step 4. Again rerun, removing predictor with highest P.

# Removed BiC

glmMale3 <- lm(Chin ~ Alv + RamHt + ML, data = x[x$Sex == "M", ])
glmFemale3 <- lm(Chin ~ Alv + RamHt + ML, data = x[x$Sex == "F", ])

summary(glmMale3)
summary(glmFemale3)

# Step 5. Resample to get a F confidence interval for the partial slope of ML
#         Get a F confidence interval for the intercept
#         Do M fall within CI?

set.seed(123)

getIntercept <- function(data) {
  sampledIndices <- sample(1:nrow(data), replace = TRUE)
  sampledData <- data[sampledIndices, ]
  model <- lm(Chin ~ Alv + RamHt + ML, data = sampledData)
  return(coef(model)[1])
}

femaleBootResultsIntercept <- replicate(1000, getIntercept(subset(x, Sex == "F")))

femaleCIIntercept<- quantile(femaleBootResultsIntercept, probs = c(0.025, 0.975))
print(femaleCIIntercept)

getSlopeML <- function(data) {
  sampledIndices <- sample(1:nrow(data), replace = TRUE)
  sampledData <- data[sampledIndices, ]
  model <- lm(Chin ~ Alv + RamHt + ML, data = sampledData)
  return(coef(model)["ML"])
}

femaleBootResultsML <- replicate(1000, getSlopeML(subset(x, Sex == "F")))

femaleCIML <- quantile(femaleBootResultsML, probs = c(0.025, 0.975))
print(femaleCIML)

maleModel <- lm(Chin ~ Alv + RamHt + ML, data = subset(x, Sex == "M"))
maleIntercept <- coef(maleModel)[1]
maleML <- coef(maleModel)["ML"] 


femaleCIIntercept
maleIntercept

femaleCIML
maleML


# Step 6.
# What do these results suggest about scaling of male and female chins?

# These results suggest that ML is similar between male and female chins.
# Because the ML&intercept value for males falls inside the female CI, these predictors may not be as trong as others in differentiating male and female chins. 
