##### Midterm #####

library(MASS)
library(e1071)
library(glmnet)
library(pls)
library(ggplot2)
library(reshape)
library(pheatmap)

##### Problem 1 #####

# Part vi.a

set.seed(1234)

lambdaTrue <- 3
n <- 10
B <- 1000

X <- rexp(n, rate = 1/lambdaTrue)
lambdaHat <- mean(X)
lambdaHatSquared <- lambdaHat^2

lambdaB2 <- numeric(B)

for (b in 1:B) {
  bootstrapSample <- rexp(n, rate = 1/lambdaHat)
  lambdaB2[b] <- (mean(bootstrapSample))^2
}

varLambdaB2 <- var(lambdaB2)

expectedVarianceParametric <- varLambdaB2

lambdaB2Nonparametric <- numeric(B)

for (b in 1:B) {
  bootstrapSampleNp <- sample(X, n, replace = TRUE) 
  lambdaB2Nonparametric[b] <- (mean(bootstrapSampleNp))^2
}

expectedVarianceNonparametric <- var(lambdaB2Nonparametric)

trueVarianceLambdaSquared <- (2 * (lambdaTrue^2)^2) / n

list(
  expectedVarianceParametric = expectedVarianceParametric,
  expectedVarianceNonparametric = expectedVarianceNonparametric,
  trueVarianceLambdaSquared = trueVarianceLambdaSquared
)



##### Problem 2 #####

problem2Train <- read.csv("Problem2training.csv")
problem2Test <- read.csv("Problem2testing.csv")

# Part i

## Logistic regression
lrModel2 <- glm(Y ~ ., data = problem2Train, family = binomial)
testPred = 1*(predict(lrModel2, newdata=problem2Test, type="response") > 0.5)
mean(testPred != (as.character(problem2Test$Y) == "1"))

## LDA
ldaModel2 <- lda(Y ~ ., data = problem2Train)
testPredLDA = as.character(predict(ldaModel2, newdata=problem2Test)$class)
mean(testPredLDA != as.character(problem2Test$Y))

## SVM with radial kernel
svmRadialModel2 <- svm(Y ~ ., data = problem2Train, kernel = "radial")
predSVM <- predict(svmRadialModel2, newdata = problem2Test)
predSVM <- factor(ifelse(predSVM > 0.5, "1", "0"), levels = c("0", "1"))
mean(predSVM != as.character(problem2Test$Y))


# Part ii

K = 10
set.seed(88970542)

problem2Train$Y <- factor(problem2Train$Y)
problem2Test$Y <- factor(problem2Test$Y)

errorMat = matrix(NA, K, 1)

optimalParams <- list()

for (k in 1 : K) {
  tune.svm = tune(svm, Y ~ ., 
                  data=problem2Train, kernel="polynomial",
                  ranges=list(cost=c(0.5, 1, 1.5, 2, 3, 4, 5, 8, 10, 15),
                              degree=c(1,2,3)))
  fit = tune.svm$best.model
  
  optimalParams[[k]] <- list(cost = fit$cost, degree = fit$degree)
  
  predSVM = as.character(predict(fit, problem2Test))
  
  errorMat[k,1] = mean(predSVM != as.character(problem2Test$Y))
}


optimalParams[[which.min(errorMat)]]$cost
optimalParams[[which.min(errorMat)]]$degree
min(errorMat)

## Heatmap

K = 10
set.seed(88970542)

problem2Train$Y <- factor(problem2Train$Y)
problem2Test$Y <- factor(problem2Test$Y)

costValues <- c(0.5, 1, 1.5, 2, 3, 4, 5, 8, 10, 15)
degreeValues <- c(1, 2, 3)

errorRates <- expand.grid(cost = costValues, degree = degreeValues)
errorRates$errorRate <- NA

for (i in 1:nrow(errorRates)) {
  cost <- errorRates$cost[i]
  degree <- errorRates$degree[i]
  
  foldErrors <- numeric(K)  
  
  for (k in 1:K) {
    tune.svm <- tune(svm, Y ~ ., 
                     data = problem2Train, kernel = "polynomial",
                     ranges = list(cost = cost, degree = degree))
    
    fit <- tune.svm$best.model
    predSVM <- as.character(predict(fit, problem2Test))
    foldErrors[k] <- mean(predSVM != as.character(problem2Test$Y))
  }
  
  errorRates$errorRate[i] <- mean(foldErrors)  
}

ggplot(errorRates, aes(x = factor(degree), y = factor(cost), fill = errorRate)) +
  geom_tile() +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "Impact of Cost and Degree on Error Rate",
       x = "Degree",
       y = "Cost",
       fill = "Error Rate") +
  theme_minimal()


# Part iii

allPredictors <- paste0("X.", 1:37)
selectedPredictors <- character(0)
previousErrorRate <- Inf  

evaluateQda <- function(data, predictors) {
  model <- qda(Y ~ ., data = data[, c("Y", predictors)])
  predictions <- predict(model)$class
  mean(predictions != data$Y)  
}

while (length(allPredictors) > 0) {
  errorRates <- sapply(allPredictors, function(p) {
    evaluateQda(problem2Train, c(selectedPredictors, p))
  })
  
  bestPredictor <- allPredictors[which.min(errorRates)]
  bestErrorRate <- min(errorRates)
  
  if (bestErrorRate >= previousErrorRate) break
  
  selectedPredictors <- c(selectedPredictors, bestPredictor)
  allPredictors <- setdiff(allPredictors, bestPredictor)
  previousErrorRate <- bestErrorRate
}

finalModel <- qda(Y ~ ., data = problem2Train[, c("Y", selectedPredictors)])
testErrorRate <- mean(predict(finalModel, newdata = problem2Test)$class != problem2Test$Y)

selectedPredictors
testErrorRate


##### Problem 3 #####

problem3Train <- read.csv("Problem3training.csv")
problem3Test <- read.csv("Problem3testing.csv")

# Part i

corrMatrix <- cor(problem3Train[, -which(names(problem3Train) == "Y")])
meltedCorr <- melt(corrMatrix)

ggplot(data = meltedCorr, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       limit = c(-1, 1), name = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),  
        axis.ticks = element_blank()) +
  labs(title = "Correlation Matrix Heatmap", x = "Predictors", y = "Predictors")


# Part ii

set.seed(88970542)

lassoCV <- cv.glmnet(x = as.matrix(problem3Train[, 2:203]), y = problem3Train[, 1], alpha = 1)
bestLamdba <- lassoCV$lambda.min
lassoPred <- predict(lassoCV, s = bestLamdba, newx = as.matrix(problem3Test[, 2:203]))
lassoMSE <- mean((lassoPred - problem3Test[, 1])^2)
lassoMSE

ridgeCV <- cv.glmnet(x = as.matrix(problem3Train[, 2:203]), y = problem3Train[, 1], alpha = 0)
bestLamdba <- ridgeCV$lambda.min
ridgePred <- predict(ridgeCV, s = bestLamdba, newx = as.matrix(problem3Test[, 2:203]))
ridgeMSE <- mean((problem3Test[, 1] - ridgePred)^2)
ridgeMSE


plsModel3 <- plsr(Y ~ ., data = problem3Train, ncomp = 10)
plsPred <- predict(plsModel3, problem3Test[, 2:203])
plsMSE <- mean((problem3Test[, 1] - plsPred)^2)
plsMSE


pcrModel3 <- pcr(Y ~ ., data = problem3Train, ncomp = 10)
pcrPred <- predict(pcrModel3, problem3Test[, 2:203])
pcrMSE <- mean((problem3Test[, 1] - pcrPred)^2)
pcrMSE

# Part iii

set.seed(88970542)  
lassoCV <- cv.glmnet(x = as.matrix(problem3Train[, 2:203]), y = problem3Train[, 1], alpha = 1)

bestLambda <- lassoCV$lambda.min

oneSElambda <- lassoCV$lambda.1se

coefBest <- as.matrix(coef(lassoCV, s = "lambda.min"))
selectedBest <- rownames(coefBest)[coefBest != 0]

coefOneSE <- as.matrix(coef(lassoCV, s = "lambda.1se"))
selectedOneSE <- rownames(coefOneSE)[coefOneSE != 0]

selectedBest
selectedOneSE

# Part v

set.seed(451)

X <- as.matrix(problem3Train[, -1]) 

n <- 100 
p <- ncol(X)

generateSparseData <- function(X) {
  k <- floor(0.2 * p) 
  betaSparse <- c(rnorm(k), rep(0, p - k)) 
  betaSparse <- sample(betaSparse)
  
  XSparse <- mvrnorm(n, mu = colMeans(X), Sigma = cov(X))
  epsilon <- rnorm(n, 0, 1)
  YSparse <- XSparse %*% betaSparse + epsilon
  
  list(X = XSparse, Y = YSparse, beta = betaSparse)
}

generateDenseData <- function(X) {
  betaDense <- rnorm(p)
  
  XDense <- mvrnorm(n, mu = colMeans(X), Sigma = cov(X))
  epsilon <- rnorm(n, 0, 1)
  YDense <- XDense %*% betaDense + epsilon
  
  list(X = XDense, Y = YDense, beta = betaDense)
}

sparseData <- generateSparseData(X)
denseData <- generateDenseData(X)

estimateModels <- function(X, Y, trueBeta) {
  
  cvLasso <- cv.glmnet(X, Y, alpha = 1)
  lassoBeta <- as.vector(coef(cvLasso, s = "lambda.min"))[-1] 
  
  cvRidge <- cv.glmnet(X, Y, alpha = 0)
  ridgeBeta <- as.vector(coef(cvRidge, s = "lambda.min"))[-1] 
  
  pcrModel <- prcomp(X, center = TRUE, scale. = TRUE)
  cumvar <- cumsum(pcrModel$sdev^2) / sum(pcrModel$sdev^2)
  numComponents <- which(cumvar >= 0.8)[1]
  
  pcrFit <- lm(Y ~ pcrModel$x[, 1:numComponents])
  pcrBeta <- coef(pcrFit)[-1]

  lassoCoeffError <- sum((lassoBeta - trueBeta)^2)
  ridgeCoeffError <- sum((ridgeBeta - trueBeta)^2)

  pcrCoeffError <- sum((c(pcrBeta, rep(0, length(trueBeta) - length(pcrBeta))) - trueBeta)^2)
  
  lassoPredError <- sum((X %*% lassoBeta - Y)^2)
  ridgePredError <- sum((X %*% ridgeBeta - Y)^2)
  
  pcrPred <- predict(pcrFit, newdata = data.frame(pcrModel$x[, 1:numComponents]))  
  pcrPredError <- sum((pcrPred - Y)^2)
  
  list(
    coeffErrors = c(lassoCoeffError, ridgeCoeffError, pcrCoeffError),
    predErrors = c(lassoPredError, ridgePredError, pcrPredError)
  )
}

sparseResults <- estimateModels(sparseData$X, sparseData$Y, sparseData$beta)
denseResults <- estimateModels(denseData$X, denseData$Y, denseData$beta)

resultsSparse <- data.frame(
  Method = c("Lasso", "Ridge", "PCR"),
  CoeffError = sparseResults$coeffErrors,
  PredError = c(sparseResults$predErrors[1], sparseResults$predErrors[2], sparseResults$predErrors[3])
)

resultsDense <- data.frame(
  Method = c("Lasso", "Ridge", "PCR"),
  CoeffError = denseResults$coeffErrors,
  PredError = c(denseResults$predErrors[1], denseResults$predErrors[2], denseResults$predErrors[3])
)


resultsSparse
resultsDense






