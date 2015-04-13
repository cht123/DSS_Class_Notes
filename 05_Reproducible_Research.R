install.packages("kernlab")
library(kernlab)
data(spam)

#perform subsampling
set.seed(3435)
trainIndicator <- rbinom(4601, size = 1, prob = 0.5)
table(trainIndicator)

trainSpam = spam[trainIndicator == 1, ]
testSpam = spam[trainIndicator == 0, ]

# exploratory analysis
plot(trainSpam$capitalAve ~ trainSpam$type)
# plot as log because the data is skewed, 1 is only added when there
# are a lot of zeros
plot(log10(trainSpam$capitalAve + 1) ~ trainSpam$type)
# pairswise plots
plot(log10(trainSpam[,1:4] + 1))


# hierarchical clustering
hCluster <- hclust(dist(t(trainSpam[, 1:57])))
plot(hCluster)

# hierarchical clustering updated to fix the skewness of the data
hClusterUpdated <- hclust(dist(t(log10(trainSpam[, 1:57] + 1))))
plot(hClusterUpdated)

# create logistical model
trainSpam$numType <- as.numeric(trainSpam$type) - 1
costFunction = function(x,y) sum(x != (y > 0.5))
cvError <- rep(NA, 55)
library(boot)
for (i in 1:55) {
  lmFormula = reformulate(names(trainSpam)[i], response = "numType")
  glmFit = glm(lmFormula, family = "binomial", data = trainSpam)
  cvError[i] = cv.glm(trainSpam, glmFit, costFunction, 2)$delta[2]
}

# get the model with the best fit
names(trainSpam)[which.min(cvError)]

# use the best model
predictionModel = glm(numType ~ charDollar, family = 'binomial', data = trainSpam)

# get predictions on the test set
predictionTest = predict(predictionModel, testSpam)
predictedSpam = rep("nospam", dim(testSpam)[1])

# classify as spam
predictedSpam[predictionModel$fitted > 0.5] = "spam"

# classification table
table(predictedSpam, testSpam$type)

#error rate
(61 + 458) / (1346 + 458 + 61 + 449)

