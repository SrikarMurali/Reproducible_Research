library(kernlab)


data(spam)

#subsampling
set.seed(3231)
trainInd <- rbinom(4601, size = 1, prob = 0.5)
table(trainInd)


trainSpam <- spam[trainInd == 1,]
testSpam <- spam[trainInd == 0,]

names(trainSpam)
head(trainSpam)
table(trainSpam$type)

plot(trainSpam$capitalAve ~ trainSpam$type)
plot(log10(trainSpam$capitalAve + 1) ~ trainSpam$type)
plot(log10(trainSpam[, 1:4] + 1))

#clustering
hCluster <- hclust(dist(t(trainSpam[, 1:57])))
plot(hCluster)
hClustUpd <- hclust(dist(t(log10(trainSpam[, 1:55] + 1))))
plot(hClustUpd)

trainSpam$numType <- as.numeric(trainSpam$type) - 1
costFunc <- function(x,y) sum(x != (y > 0.5))
cvError <- rep(NA, 57)
library(boot)

for (i in 1:57) {
  lmForm <- reformulate(names(trainSpam)[i], response = 'numType')
  glmFit <- glm(lmForm, family = 'binomial', data = trainSpam)
  cvError[i] <- cv.glm(trainSpam, glmFit, costFunc, 2)$delta[2]
  
}

##which predictor is the best min cv error
names(trainSpam)[which.min(cvError)]

## use best model
predModel <- glm(numType ~ charDollar, family = 'binomial', data = trainSpam)

## get pred on test
predTest <- predict(predModel, testSpam)
predSpam <- rep('nonspam', dim(testSpam)[1])

## classify spam for those prob > 0.5
predSpam[predModel$fitted.values > 0.5] <- 'spam'


##table
table(predSpam, testSpam$type)

