
########### 4. Obtain the data ###########
install.packages("kernlab")
library(kernlab)
data(spam)

########## 5. Clean the data ##############

#give it a starting point so you can repeat it
set.seed(3435)

# Divide the test group into test and train sets

#create a vector the size of the data set with either 0 or 1
trainIndicator<-rbinom(4601, size = 1, prob = 0.5)

#show how many in each group
table(trainIndicator)

# 1 = training set
trainSpam <- spam[trainIndicator == 1,]

#2 = test set
testSpam <- spam[trainIndicator ==0,]

#############6. Exploratory Analysis #############

#what variables do we have?
names(trainSpam)

#what do the top 10 records look like?
head(trainSpam)

#how many spam?
table(trainSpam$type)

#look at the data

#average capital letters in spam cs non spam
plot(trainSpam$capitalAve ~ trainSpam$type)

#Make it pretty
plot(log10(trainSpam$capitalAve+1)~trainSpam$type)

plot(log10(trainSpam[,1:4] +1 ))

#Clustering

hCluster <- hclust(dist(t(trainSpam[,1:57])))
plot(hCluster)

hClusterUpdated <- hclust(dist(t(log10(trainSpam[,1:55]+1))))
plot(hClusterUpdated)

################# 7. Statistical prediction/modeling ############

#transform
trainSpam$numType = as.numeric(trainSpam$type) - 1

#generate predictive model
costFunction <- function(x,y) sum(x !=(y>0.5))
cvError<-rep(NA,55)
library(boot)
for(i in 1:55){
  lmFormula <- reformulate(names(trainSpam)[i], response = "numType")
  glmFit <- glm(lmFormula, family = "binomial", data = trainSpam)
  cvError[i] = cv.glm(trainSpam, glmFit, costFunction, 2)$delta[2]
}

names(trainSpam)[which.min(cvError)]

#get measure of uncertainty

## Use the best model from the group
predictionModel<-glm(numType ~ charDollar, family = "binomial", data = trainSpam)

#get predictions on the test set
predictionTest <-predict(predictionModel, testSpam)
predictedSpam <- rep("nonspam", dim(testSpam)[1])

#Classify prob>0.5 as spam

predictedSpam[predictionModel$fitted>0.5] = "spam"

#classification table
table(predictedSpam, testSpam$type)

##Error rate
(61+458)/(1346 + 458 + 61 + 449)
