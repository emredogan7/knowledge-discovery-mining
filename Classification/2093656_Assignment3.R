

###

setwd("/Users/emre/Desktop/Assignment 3")     # set working directory

#check if package is already installed. If not, install the package
if("mRMRe" %in% rownames(installed.packages())==FALSE){
  install.packages("mRMRe")
}
if("FSelector" %in% rownames(installed.packages())==FALSE){
  install.packages("FSelector")
}
if("discretization" %in% rownames(installed.packages())==FALSE){
  install.packages("discretization")
}
if("arules" %in% rownames(installed.packages())==FALSE){
  install.packages("arules")
}
if("arulesViz" %in% rownames(installed.packages())==FALSE){
  install.packages("arulesViz")
}
if("beanplot" %in% rownames(installed.packages())==FALSE){
  install.packages("beanplot")
}
if("car" %in% rownames(installed.packages())==FALSE){
  install.packages("car")
}
if("randomForest" %in% rownames(installed.packages())==FALSE){
  install.packages("randomForest")
}
if("class" %in% rownames(installed.packages())==FALSE){
  install.packages("class")
}
if("gmodels" %in% rownames(installed.packages())==FALSE){
  install.packages("gmodels")
}
if("caret" %in% rownames(installed.packages())==FALSE){
  install.packages("caret")
}
if("party" %in% rownames(installed.packages())==FALSE){
  install.packages("party")
}


#load the package
require("mRMRe")
require("caret")
require("e1071")
require("discretization")
require("arules")
require("arulesViz")
require("car")
require("randomForest")
require(foreign)
library(gmodels)
require(ggplot2)
library(class)
library(gridExtra)
library(knitr)
library(party)

data_test=read.csv("./test.csv")
data_train=read.csv("./train.csv")

###

head(data_test)
summary(data_test)
summary(data_train)


###

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }

data_train_norm <- as.data.frame(lapply(data_train[1:8], normalize))



#prc_train <- prc_n[1:65,]
#prc_test <- prc_n[66:100,]


train_labels = data_train[, 9]
test_labels = data_test[, 9] 
test_labels = as.data.frame(test_labels)



data_test1 <- data_test[, 1:8]
data_train1 <- data_train_norm[, 1:8]


test_prediction_knn1 <- knn(train = data_train1, test = data_test1,cl = train_labels, k=1)
test_prediction_knn5 <- knn(train = data_train1, test = data_test1,cl = train_labels, k=5)
test_prediction_knn11 <- knn(train = data_train1, test = data_test1,cl = train_labels, k=11)
test_prediction_knn101 <- knn(train = data_train1, test = data_test1,cl = train_labels, k=101)


prediction_knn1 = knn(data_train, data_test, cl = train_labels, k = 1)
prediction_knn5 = knn(data_train, data_test, cl = train_labels, k = 5)
prediction_knn10 = knn(data_train, data_test, cl = train_labels, k = 10)
prediction_knn100 = knn(data_train, data_test, cl = train_labels, k = 100)


accuracy_knn1 = sum(prediction_knn1 == data_test$Y1)/length(data_test$Y1)
accuracy_knn5 = sum(prediction_knn5 == data_test$Y1)/length(data_test$Y1)
accuracy_knn10 = sum(prediction_knn10 == data_test$Y1)/length(data_test$Y1)
accuracy_knn100 = sum(prediction_knn100 == data_test$Y1)/length(data_test$Y1)

#precision = xtab[1,1]/sum(xtab[,1])
#recall = xtab[1,1]/sum(xtab[1,])
#f = 2 * (precision * recall) / (precision + recall)
cat(paste("Accuracy:\t", format(accuracy_knn1 ), "\n",sep=" "))

cat(paste("Accuracy:\t", format(accuracy_knn5 ), "\n",sep=" "))

cat(paste("Accuracy:\t", format(accuracy_knn10 ), "\n",sep=" "))

cat(paste("Accuracy:\t", format(accuracy_knn100 ), "\n",sep=" "))




###########################################################################

require(e1071) #Contains the SVM 
data_test=read.csv("./test.csv")
data_train=read.csv("./train.csv")

data_test1 <- data_test[, 1:8]
data_train1 <- data_train_norm[, 1:8]

# create model
model_svm0.05 <- svm(Y1~. , data=data_train,type="C-classification", cost = 0.05)
summary(model_svm0.05)
svm0.05_preds <- predict(model_svm0.05, data_test1) 
accuracy_svm0.05 = sum(svm0.05_preds == data_test$Y1)/length(data_test$Y1)

model_svm1 <- svm(Y1~. , data=data_train,type="C-classification", cost = 1)
summary(model_svm1)
svm1_preds <- predict(model_svm1, data_test1) 
accuracy_svm1 = sum(svm1_preds == data_test$Y1)/length(data_test$Y1)

model_svm5 <- svm(Y1~. , data=data_train,type="C-classification", cost = 5)
summary(model_svm5)
svm5_preds <- predict(model_svm5, data_test1) 
accuracy_svm5 = sum(svm5_preds == data_test$Y1)/length(data_test$Y1)


cat(paste("Accuracy:\t", format(accuracy_svm0.05 ), "\n",sep=" "))

cat(paste("Accuracy:\t", format(accuracy_svm5 ), "\n",sep=" "))

cat(paste("Accuracy:\t", format(accuracy_svm5 ), "\n",sep=" "))

###########################################################################


data_train$Y1 <- as.character(data_train$Y1)
data_train$Y1 <- as.factor(data_train$Y1)

model_rf100 <-  randomForest(formula = Y1 ~ ., data = data_train, ntree = 100) 
#summary(model_rf100)
rf100_preds <- predict(model_rf100, data_test1) 

accuracy_rf100 = sum(rf100_preds == data_test$Y1)/length(data_test$Y1)
cat(paste("Accuracy:\t", format(accuracy_rf100), "\n",sep=" "))



model_rf1000 <-  randomForest(formula = Y1 ~ ., data = data_train, ntree = 1000) 
#summary(model_rf1000)
rf1000_preds <- predict(model_rf1000, data_test1) 

accuracy_rf1000 = sum(rf1000_preds == data_test$Y1)/length(data_test$Y1)
cat(paste("Accuracy:\t", format(accuracy_rf1000), "\n",sep=" "))

