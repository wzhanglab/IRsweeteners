setwd("D:/Dropbox/OneDrive/publish/libin_zhang/R/")

choose(5,1) + choose(5,2) + choose(5,3) + choose(5,4) + choose(5,5)

csf <- csf[,-1]
##randomly sample, testing data,30%,training data 70%
require(ggplot2)


#Stratified Sampling
## split data into a train and test set
require(sampling)

sub_train=strata(csf,stratanames=("Species"),size=rep(18,31),method="srswor")
head(sub_train)
trainset=csf[sub_train$ID_unit,]
testset=csf[-sub_train$ID_unit,]


#learning from training
library(e1071)
svm_tune <- tune(svm, train.x=trainset,  
                 kernel="radial", ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)))

obj <- tune(svm, Species~., data = trainset, 
            ranges = list(gamma = 2^(-1:1), cost = 10^(-1:4)),
            tunecontrol = tune.control(sampling = "fix")
)
obj <- tune.svm(Species~., data = trainset, 
                cost = 10^(-1:2), 
                kernel = "radial")
for (i in c(0.1:100)){
  
  
}
svm.model <- svm(Species ~ ., data = trainset,kernel = "radial",
                 gamma = 0.012, cost = 27.1,
                 probability = TRUE)
#, decision.values = TRUE
#gamma = 0.01, cost = 100, cross = 8,
# predict the test data
# method = "C-classfication",
#  
#
svm.pred <- predict(svm.model, trainset[,-131], decision.values=TRUE, probability = TRUE)
svm.probs <- attr(svm.pred,"probabilities") #"decision.values"
svm.class <- predict(svm.model,trainset[,-131], type =  "class")
svm.labels <- trainset$Species

## compute svm confusion matrix
library(lattice)
library(ggplot2)
library(caret)
svm.confusion <- confusionMatrix(svm.labels,svm.class)
table(svm.labels,svm.class)


######
svm.prediction<-prediction(svm.probs[,2],svm.labels)
svmmodel.performance<-performance(svm.prediction,"tpr","fpr")
svmmodel.auc<-performance(svm.prediction,"auc")@y.values[[1]]

plot(svmmodel.performance)

acf.x = unlist(svmmodel.performance@x.values)
acf.y = unlist(svmmodel.performance@y.values)

#compute roc roc curve of test data

require(ROCR)
library(e1071)
roc_svm <- raw_data[,-1]
acf <- subset(roc_svm, species == "ACF")
acf_n <- subset(roc_svm, species != "ACF")
acf$species <- factor(acf$species) 
acf_n$species <- factor("N")

acf_test <- acf[rsf(acf),]
acf_train <- acf[-rsf(acf),]

acf_n_test <- acf_n[rsf(acf_n),]
acf_n_train <- acf_n[-rsf(acf_n),]
acf_others_train <- rbind(acf_train,acf_n_train)
acf_others_test <- rbind(acf_test, acf_n_test)

trainset <- acf_others_train
testset <- acf_others_test

svm.model <- svm(species ~ ., data = trainset,
                 probability = TRUE, decision.values = TRUE)
                   #, decision.values = TRUE 

svm.pred <- predict(svm.model, testset[,-1057], decision.values=TRUE, probability = TRUE)
svm.probs <- attr(svm.pred,"probabilities")
svm.class <- predict(svm.model,testset, type =  "class")
svm.labels <- testset$species

svm.prediction<-prediction(svm.probs[2],svm.labels)
svmmodel.performance<-performance(svm.prediction,"tpr","fpr")
svmmodel.auc<-performance(svm.prediction,"auc")@y.values[[1]]

plot(svmmodel.performance)
plot(svmmodel.auc)
mylist <- svm.pred
capture.output(mylist, file = "svm.pred.txt")

expected <- factor(testset$species)
predicted <- factor(svm.pred)
results <- confusionMatrix(table(pred = predicted, real = expected))
# export the confusion matrix result
mylist <- results
capture.output(mylist, file = "My New File.txt")

#analyzing result


results <- confusionMatrix(data=predicted, reference=expected)
print(results)


table(expected)
table(predicted)

summary(svm.model)
summary(svm.pred)


## kNN model


