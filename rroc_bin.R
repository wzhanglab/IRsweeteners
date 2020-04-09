require(mlbench)
data(BreastCancer)
# some algorithms don't like missing values, so remove rows with missing values
BreastCancer <- na.omit(BreastCancer) 
# remove the unique identifier, which is useless and would confuse the machine learning algorithms
BreastCancer$Id <- NULL 
# partition the data set for 80% training and 20% evaluation (adapted from ?randomForest)
set.seed(2)
ind <- sample(2, nrow(BreastCancer), replace = TRUE, prob=c(0.8, 0.2))

# create model using svm (support vector machine)
require(e1071)
# svm requires tuning
x.svm.tune <- tune(svm, Class~., data = BreastCancer[ind == 1,],
                   ranges = list(gamma = 2^(-8:1), cost = 2^(0:4)),
                   tunecontrol = tune.control(sampling = "fix"))
# display the tuning results (in text format)
x.svm.tune
# If the tuning results are on the margin of the parameters (e.g., gamma = 2^-8), 
# then widen the parameters.
# I manually copied the cost and gamma from console messages above to parameters below.
x.svm <- svm(Class~., data = BreastCancer[ind == 1,], cost=4, gamma=0.0625, probability = TRUE)
x.svm.prob <- predict(x.svm, type="prob", newdata=BreastCancer[ind == 2,], probability = TRUE)

# svm
x.svm.prob.rocr <- prediction(attr(x.svm.prob, "probabilities")[,2], BreastCancer[ind == 2,'Class'])
x.svm.perf <- performance(x.svm.prob.rocr, "tpr","fpr")
plot(x.svm.perf)

##############
library(ROCR) 
data(ROCR.simple) 
pred <- prediction( ROCR.simple$predictions, ROCR.simple$labels ) 
perf <- performance( pred, "tpr", "fpr" ) 


library(ROCR) 
data(ROCR.hiv) 
manypred = prediction(ROCR.hiv$hiv.svm$predictions, ROCR.hiv$hiv.svm$labels) 
many.roc.perf = performance(manypred, measure ="tpr", x.measure = "fpr") 
plot(many.roc.perf, col=1:10) 
abline(a=0, b= 1) 
