
table(pred = predicted, real = expected)
table(pred = svm.pred, real = testset$species)

library(lattice)
library(ggplot2)



data(iris)

ir<-iris

set.seed(124)

count.test<-round(runif(50,1,150))

test<-ir[count.test,]

library(e1071)

sv<-svm(Species~.,data=ir,cross=5,type='C-classification',kernel='sigmoid')

summary(sv)  #?d?ݤ????V?q??sv?????^?H???A????????5?????e???????????̲v???92%



pre<-predict(sv,test)#???????????????@???????Cpre?O?@??????????V?q?C

dim(test[test$Species!=pre,])[1]/dim(test)[1]#????????????v



library(e1071)
library(rpart)
data(Glass, package="mlbench")
## split data into a train and test set
index <- 1:nrow(Glass)
testindex <- sample(index, trunc(length(index)/3))
testset <- Glass[testindex,]
trainset <- Glass[-testindex,]

svm.model <- svm(Type ~ ., data = trainset, cost = 100, gamma = 1)
svm.pred <- predict(svm.model, testset[,-10])

## compute svm confusion matrix
table(pred = svm.pred, true = testset[,10])

##Non-linear -Regression

library(e1071)
library(rpart)
data(Ozone, package="mlbench")
> ## split data into a train and test set
index <- 1:nrow(Ozone)
testindex <- sample(index, trunc(length(index)/3))
testset <- na.omit(Ozone[testindex,-3])
trainset <- na.omit(Ozone[-testindex,-3])

svm.model <- svm(V4 ~ ., data = trainset, cost = 1000, gamma = 0.0001)

svm.pred <- predict(svm.model, testset[,-3])

crossprod(svm.pred - testset[,3]) / length(testindex)



library(lattice)
install.packages("e1071")
library(e1071)
xyplot(Petal.Length ~ Petal.Width, data = iris, groups = Species,auto.key=list(corner=c(1,0)))
data("iris")
attach(iris)
subdata <- iris[iris$Species != 'virginica',]
subdata$Species <- factor(subdata$Species)
iris$Species <- factor(iris$Species)
model1 <- svm(Species ~ Petal.Length + Petal.Width, data = subdata)
plot(model1, subdata, Petal.Length ~ Petal.Width)
summary(model1)

