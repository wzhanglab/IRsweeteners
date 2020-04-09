data(iris)
library(randomForest)
library(pROC)
set.seed(1000)
# 3-class in response variable
rf = randomForest(Species~., data = iris, ntree = 100)
# predict(.., type = 'prob') returns a probability matrix
predictions <- as.numeric(predict(rf, iris, type = 'response'))
roc.multi <- multiclass.roc(iris$Species, predictions)
auc(roc.multi)
rs <- roc.multi[['rocs']]
plot.roc(rs[[1]])
sapply(2:length(rs),function(i) lines.roc(rs[[i]],col=i))

library(pROC)

data(aSAH)
roc1 <- plot.roc(aSAH$outcome, aSAH$s100, main="Statistical comparison", percent=TRUE, col="1")
roc2 <- lines.roc(aSAH$outcome, aSAH$ndka, percent=TRUE, col="2")
testobj<- roc.test(roc1,roc2)
text(50, 50, labels=paste("p-value =", format.pval(testobj$p.value)), adj=c(0, .5))
legend("bottomright", legend=c("S100B", "NDKA"), col=c("1", "2"), lwd=2)


