library(randomForest)

Randommodel <- randomForest(Species ~ ., data=trainset,importance = TRUE, proximity = FALSE, ntree = 100)

print(Randommodel)
importance(Randommodel,type=1)
importance(Randommodel,type=2)  #Gini指数
varImpPlot(Randommodel)         #可视化
rf.prediction <- predict(Randommodel, trainset[,-131],type="class")  #还有response回归类型

table(observed =testset$Species,predicted=rf.prediction) 
rf.confusion <- confusionMatrix(trainset$Species,rf.prediction)

#tune rf

for (i in c(1:120)) {
  Randommodel <- randomForest(Species ~ ., data=trainset,importance = TRUE, proximity = FALSE, ntree = i)
  rf.prediction <- predict(Randommodel, testset[,-131],type="class")
  rf.confusion <- confusionMatrix(testset$Species,rf.prediction)
  print(i)
  print(rf.confusion$overall[1])
  print("~~~~~~~~~~~")
}



