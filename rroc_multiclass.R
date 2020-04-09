library(ROCR)
library(klaR)

#data(iris)
#lvls = levels(iris$Species)
sw <- raw_data[,-1]

ind <- sample(2, length(sw$species), replace = TRUE, prob=c(0.7, 0.3))
sw.train = sw[ind == 1, ]
sw.test = sw[ind == 2, ]

aucs = c()
plot(x=NA, y=NA, xlim=c(0,1), ylim=c(0,1),
     ylab='True Positive Rate',
     xlab='False Positive Rate',
     bty='n')

library(e1071)
levl <-c('A', 'ABC', 'ACF', 'AD', 'B', 'C', 'D', 'F')

for (species in levl) {
  print(species)
  ind <- sample(2, length(sw$species), replace = TRUE, prob=c(0.7, 0.3))
  sw.train = sw[ind == 1, ]
  sw.test = sw[ind == 2, ]
  mysel <- sw.train$species %in% species
  sw.train$species[!mysel] <- c("N")
  mysel <- sw.test$species %in% species
  sw.test$species[!mysel] <- c("N")
  
  sw.train$species <- factor(sw.train$species)
  sw.test$species <- factor(sw.test$species)
  
  svm.model <- svm(species ~ ., data = sw.train,
                   kernel = "radial", 
                   probability = TRUE)
  # method = "C-classfication", 
  #decision.values = TRUE,
  #gamma = 0.01, cost = 100, cross = 8, 
  
  # predict the test data
  svm.pred <- predict(svm.model, sw.test[,-1057], decision.values=TRUE, 
                      probability = TRUE, type='raw')
  svm.probs <- attr(svm.pred,"probabilities") #"decision.values"
  svm.class <- predict(svm.model,sw.test[,-1057], type =  "class")
  svm.labels <- sw.test$species  
  
  svm.prediction<-prediction(svm.probs[,1],svm.labels)
  svmmodel.performance<-performance(svm.prediction,"tpr","fpr")
  svmmodel.auc<-performance(svm.prediction,"auc")@y.values[[1]]

  roc.x = unlist(svmmodel.performance@x.values)
  roc.y = unlist(svmmodel.performance@y.values)
  lines(roc.y ~ roc.x, col=which(species == levl), lwd=1,lty = 5)
  text(species)
  
  svmmodel.auc<-performance(svm.prediction,"auc")@y.values[[1]]
  aucs[which(species == levl)] = svmmodel.auc
}



mean(aucs)
