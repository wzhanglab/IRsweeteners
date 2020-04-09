library(tidyverse)
library(caret)
theme_set(theme_classic())
# Load the data
data("iris")
# Split the data into training (80%) and test set (20%)
set.seed(123)
training.samples <- iris$Species %>% 
  createDataPartition(p = 0.8, list = FALSE)


train.data <- iris[training.samples, ]
test.data <- iris[-training.samples, ]

# Estimate preprocessing parameters
preproc.param <- train.data %>% 
  preProcess(method = c("center", "scale"))
# Transform the data using the estimated parameters
train.transformed <- preproc.param %>% predict(train.data)
test.transformed <- preproc.param %>% predict(test.data)

#Compute LDA
library(MASS)
lda.model <- lda(Species~., data = train.transformed)
lda.model

# Make predictions
lda.predictions <- lda.model %>% predict(test.transformed)

#Note that, you can create the LDA plot using ggplot2 as follow:
lda.data <- cbind(train.transformed, predict(lda.model)$x)
ggplot(lda.data, aes(LD1, LD2)) +
  geom_point(aes(color = Species))
# Model accuracy
mean(predictions$class==test.transformed$Species)

