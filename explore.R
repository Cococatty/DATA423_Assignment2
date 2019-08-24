iris_miss_5 <- iris
iris_miss_5[1,5] <- NA
set.seed(1)
test5Result <- train(Sepal.Length ~ . , data = iris_miss_5, method = "lm", 
                     preProc = "knnImpute", na.action = na.pass)
head(test5Result)#ends in error


train(Pruning ~ . , data=canterCleansed, method = "knn", preProc = "knnImpute", na.action = na.pass)
test5Result <- train(Sepal.Length ~ . , data = iris_miss_5, method = "lm", 
                     preProc = "knnImpute", na.action = na.pass)
head(test5Result)#ends in error


install.packages("missForest")
install.packages("e1071")

library(missForest)
data(iris)
iris2 <- missForest::prodNA(iris, noNA = 0.1)    # cause each column to have about 10% of values missing

library(caret)
library(recipes, quietly = TRUE)
r <- recipe(Species ~., data = iris2) %>%
  step_naomit(all_outcomes(), skip = TRUE) %>%
  step_knnimpute(all_predictors())
mod <- caret::train(r, data = iris2, method = "lda")
predict(mod, newdata = iris)
