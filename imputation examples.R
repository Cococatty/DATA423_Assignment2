######-------------- kNN
library(DMwR)
data ("BostonHousing", package="mlbench")  # initialize the data  # load the data
original <- BostonHousing  # backup original data
# Introduce missing values
set.seed(100)
BostonHousing[sample(1:nrow(BostonHousing), 40), "rad"] <- NA
BostonHousing[sample(1:nrow(BostonHousing), 40), "ptratio"] <- NA
# 
# 
# knnOutput <- knnImputation(BostonHousing[, !names(BostonHousing) %in% "medv"])  # perform knn imputation.
# anyNA(knnOutput)
# 
# actuals <- original$ptratio[is.na(BostonHousing$ptratio)]
# predicteds <- knnOutput[is.na(BostonHousing$ptratio), "ptratio"]
# regr.eval(actuals, predicteds)


######-------------- rPart
library(rpart)
# since rad is a factor
class_mod <- rpart(rad ~ . - medv, data=BostonHousing[!is.na(BostonHousing$rad), ], method="class", na.action=na.omit)
# since ptratio is numeric.
anova_mod <- rpart(ptratio ~ . - medv, data=BostonHousing[!is.na(BostonHousing$ptratio), ], method="anova", na.action=na.omit)  
rad_pred <- predict(class_mod, BostonHousing[is.na(BostonHousing$rad), ])
ptratio_pred <- predict(anova_mod, BostonHousing[is.na(BostonHousing$ptratio), ])

actuals <- original$ptratio[is.na(BostonHousing$ptratio)]
predicteds <- ptratio_pred
regr.eval(actuals, predicteds)

actuals <- original$rad[is.na(BostonHousing$rad)]
predicteds <- as.numeric(colnames(rad_pred)[apply(rad_pred, 1, which.max)])
mean(actuals != predicteds)  # compute misclass error.


##########-----------------
library(caret)
library(recipes, quietly = TRUE)
library(missForest)
data(iris)
iris2 <- missForest::prodNA(iris, noNA = 0.1)    # cause each column to have about 10% of values missing

r <- recipe(Species ~., data = iris2) %>%
  step_naomit(all_outcomes(), skip = TRUE) %>%
  step_knnimpute(all_predictors())
mod <- caret::train(r, data = iris2, method = "lda")
predict(mod, newdata = iris
