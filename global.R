#  Loading required library
#  , quietly = TRUE
# install.packages("caret")
# install.packages("ROSE")
# install.packages("RANN")

library(readr, quietly = TRUE)
library(shiny, quietly = TRUE)
library(dashboard, quietly = TRUE)

library(data.table, quietly = TRUE)
library(summarytools, quietly = TRUE)
library(dplyr, quietly = TRUE)
library(googleVis, quietly = TRUE)
library(ggplot2, quietly = TRUE)
library(DMwR, quietly = TRUE)
library(rpart, quietly = TRUE)
library(RANN, quietly = TRUE)
library(randomForest, quietly = TRUE)
library(naniar, quietly = TRUE)
library(recipes, quietly = TRUE)
library(caret, quietly = TRUE)
library(mice, quietly = TRUE)
library(ROSE, quietly = TRUE)
##############------------ TEST
library(visdat, quietly = TRUE)
library(carData, quietly = TRUE)


################## *******             ASSISTING FUNCTIONS *******              ##################

############        Find Columns Data Types of Specified Dataset
## Input:   the dataset to be check
## Output:  a list of:
#### numeric and factor column names
#### Frequency tables of all FACTOR variables
## Use of Output: obtain the returned result
## TEST SETUP
test_classifyColTypes <- function() {
  dtToClassify <<- canterCleansed
  factorList <<- canterCleansedColsType$factorList
}
## FUNCTION BODY
classifyColTypes <- function(dtToClassify) {
  # test_classifyColTypes()
  dtNames <- names(dtToClassify)
  numericCols <- dtNames[ sapply(dtNames, function(var) is.numeric(dtToClassify[[var]])) ]
  factorCols <- dtNames[ sapply(dtNames, function(var) is.factor(dtToClassify[[var]])) ]
  freqTblList <- list()
  
  ##  CREATE FREQUENCY TABLE FOR FUTURE ANALYSIS
  for(var in factorCols){
    currentFreqDF <- as.data.frame( table(dtToClassify[, var]) ) #, dnn = c("tes", "Freq")
    currentFreqDF$Freq.style <- c("orange") #rainbow(nrow(currentFreqDF))
    ## Fix names
    names(currentFreqDF)[names(currentFreqDF) == "Var1"] <- var
    freqTblList[[var]] <- currentFreqDF
  }
  resultList <- list(numericList = numericCols, factorList = factorCols, freqTblList = freqTblList)
  return(resultList)
}

########      PLOT GOOGLE VIS COLUMN CHART (BARCHART)
## TEST SETUP
test_plotGVisColChart <- function() {
  dtToClassify <<- canterCleansed
  colInfoList <<- canterCleansedColsType
  factorList <<- colInfoList$factorCols
  i <<- "Owner.size"
}
## FUNCTION BODY
plotGVisColChart <- function(dataToPlot, colInfoList) {
  # test_plotGVisColChart()
  gVisColChartList <- list()
  freqTblList <- colInfoList$freqTblList
  
  for (i in names(freqTblList)) {
    # print(i)
    # print(freqTblList[i])
    # print(str(freqTblList[i]))
    dfToPlot <- data.frame(freqTblList[i])
    names(dfToPlot) <- c(i, "Freq", "Freq.style")
    
    currentVisColPlot <- gvisColumnChart(dfToPlot, xvar = i, yvar = c("Freq", "Freq.style")
                                         , options = list(
                                           title = paste("Barchart of", i, sep = " ")
                                           , legend = "none"
                                         )
    )
    # plot(currentVisColPlot)
    gVisColChartList[[i]] <- currentVisColPlot
  }
  visPlotList <- Reduce(gvisMerge, gVisColChartList) #, horizontal=TRUE
  # plot(visPlotList)
  return(visPlotList)
}

################## *******             IMPUTATION *******              ##################

############        Imputation Train and Test Split
## Input:   the dataset to be split
## Output:  train dataset and test dataset
## Use of Output: train and test models
## TEST SETUP
test_splitTrainTestImpute <- function() {
  trainRatio <- 0.75
  dtToImpute <- canterCleansed
  colInfo <- canterCleansedColsType
  varToImpute <- "Pruning"
}
## FUNCTION BODY
splitTrainTestImpute <- function(dtToImpute, trainRatio = 0.75, varToImpute) {
  # test_splitTrainTestImpute()
  
  ##    Imputation
  ##    If any NA exist in dataset to be used for modelling, this means imputation is required
  if (anyNA(dtToImpute)) {
    # print("NA Exists!")
    nonNADT <<- na.omit(dtToImpute)
    allNADT <<- dtToImpute[ rowSums(is.na(dtToImpute))!=0, ]
    NAToSet <<- nrow(nonNADT) * (1-trainRatio)
    set.seed(50)
    NAIndex <<- sample(1:nrow(nonNADT), NAToSet)
    
    # 45 rows
    NAExpectedDT_t <- nonNADT[ NAIndex, ]
    NAExpectedDT <<- NAExpectedDT_t
    
    NATestDT_t <- NAExpectedDT
    NATestDT_t[, varToImpute] <- NA
    # 45 rows
    NATestDT_Small <<- NATestDT_t
    
    # 180 rows, with some NAs
    NATestDT_t <- nonNADT
    NATestDT_t[NAIndex, varToImpute] <- NA
    NATestDT <<- NATestDT_t
    
    # View(NATestDT)
    # dim(NATestDT_Small)
    # dim(NATrainDT)
    # dim(NATestDT)
    # dim(nonNADT)
  }
}


############        Impute Missing Data with kNN
## Input:   the dataset to be check
## Output:  a list of:
#### numeric and factor column names
#### Frequency tables of all FACTOR variables
## Use of Output: obtain the returned result
## TEST SETUP
test_imputeWithKNN <- function() {
  sourceDTToImpute <- canterCleansed
  trainRatio <- 0.75
  varToImpute <- "Pruning"
  splitTrainTestImpute(sourceDTToImpute, trainRatio, varToImpute)
}
## FUNCTION BODY
imputeWithKNN <- function() {
  # test_imputeWithKNN()
  
  ##  Impute by kNN
  knnImputeResult <- knnImputation(NATestDT)
  # anyNA(knnImputeResult)
  
  expectedResult <- NAExpectedDT$Pruning
  knnPredResult <- knnImputeResult[ NAIndex, "Pruning"]
  
  matchedCount <- length(which(expectedResult == knnPredResult))
  knnImputeAccuracy <<- round(matchedCount / NAToSet, 2)
  imputationAccuracyList[["kNN"]] <<- list(resultTable = data.table(expectedResult, knnPredResult)
                                             , accuracy = knnImputeAccuracy)
}


############        Impute Missing Data with rPart
## Input:   the dataset to be check
## Output:  a list of:
#### numeric and factor column names
#### Frequency tables of all FACTOR variables
## Use of Output: obtain the returned result
## TEST SETUP
test_imputeWithRPart <- function() {
  sourceDTToImpute <- canterCleansed
  trainRatio <- 0.75
  varToImpute <- "Pruning"
  splitTrainTestImpute(sourceDTToImpute, trainRatio, varToImpute)
}
## FUNCTION BODY
imputeWithRPart <- function() {
  # test_imputeWithRPart()
  
  ##  Build model by dataset without NAs
  rPartImputeForm <- rpart(Pruning ~ ., data = nonNADT, method = "class")
  
  rPartPredResult <- predict(rPartImputeForm, NATestDT_Small )
  expectedResult <- NAExpectedDT$Pruning
  rPartPredCompare <- colnames(rPartPredResult)[apply(rPartPredResult, 1, which.max)]
  
  matchedCount <- length(which(expectedResult == rPartPredCompare))
  rPartImputeAccuracy <<- round(matchedCount / NAToSet, 2)
  imputationAccuracyList[["rPart"]] <<- list( resultTable = data.table(expectedResult, rPartPredCompare)
                                             , accuracy = rPartImputeAccuracy)
  
}

############        Impute Missing Data with Multivariate Imputation by Chained Equations
## Input:   the dataset to be check
## Output:  a list of:
#### numeric and factor column names
#### Frequency tables of all FACTOR variables
## Use of Output: obtain the returned result
## TEST SETUP
test_imputeWithMICE <- function() {
  sourceDTToImpute <- canterCleansed
  trainRatio <- 0.75
  varToImpute <- "Pruning"
  splitTrainTestImpute(sourceDTToImpute, trainRatio, varToImpute)
}
## FUNCTION BODY
imputeWithMICE <- function() {
  # test_imputeWithMICE()
  
  ##  Impute with Random Forest
  # dim(NATestDT)
  miceImputeForm <- mice(NATestDT, method = "rf")
  miceImputePrediction <- mice::complete(miceImputeForm)
  # anyNA(miceImputeResult)
  
  ##  Now Test!
  miceImputeResult <- miceImputePrediction[NAIndex, "Pruning"]
  expectedResult <- NAExpectedDT$Pruning
  
  matchedCount <- length(which(expectedResult == miceImputeResult))
  miceImputeAccuracy <<- round(matchedCount / NAToSet, 2)
  imputationAccuracyList[["MICE"]] <<- list(resultTable = data.table(expectedResult, miceImputeResult)
                                             , accuracy = miceImputeAccuracy)
}

############        Impute Missing Data with Recipe
## Input:   the dataset to be check
## Output:  a list of:
#### numeric and factor column names
#### Frequency tables of all FACTOR variables
## Use of Output: obtain the returned result
## TEST SETUP
test_imputeWithRecipe <- function() {
  sourceDTToImpute <- canterCleansed
  trainRatio <- 0.75
  varToImpute <- "Pruning"
  splitTrainTestImpute(sourceDTToImpute, trainRatio, varToImpute)
}
## FUNCTION BODY
imputeWithRecipe <- function() {
  # test_imputeWithRecipe()
  recipeImputeForm <- recipe(Pruning ~ . , data = NATestDT) %>% 
    step_naomit(all_outcomes(), skip = TRUE) %>%
    step_knnimpute(all_predictors())  
  
  recipeImputeTrained <- caret::train(recipeImputeForm, data = NATestDT, method = "rf")
  recipePredResult <- predict(recipeImputeTrained, newdata = NATestDT_Small)
  
  expectedResult <- NAExpectedDT$Pruning
  matchedCount <- length(which(expectedResult == recipePredResult))
  recipeImputeAccuracy <<- round(matchedCount / NAToSet, 2)
  imputationAccuracyList[["Recipe"]] <<- list(resultTable = data.table(expectedResult, recipePredResult)
                                             , accuracy = recipeImputeAccuracy)
}

############        Impute MASTER CALL
## Input:   the dataset to be check
## Output:  a list of:
#### numeric and factor column names
#### Frequency tables of all FACTOR variables
## Use of Output: obtain the returned result
## TEST SETUP
test_imputeMaster <- function() {
  dtToImpute <- canterCleansed
  imputeTrainRatio <- 0.75
  varToImpute <- "Pruning"
  # i <- imputationAccuracyList[[1]]
  # i <- 1
  # print(resultDT)
}
## FUNCTION BODY
imputeMaster <- function(dtToImpute, imputeTrainRatio, varToImpute) {
  # test_imputeMaster()

  ##  Call all required functions to try different imputations
  splitTrainTestImpute(dtToImpute, imputeTrainRatio, varToImpute)  
  imputeWithKNN()
  imputeWithRPart()
  imputeWithMICE()
  imputeWithRecipe()
  
  ##  Obtain imputations results
  imputationAccuracyDT <<- data.table(method = names(imputationAccuracyList)
                                    , accuracy = NA_real_  )
  
  masterImputationResultDT <<- data.table(method = character(), expectedResult = character()
                                    , predictResult = character())
  
  ##  Unwrap to get result table and accuracy
  for ( i in seq(1:length(names(imputationAccuracyList))) ) {
    currentSet <- imputationAccuracyList[i]
    methodName <- as.character(names(currentSet))
    
    unlistDT <- as.data.table(unlist(currentSet, recursive = FALSE))
    v <- as.numeric(unique(unlistDT[, 3]))
    ##  Take out accuracy column
    resultDT <- unlistDT[, -3]
    names(resultDT) <- c("expectedResult", "predictResult")
    resultDT[, method := rep(methodName, nrow(resultDT)) ]
    ## Form result table to add into master result table
    resultDT <- setcolorder(resultDT, c("method","expectedResult","predictResult"))

    masterImputationResultDT <<- base::rbind(masterImputationResultDT, resultDT)
    imputationAccuracyDT[ method == methodName, accuracy := v ]
  }
  # masterImputationResultDT
}
################## *******             MODELLING *******              ##################

############        Model - Train and Test Split
## Input:   the dataset to be split
## Output:  train dataset and test dataset
## Use of Output: train and test models
## TEST SETUP
test_splitTrainTestModel <- function() {
  trainRatio <<- 0.75
  dtToModel <<- canterCleansed
  colInfo <<- canterCleansedColsType
}
## FUNCTION BODY
splitTrainTestModel <- function(dtToModel, trainRatio = 0.75) {
  # test_splitTrainTestModel()
  
  modelRowsLength <<- nrow(dtToModel)
  maxNumToMatch <<- max(table(dtToModel[ "Owner.size" ]))
  ## Calculate sample size
  trainLength <<- floor(nrow(dtToModel) * trainRatio)

  ## split data by trainRatio
  trainIndexSet <<- sample(seq_len(nrow(dtToModel)), size = trainLength)
  trainDT <<- dtToModel[trainIndexSet, ]
  testDT <<- dtToModel[-trainIndexSet, ]
}

############        Model with Purely Under-sampling
## Input:   the dataset to be check
## Output:  a list of:
#### numeric and factor column names
#### Frequency tables of all FACTOR variables
## Use of Output: obtain the returned result
## TEST SETUP
test_modelWithUnderSample <- function() {
  trainRatio <<- 0.75
  dtToModel <<- canterCleansed
  modelSplitTrainTest(dtToModel, trainRatio)
}
## FUNCTION BODY
modelWithUnderSample <- function() {
  test_modelWithUnderSample()

  underSampledTrainDT <- ovun.sample(Owner.size ~ ., data = trainDT, method = "under", N = maxNumToMatch)$data
  table(underSampledTrainDT$Owner.size)
}
  


############        Model with Purely Over-sampling
## Input:   the dataset to be check
## Output:  a list of:
#### numeric and factor column names
#### Frequency tables of all FACTOR variables
## Use of Output: obtain the returned result
## TEST SETUP
test_modelWithOverSample <- function() {
  trainRatio <<- 0.75
  dtToModel <<- canterCleansed
}
## FUNCTION BODY
modelWithOverSample <- function(dtToModel, trainRatio = 0.75) {
  test_modelWithOverSample()

  data_balanced_over <- ovun.sample(Owner.size ~ ., data = trainDT
                                    , method = "over", N = modelRowsLength)$data
  table(data_balanced_over$Owner.size)

  roc.curve(hacide.test$cls, pred.treeimb[,2], plotit = F)
}
  
  
  
############        Model with Recipe (Over and Under Samplings)
## Input:   the dataset to be check
## Output:  a list of:
#### numeric and factor column names
#### Frequency tables of all FACTOR variables
## Use of Output: obtain the returned result
## TEST SETUP
test_modelWithRecipe <- function() {
  trainRatio <<- 0.75
  dtToModel <<- canterCleansed
}
## FUNCTION BODY
modelWithRecipe <- function(dtToModel, trainRatio = 0.75) {
  test_modelWithRecipe()

  # canterCleansedColsType$freqTblList$Owner.size
  modelRecipe <- recipe(Owner.size ~ ., data = trainDT) %>%
    step_downsample(all_outcomes(), ratio = 10, skip = FALSE) %>%
    step_upsample(all_outcomes(), ratio = 1, skip = FALSE) %>%
    prep(data = trainDT)
  
  trainData_Recipe <- bake(modelRecipe, trainDT)
  balDataRecipe <<- classifyColTypes(trainData_Recipe)
  
  modelRecipe <- caret::train(OwnerSize ~ ., data = trainDT, method = "rpart2", metric = "Accuracy"
                              , trControl = trainControl(method = "boot", number = 20))
?caret::train
View(trainDT)
  names(trainDT)[4] <- "OwnerSize"
  levels(trainDT$OwnerSize)
  set.seed(99)
  rec <- recipe(cls ~ ., data = hacide.train) %>%
    step_downsample(all_outcomes(), ratio = 10, skip = TRUE) %>%
    step_upsample(all_outcomes(), ratio = 1, skip = TRUE)
  caret::train(rec, data = hacide.train, method = "rpart2", metric = "Accuracy",
               trControl = trainControl(method = "boot", number = 20))

  roc.curve(hacide.test$cls, pred.treeimb[,2], plotit = F)
}

############        Model with Weighted Data
## Input:   the dataset to be check
## Output:  a list of:
#### numeric and factor column names
#### Frequency tables of all FACTOR variables
## Use of Output: obtain the returned result
## TEST SETUP
test_modelWithWeights <- function() {
  trainRatio <<- 0.75
  dtToModel <<- canterCleansed
}
## FUNCTION BODY
modelWithWeights <- function(dtToModel, trainRatio = 0.75) {
  test_modelWithWeights()
  
  tab <- table(hacide.train$cls)
  w <- rep(1, times = nrow(hacide.train))
  for (level in names(tab)) {
    w[hacide.train$cls == level] <- sum(tab) / tab[[level]]
  }
  w
  
  caret::train(cls ~ ., data = hacide.train, method = "rpart2", metric = "Accuracy",
             trControl = trainControl(method = "boot", number = 20),
             weights = w)
  roc.curve(hacide.test$cls, pred.treeimb[,2], plotit = F)
}

############        Model with Random Over Sampling Examples
## Input:   the dataset to be check
## Output:  a list of:
#### numeric and factor column names
#### Frequency tables of all FACTOR variables
## Use of Output: obtain the returned result
## TEST SETUP
test_modelWithROSE <- function() {
  
}
## FUNCTION BODY
modelWithROSE <- function(dtToModel, trainRatio = 0.75) {
  test_modelWithWeights()
  
  roseTree <- rpart(Owner.size ~ ., data = trainDT)
  predROSE <- predict(roseTree, newdata = testDT)
  
  # ROSE::accuracy.meas(hacide.test$cls, pred.treeimb[,2])
  # roc.curve(hacide.test$cls, pred.treeimb[,2], plotit = F)
  
}

############        Model with Synthetic Minority Over-sampling Technique
## Input:   the dataset to be check
## Output:  a list of:
#### numeric and factor column names
#### Frequency tables of all FACTOR variables
## Use of Output: obtain the returned result
## TEST SETUP
test_modelWithSMOTE <- function() {
  trainRatio <<- 0.75
  dtToModel <<- canterCleansed
}
## FUNCTION BODY
modelWithSMOTE <- function(dtToModel, trainRatio = 0.75) {
  test_modelWithWeights()
  
  ctrl <- trainControl(method = "repeatedcv", 
                     number = 10, 
                     repeats = 10, 
                     verboseIter = FALSE,
                     sampling = "smote")
  
  set.seed(42)
  model_rf_smote <- caret::train(classes ~ .,
                                data = train_data,
                                method = "rf",
                                preProcess = c("scale", "center"),
                                trControl = ctrl)
  final_smote <- data.frame(actual = test_data$classes,
                           predict(model_rf_smote, newdata = test_data, type = "prob"))
  final_smote$predict <- ifelse(final_smote$benign > 0.5, "benign", "malignant")
  cm_smote <- confusionMatrix(final_smote$predict, test_data$classes)
}

############        Model with Synthetic Minority Over-sampling Technique
## Input:   the dataset to be check
## Output:  a list of:
#### numeric and factor column names
#### Frequency tables of all FACTOR variables
## Use of Output: obtain the returned result
## TEST SETUP
test_predictModels <- function() {
  trainRatio <<- 0.75
  dtToModel <<- canterCleansed
}
## FUNCTION BODY
predictModels <- function(dtToModel, trainRatio = 0.75) {
  test_predictModels()
  
  models <- list(original = model_rf,
                         under = model_rf_under,
                         over = model_rf_over,
                         smote = model_rf_smote,
                         rose = model_rf_rose)
  
  resampling <- resamples(models)
  bwplot(resampling)

  library(dplyr)
  comparison <- data.frame(model = names(models),
                           Sensitivity = rep(NA, length(models)),
                           Specificity = rep(NA, length(models)),
                           Precision = rep(NA, length(models)),
                           Recall = rep(NA, length(models)),
                           F1 = rep(NA, length(models)))

  for (name in names(models)) {
    model <- get(paste0("cm_", name))
    
    comparison[comparison$model == name, ] <- filter(comparison, model == name) %>%
      mutate(Sensitivity = model$byClass["Sensitivity"],
             Specificity = model$byClass["Specificity"],
             Precision = model$byClass["Precision"],
             Recall = model$byClass["Recall"],
             F1 = model$byClass["F1"])
  }
  
  library(tidyr)
  comparison %>%
    gather(x, y, Sensitivity:F1) %>%
    ggplot(aes(x = x, y = y, color = model)) +
      geom_jitter(width = 0.2, alpha = 0.5, size = 3)
}



############        Accuracy Test
## Input:   the dataset to be check
## Output:  a list of:
#### numeric and factor column names
#### Frequency tables of all FACTOR variables
## Use of Output: obtain the returned result
## TEST SETUP
test_accuracyTestMode <- function() {
  trainRatio <<- 0.75
  dtToModel <<- canterCleansed
}
## FUNCTION BODY
accuracyTestModel <- function() {
  test_accuracyTestMode()
  ROSE::accuracy.meas(testDT$cls, predROSE[,2])
                      
  roc.curve(testDT$Owner.size, predROSE[,2], plotit = F)
}

  
  
################## *******             BASIC DATA MANIPULATION *******              ##################

# Read in the data set
allWoodData <- read.delim("2015-yield-tables-Canterbury.tsv", header = TRUE, sep = "\t")
## Fix column names
names(allWoodData) <- gsub("\\.\\.", ".", names(allWoodData))
##  Get Canterbury Data
canterSource <- subset(allWoodData, Wood.Supply.Region == "Canterbury") 
canterSource <- droplevels.data.frame(canterSource)
canterSourceColsType <<- classifyColTypes(canterSource)

# DROP COLUMNS THAT HAVE ONLY ONE LEVEL
canterCleansed <<- canterSource[, sapply(canterSource, function(col) length(unique(col))) > 1]
canterCleansedColsType <<- classifyColTypes(canterCleansed)

imputationAccuracyList <<- list()

##  INITIAL IMPUTATION CALL
imputeMaster(canterCleansed, 0.75, "Pruning")

####      CALL MAIN FUNCTIONS
# modelSplitTrainTest(canterCleansed, 0.75)


dev <- function() {
  
######    Current scope - Imputation
pp <- caret::preProcess(x = canterCleansed, method = "knnImpute")
t <- predict(pp, canterCleansed)
View(t)
dim(t)
View(canterCleansed)

colSums(is.na(canterCleansed))
??colsum
?predict
install.packages("mice")
library(mice)
imputed_Data <- mice::mice(canterCleansed, m = 5, maxit = 50, method = 'pmm', print = FALSE)
summary(imputed_Data$m)
View(imputed_Data)
names(canterCleansed)

modset <- with(data = imputed_Data, exp = lm(Pruning ~ .)) 
summary(modset)


  View(canterCleansed)
  names(canterCleansed)
 
  summary(canterCleansed)
  summarytools::dfSummary(canterCleansed, style = "multiline")
  ?gvisColumnChart
  
  data(MplsStops)
    d <- MplsStops
    limit <- 1000
    if (nrow(d) > limit) {
      d <- d[sample(1:nrow(d), limit),]
    }
    d <- d[order(d$idNum, decreasing = FALSE),]
    View(d)
    visdat::vis_dat(d)

  install.packages("Amelia")    
  library(Amelia)
  amelia_sets <- Amelia::amelia(iris2, m = 5, noms = "Species")
  summary(amelia_sets)
  nrow(amelia_sets)
  dim(iris2)
  amelia_sets$imputations[[2]]
  
  t1 <- Amelia::amelia(trainDT, m = 5, noms = "Pruning")
  
  tt <- mice(trainDT)
  tt$imp$Pruning
  View(tt)
  t2 <- complete(tt)
  View(t2)
  which(is.na(t2$Pruning))
  ?which
  imp <- mice(nhanes)
imp

library(caret, quietly = TRUE)
pp <- caret::preProcess(x = trainDT, method = "knnImpute")
pT <- predict(pp, trainDT)
View(pT)
is.na(pT$Pruning)
# }
# 
test5Result <- train(Sepal.Length ~ . , data = iris_miss_5, method = "lm",
           preProc = "knnImpute", na.action = na.pass)

# NOT RUN {
install.packages("mlr")
library(mlr)
df = data.frame(x = c(1, 1, NA), y = factor(c("a", "a", "b")), z = 1:3)
imputed = impute(df, target = character(0), cols = list(x = 99, y = imputeMode()))
print(imputed$data)
reimpute(data.frame(x = NA_real_), imputed$desc)


test5Result <- caret::train(Pruning ~ . , data = trainDT, method = "RFlda", 
           preProc = "bagImpute", na.action = na.pass)
head(test5Result)#ends in error

library(rpart)
install.packages("mlbench")
data ("BostonHousing", package="mlbench")
class_mod <- rpart(rad ~ . - medv, data=BostonHousing[!is.na(BostonHousing$rad), ], method="class", na.action=na.omit)  # since rad is a factor
anova_mod <- rpart(ptratio ~ . - medv, data=BostonHousing[!is.na(BostonHousing$ptratio), ], method="anova", na.action=na.omit)  # since ptratio is numeric.
rad_pred <- predict(class_mod, BostonHousing[is.na(BostonHousing$rad), ])
ptratio_pred <- predict(anova_mod, BostonHousing[is.na(BostonHousing$ptratio), ])

# }
# 
}




