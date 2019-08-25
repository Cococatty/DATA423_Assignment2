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
library(Rborist, quietly = TRUE)
library(mice, quietly = TRUE)
library(ROSE, quietly = TRUE)
##############------------ TEST
library(visdat, quietly = TRUE)
library(carData, quietly = TRUE)
library(rpart)
library(rpart.plot)

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
  i <<- "Planting.coverage"
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
  knnImputeAccuracy <- round(matchedCount / NAToSet, 2)
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
  rPartImputeAccuracy <- round(matchedCount / NAToSet, 2)
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
  miceImputeAccuracy <- round(matchedCount / NAToSet, 2)
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
  recipeImputeAccuracy <- round(matchedCount / NAToSet, 2)
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
  modelTrainRatio <- 0.75
  dtToModel <- canterCleansedNoNA
  colInfo <- canterCleansedColsType
}
## FUNCTION BODY
splitTrainTestModel <- function(dtToModel, modelTrainRatio = 0.75) {
  # test_splitTrainTestModel()

  dtToModel <- canterCleansedNoNA
  modelRowsLength <<- nrow(dtToModel)
  maxNumToMatch <<- max(table(dtToModel[ "Planting.coverage" ]))
  ## Calculate sample size
  trainLength <<- floor(nrow(dtToModel) * modelTrainRatio)

  ## split data by modelTrainRatio
  set.seed(100)
  
  sourceLevels <- levels(dtToModel$Planting.coverage)
  trainIndexSet_a <- sample(seq_len(nrow(dtToModel)), size = trainLength)
  validTrainDT <- dtToModel[trainIndexSet_a, ]
  outcomeLevels <- levels(droplevels(validTrainDT$Planting.coverage))
  # i <- sourceLevels[1]
  
  while (length(outcomeLevels) != length(sourceLevels) ) {
    trainIndexSet_a <- sample(seq_len(nrow(dtToModel)), size = trainLength)
    validTrainDT <- dtToModel[trainIndexSet_a, ]
    outcomeLevels <- levels(droplevels(validTrainDT$outcomeLevels))
  }
  
  trainIndexSet <<- trainIndexSet_a
  modelTrainDT <<- dtToModel[trainIndexSet, ]
  modelTrainNAsDT_t <- modelTrainDT
  modelTestDT <<- dtToModel[-trainIndexSet, ]
  modelTestNAIndex <<- sample(seq_len(nrow(modelTestDT)), size = nrow(modelTestDT) * (1-modelTrainRatio))
  modelNALength <<- length(modelTestNAIndex)
  modelTestNAsDT_t <- modelTestDT[modelTestNAIndex, ]
  modelTestNAsExpectedDT <<- modelTestDT[modelTestNAIndex, ]
  modelTestNAsDT_t$Planting.coverage <- NA
  modelTestNAsDT <<- modelTestNAsDT_t
  # View(modelTestDT)
}

############        Model with Recipe (Over and Under Samplings)
## Input:   the dataset to be check
## Output:  a list of:
#### numeric and factor column names
#### Frequency tables of all FACTOR variables
## Use of Output: obtain the returned result
## TEST SETUP
test_modelWithRecipe <- function() {
  modelTrainRatio <- 0.75
  dtToModel <- canterCleansed
}
## FUNCTION BODY
modelWithRecipe <- function() {
  # test_modelWithRecipe()

  # canterCleansedColsType$freqTblList$outcomeLevels
  tempDT <- na.omit(modelTrainDT)
  recipeBalance <- recipe(Planting.coverage ~ ., data = tempDT) %>%
    step_downsample(all_outcomes(), ratio = 10, skip = FALSE) %>%
    step_upsample(all_outcomes(), ratio = 1, skip = FALSE) %>%
    prep(data = tempDT)
  
  balancedData <- bake(recipeBalance, tempDT)
  # table(balancedData$Planting.coverage)

  recipeFormula <- caret::train(Planting.coverage ~ ., data = balancedData, method = "rpart2", metric = "Accuracy",
             trControl = trainControl(method = "boot", number = 20))

  predictionResult <- predict(recipeFormula, modelTestNAsDT)

  ##  Accuracy feedback
  simpleAccuracyTestModel("Recipe", predictionResult)
  modelResultConfMatRecipe <<- confMatAccuracyTestModel(predictionResult)
}

############        Model with Weighted Data
## Input:   the dataset to be check
## Output:  a list of:
#### numeric and factor column names
#### Frequency tables of all FACTOR variables
## Use of Output: obtain the returned result
## TEST SETUP
test_modelWithWeights <- function() {
  modelTrainRatio <- 0.75
  dtToModel <- canterCleansed
}
## FUNCTION BODY
modelWithWeights <- function() {
  # test_modelWithWeights()
  tempDT <- modelTrainDT
  tab <- table(tempDT$Planting.coverage)
  weightToGet <- rep(1, times = nrow(tempDT))
  
  for (level in names(tab)) {
    weightToGet[tempDT$Planting.coverage == level] <- sum(tab) / tab[[level]]
  }
  
  weightFormular <- caret::train(Planting.coverage ~ ., data = tempDT, method = "rpart2", metric = "Accuracy"
                                 , trControl = trainControl(method = "boot", number = 20)
                                 , weights = weightToGet)
  
  predictionResult <- predict(weightFormular, modelTestNAsDT)
  
  ##  Accuracy feedback
  simpleAccuracyTestModel("Weighted", predictionResult)
  modelResultConfMatWeighted <<- confMatAccuracyTestModel(predictionResult)
}

############        Model with Random Over Sampling Examples
## Input:   the dataset to be check
## Output:  a list of:
#### numeric and factor column names
#### Frequency tables of all FACTOR variables
## Use of Output: obtain the returned result
## TEST SETUP
test_modelWithROSE <- function() {
  modelTrainRatio <- 0.75
  dtToModel <- canterCleansed
  splitTrainTestModel(dtToModel, modelTrainRatio)
  i <- 1
}
## FUNCTION BODY
modelWithROSE <- function() {
  # test_modelWithWeights()
  tempDT <- modelTrainDT
  roseFormula <- rpart(Planting.coverage ~ ., data = tempDT)
  predictionResult <- predict(roseFormula, newdata = modelTestNAsDT, type = "class")
  
  ##  Accuracy feedback
  simpleAccuracyTestModel("ROSE", predictionResult)
  modelResultConfMatROSE <<- confMatAccuracyTestModel(predictionResult)
  # modelAccuracyList is global
}


############        Model with Synthetic Minority Over-sampling Technique
## Input:   the dataset to be check
## Output:  a list of:
#### numeric and factor column names
#### Frequency tables of all FACTOR variables
## Use of Output: obtain the returned result
## TEST SETUP
test_modelWithSMOTE <- function() {
  modelTrainRatio <- 0.75
  dtToModel <- canterCleansed
  splitTrainTestModel(dtToModel, modelTrainRatio)
  i <- 1
}
## FUNCTION BODY
modelWithSMOTE <- function() {
  # test_modelWithSMOTE()
  # temp <- modelTrainDT
  smoteCtrl <- trainControl(method = "repeatedcv", number = 10, repeats = 10
                       , verboseIter = FALSE, sampling = "smote")
  
  set.seed(100)
  smoteFormula <- caret::train(Planting.coverage ~ .,
                                data = modelTrainDT,
                                method = "rf",# bartMachine
                                preProcess = c("scale", "center"),
                                trControl = smoteCtrl)
  # Large-scale forests
  predictionResult <- predict(smoteFormula, newdata = modelTestNAsDT, type = "prob")
  expectedResult <- modelTestNAsExpectedDT$Planting.coverage
  resultMatrixSMOTE <<- data.table(actual = expectedResult, predictionResult)
  
  predictValues <- colnames(resultMatrixSMOTE)[apply(resultMatrixSMOTE, 1, which.max)]
  
  ##  Accuracy feedback
  simpleAccuracyTestModel("SMOTE", predictValues)
  # modelAccuracyList$SMOTE
}


############        Model with Synthetic Minority Over-sampling Technique
## Input:   the dataset to be check
## Output:  a list of:
#### numeric and factor column names
#### Frequency tables of all FACTOR variables
## Use of Output: obtain the returned result
## TEST SETUP
test_modelWithSimple <- function() {
  modelTrainRatio <- 0.75
  dtToModel <- canterCleansed
  splitTrainTestModel(dtToModel, modelTrainRatio)
  i <- 1
}
## FUNCTION BODY
modelWithSimple <- function() {
  # test_modelWithSimple()
  simpleFormula <- Planting.coverage ~ .
  simpleModel <- rpart(simpleFormula, data = modelTrainDT, method = "class")
  predictionResult <- predict(simpleModel, newdata = modelTestNAsDT)
  predictionValues <- colnames(predictionResult)[apply(predictionResult, 1, which.max)]
  simpleResultDT <<- data.table(expectedResult = modelTestNAsExpectedDT$Planting.coverage
                                , predictionResult = predictionValues)
  
  simpleAccuracyTestModel("Simple", predictionValues)
  # confMatAccuracyTestModel(predictionResult)
  modelTabSimple <<- table(actual = modelTestNAsExpectedDT$Planting.coverage
                           , predict = predictionValues)
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



############        Simple Accuracy Test - Modelling
## Input:   the dataset to be check
## Output:  a list of:
#### numeric and factor column names
#### Frequency tables of all FACTOR variables
## Use of Output: obtain the returned result
## TEST SETUP
test_simpleAccuracyTestModel <- function() {
  modelResultList <- c("a", "b",)
  predictionResult <-
  modelMethod <- "ROSE"
}
## FUNCTION BODY
simpleAccuracyTestModel <- function(modelMethod, predictionResult) {
  # test_simpleAccuracyTestModel()
   
  expectedResult <- modelTestNAsExpectedDT$Planting.coverage
  
  ##  Simple accuracy
  matchedCount <- length( which(expectedResult == predictionResult) )
  modelAccuracy <- round(matchedCount / modelNALength, 2)
  modelAccuracyList[[modelMethod]] <<- list( 
    resultTable = data.table(expectedResult, predictionResult)
    , accuracy = modelAccuracy)
}


############        Confusion Matrix Accuracy Test - Model
## Input:   the dataset to be check
## Output:  a list of:
#### numeric and factor column names
#### Frequency tables of all FACTOR variables
## Use of Output: obtain the returned result
## TEST SETUP
test_confMatAccuracyTestModel <- function() {
  modelResultList <- c("a", "b",)
  predictionResult <-
  modelMethod <- "ROSE"
}
## FUNCTION BODY
confMatAccuracyTestModel <- function(predictionResult) {
  # test_confMatAccuracyTestModel()
   
  expectedResult <- modelTestNAsExpectedDT$Planting.coverage
  targetLevels <- levels(expectedResult)
  modelResultConfMatList <- vector("list", length(targetLevels))
  
  ##  Confusion Matrix
  for (i in seq(1:length(targetLevels))) {
    positive.class <- targetLevels[i]
    # in the i-th iteration, use the i-th class as the positive class
    modelResultConfMatList[[i]] <- confusionMatrix(expectedResult, predictionResult,
                               positive = positive.class)
  }
  return(modelResultConfMatList)
}

############        Modelling Master
## Input:   training ratio
## Output:  a list of:
#### numeric and factor column names
#### Frequency tables of all FACTOR variables
## Use of Output: obtain the returned result
## TEST SETUP
test_modelMaster <- function() {
  modelTrainRatio <- 0.75
  dtToModel <- canterCleansed
}
## FUNCTION BODY
modelMaster <- function(modelTrainRatio = 0.75) {
 # test_modelMaster()
 
 #### Impute NAs in Pruning variable
 # pruningImputeForm <- recipe(Pruning ~ . , data = canterCleansed) %>%
 #   step_naomit(all_outcomes(), skip = TRUE) %>%
 #   step_knnimpute(all_predictors())
 # 
 # pruningImputeTrained <- caret::train(pruningImputeForm, data = canterCleansed, method = "Rborist")
 # dtToPredict <- canterCleansed[ is.na(canterCleansed$Pruning), ]
 # pruningPredResult <- predict(pruningImputeTrained, newdata = dtToPredict)

 ##  Merge prediction into canterCleansed
 # canterCleansed_t <- canterCleansed
 # canterCleansed_t[ is.na(canterCleansed_t$Pruning), ]$Pruning <- pruningPredResult
 # canterCleansed <<- canterCleansed_t
 # View(canterCleansed)
 #### Impute NAs in Pruning variable -- END
  
  canterCleansedNoNA <<- na.omit(canterCleansed)
  
  # print(modelTrainRatio)
  # splitTrainTestModel(canterCleansed, modelTrainRatio)
  splitTrainTestModel(canterCleansedNoNA, modelTrainRatio)
  
  ##  Call the modelling methods
  modelWithSimple()
  modelWithWeights()
  modelWithRecipe()
  modelWithROSE()
  # modelWithSMOTE()

  ##  Obtain modelling results
  modelAccuracyDT <<- data.table(method = names(modelAccuracyList)
                                    , accuracy = NA_real_  )
  
  masterModelResultDT <<- data.table(method = character(), expectedResult = character()
                                    , predictResult = character())
  
  ##  Unwrap to get result table and accuracy
  for ( i in seq(1:length(names(modelAccuracyList))) ) {
    currentSet <- modelAccuracyList[i]
    methodName <- as.character(names(currentSet))
    
    unlistDT <- as.data.table(unlist(currentSet, recursive = FALSE))
    v <- as.numeric(unique(unlistDT[, 3]))
    ##  Take out accuracy column
    resultDT <- unlistDT[, -3]
    names(resultDT) <- c("expectedResult", "predictResult")
    resultDT[, method := rep(methodName, nrow(resultDT)) ]
    ## Form result table to add into master result table
    resultDT <- setcolorder(resultDT, c("method","expectedResult","predictResult"))

    masterModelResultDT <<- base::rbind(masterModelResultDT, resultDT)
    modelAccuracyDT[ method == methodName, accuracy := v ]
  }
  # masterModelResultDT
}
  
################## *******             CLASSIFICATION VISUALIZATION *******              ##################
########      Tree with Recipt model
## TEST SETUP
test_plotRecipeTree <- function() {
  dtToPlot <- na.omit(canterCleansed)
}
## FUNCTION BODY
plotRecipeTree <- function(dtToPlot) {
  # test_plotRecipeTree()

  recipeBalance <- recipe(Planting.coverage ~ ., data = dtToPlot ) %>%
    step_downsample(all_outcomes(), ratio = 10, skip = FALSE) %>%
    step_upsample(all_outcomes(), ratio = 1, skip = FALSE) %>%
    prep(data = dtToPlot)
  
  balancedData <- bake(recipeBalance, dtToPlot)
  # table(balancedData$Planting.coverage)

  recipeFormula <- caret::train(Planting.coverage ~ ., data = dtToPlot, method = "rpart2", metric = "Accuracy",
             trControl = trainControl(method = "boot", number = 20))
  
}

########      PLOT GOOGLE VIS COLUMN CHART (BARCHART)
## TEST SETUP
test_plotRPartTree <- function() {
  dtToPlot <- na.omit(canterCleansed)
}
## FUNCTION BODY
plotRPartTree <- function(dtToPlot) {
  # test_plotRPartTree()
  simpleFormula <- Planting.coverage ~ .
  simpleModel <- rpart(simpleFormula, data = modelTrainDT, method = "class")
  
  # Visualize the decision tree with rpart.plot
  rpart.plot(simpleModel, box.palette="RdBu", shadow.col="gray", nn=TRUE
             , main = "Decision Tree to answer: How long does the tree stand?")
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
modelAccuracyList <<- list()

##  INITIAL IMPUTATION CALL, with default setup
##  UNCOMMENT BELOW WHEN LIVE!
imputeMaster(canterCleansed, 0.75, "Pruning")

##  INITIAL MODELLING CALL, with default setup
modelMaster(0.75)
# canterCleansed




dev <- function() {
######    Current scope - modelling
  # View(canterCleansed)
  View(unlist(modelResultConfMatROSE, recursive = FALSE))
  print(modelResultConfMatROSE)
  # View(predictionResult)
  # attributes(predictionResult)
  # class(predictionResult)
  # str(predictionResult)
  # dimnames(predictionResult)
  objectInEnv <- ls(envir=.GlobalEnv)
  objectInEnv[grep("model", objectInEnv)]
  ?grep
  
  modelAccuracyList
  modelAccuracyDT            

}






############        Model with Purely Under-sampling
## Input:   the dataset to be check
## Output:  a list of:
#### numeric and factor column names
#### Frequency tables of all FACTOR variables
## Use of Output: obtain the returned result
## TEST SETUP
# test_modelWithUnderSample <- function() {
#   modelTrainRatio <- 0.75
#   dtToModel <- canterCleansed
#   splitTrainTestModel(dtToModel, modelTrainRatio)
# }
# ## FUNCTION BODY
# modelWithUnderSample <- function() {
#   # test_modelWithUnderSample()
#   
#   modelTrainDT$Planting.coverage
#   View(modelTrainDT)
#   anyNA(modelTrainDT)
#   is.factor(modelTrainDT$Pruning)
#   levels(modelTrainDT$Planting.coverage)
#   underSampledTrainDT <- ovun.sample(Planting.coverage ~ ., data = modelTrainDT, method = "under"
#                                      , N = maxNumToMatch)$data
#   table(underSampledTrainDT$Planting.coverage)
# }
#   
# 
# 
# ############        Model with Purely Over-sampling
# ## Input:   the dataset to be check
# ## Output:  a list of:
# #### numeric and factor column names
# #### Frequency tables of all FACTOR variables
# ## Use of Output: obtain the returned result
# ## TEST SETUP
# test_modelWithOverSample <- function() {
#   trainRatio <<- 0.75
#   dtToModel <<- canterCleansed
# }
# ## FUNCTION BODY
# modelWithOverSample <- function(dtToModel, trainRatio = 0.75) {
#   test_modelWithOverSample()
# 
#   data_balanced_over <- ovun.sample(Planting.coverage ~ ., data = trainDT
#                                     , method = "over", N = modelRowsLength)$data
#   table(data_balanced_over$Planting.coverage)
# 
#   roc.curve(hacide.test$cls, pred.treeimb[,2], plotit = F)
# }
  
  
    # print("I'm here!")