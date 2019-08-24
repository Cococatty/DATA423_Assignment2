#  Loading required library
#  , quietly = TRUE
# install.packages("caret")

library(readr)
library(shiny)
library(dashboard)

library(data.table)
library(summarytools)
library(dplyr)
library(googleVis)
library(ggplot2)
library(recipes)
library(caret)

########################          ASSISTING FUNCTIONS          ########################

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


########################          BASIC DATA MANIPULATION          ########################
# Read in the data set
allWoodData <- read.delim("2015-yield-tables-Canterbury.tsv", header = TRUE, sep = "\t")
## Fix column names
names(allWoodData) <- gsub("\\.\\.", ".", names(allWoodData))
##  Get Canterbury Data
canterSource <- subset(allWoodData, Wood.Supply.Region == "Canterbury") 
canterSource <- droplevels.data.frame(canterSource)
canterSourceColsType <- classifyColTypes(canterSource)

# DROP COLUMNS THAT HAVE ONLY ONE LEVEL
canterCleansed <<- canterSource[, sapply(canterSource, function(col) length(unique(col))) > 1]
canterCleansedColsType <- classifyColTypes(canterCleansed)


dev <- function() {
  
  standarizedData <- scale(canterSource[ , canterSourceColsType$numericList, drop = FALSE]
                           , center = TRUE, scale = TRUE)
  keyValues <- tidyr::gather(as.data.frame(standarizedData))
  keyValuesDT <- data.table(keyValues)
  
  Candle <- gvisCandlestickChart(keyValuesDT, 
                               options=list(legend='none'))
plot(Candle)

testDT <- data.table(a= seq(1:2), low = c(100,200), Max =  )
  OpenClose
  
  ggplot(mapping = aes(x = keyValuesDT$key, y = keyValuesDT$value, fill = keyValuesDT$key)) +
      geom_boxplot(coef = input$edaSourcePlotMultiplier, outlier.colour = "red") +
      labs(title = paste("Boxplots at IQR multiplier of", input$edaPlotMultiplier),
           x = "Standardised variable value", y = "Std Value") +
      coord_flip()
  
  View(canterCleansed)
  summary(canterCleansed)
  summarytools::dfSummary(canterCleansed, style = "multiline")
  ?gvisColumnChart
  
 

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

########################          MODELLING AND RELEVANT          ########################

############        Train and Test Split
## Input:   the dataset to be split
## Output:  train dataset and test dataset
## Use of Output: train and test models
## TEST SETUP
test_splitTrainTest <- function() {
  trainRatio <<- 0.75
  dtToImpute <<- canterCleansed
}
## FUNCTION BODY
splitTrainTest <- function(dtToImpute, trainRatio = 0.75) {
  # test_splitTrainTest()
  
  ## Calculate sample size
  sampleLength <- floor(nrow(dtToImpute) * trainRatio)
  
  ## Set the seed to make partition reproducible
  # set.seed(5)
  
  ## split data by trainRatio
  trainIndexSet <- sample(seq_len(nrow(dtToImpute)), size = sampleLegth)
  trainDT <<- dtToImpute[trainIndexSet, ]
  testDT <<- dtToImpute[-trainIndexSet, ]
  # nrow(testDT)/nrow(trainDT)
}

############        Impute Data
## Input:   the dataset to be check
## Output:  a list of:
#### numeric and factor column names
#### Frequency tables of all FACTOR variables
## Use of Output: obtain the returned result
## TEST SETUP
test_Recipe <- function() {
  trainRatio <<- 0.75
  dtToImpute <<- canterCleansed
}
## FUNCTION BODY
modelWithRecipe <- function(dtToImpute, trainRatio = 0.75) {
  test_Recipe()

  # canterCleansedColsType$freqTblList$Owner.size
  modelRecipe <- recipe(Owner.size ~ ., data = trainDT) %>%
    step_downsample(all_outcomes(), ratio = 10, skip = FALSE) %>%
    step_upsample(all_outcomes(), ratio = 1, skip = FALSE) %>%
    prep(data = trainDT)
  
  trainData_Recipe <- bake(modelRecipe, trainDT)
  classifyColTypes(trainData_Recipe)
  
  names(trainData)
  table(trainData$Owner.size)
  table(trainData$Pruning)
  table(trainData$Pruning)
  
  caret::train(cls ~ ., data = trainData, method = "rpart2", metric = "Accuracy",
             trControl = trainControl(method = "boot", number = 20))
  
  
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
  dtToImpute <<- canterCleansed
}
## FUNCTION BODY
modelWithWeights <- function(dtToImpute, trainRatio = 0.75) {
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
  trainRatio <<- 0.75
  dtToImpute <<- canterCleansed
}
## FUNCTION BODY
modelWithROSE <- function(dtToImpute, trainRatio = 0.75) {
  test_modelWithWeights()
  
  library(rpart)
  treeimb <- rpart(cls ~ ., data = hacide.train)
  pred.treeimb <- predict(treeimb, newdata = hacide.test)
  
  library(ROSE)  # Random Over Sampling Examples
  ROSE::accuracy.meas(hacide.test$cls, pred.treeimb[,2])
  roc.curve(hacide.test$cls, pred.treeimb[,2], plotit = F)
  
  data_balanced_over <- ovun.sample(cls ~ ., data = hacide.train, method = "over",N = 1960)$data
  table(data_balanced_over$cls)
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
  dtToImpute <<- canterCleansed
}
## FUNCTION BODY
modelWithSMOTE <- function(dtToImpute, trainRatio = 0.75) {
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
  dtToImpute <<- canterCleansed
}
## FUNCTION BODY
predictModels <- function(dtToImpute, trainRatio = 0.75) {
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
test_accuracyTest <- function() {
  trainRatio <<- 0.75
  dtToImpute <<- canterCleansed
}
## FUNCTION BODY
accuracyTest <- function(dtToImpute, trainRatio = 0.75) {
  test_predictModels()
  roc.curve(hacide.test$cls, pred.treeimb[,2], plotit = F)
}

  View(canterCleansed)
  names(canterCleansed)
  
  







