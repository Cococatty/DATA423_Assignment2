#  Loading required library
#  , quietly = TRUE
library(readr)
library(shiny)
library(dashboard)

library(data.table)
library(summarytools)
library(dplyr)
library(googleVis)
library(ggplot2)


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
  View(canterCleansed)
  summary(canterCleansed)
  summarytools::dfSummary(canterCleansed, style = "multiline")
  ?gvisColumnChart
  
  freq <- iris
  
  n <- names(canterCleansed)
  n <- n[sapply(n, function(var) is.factor(canterCleansed[[var]]))]
  
  for(var in factorColsCleansed){
    cnt <- dplyr::count(canterCleansed, get(var))
    print(cnt)
  }

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
