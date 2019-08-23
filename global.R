#  Loading required library
#

# install.packages("semantic.dashboard")
# install.packages("summarytools")
library(readr, quietly = TRUE)
library(shiny, quietly = TRUE)
library(dashboard, quietly = TRUE)

library(data.table, quietly = TRUE)
library(summarytools, quietly = TRUE)
library(googleVis, quietly = TRUE)

library(ggplot2, quietly = TRUE)
# library(Stat2Data)

########################          ASSISTING FUNCTIONS          ########################

############        Find Columns Data Types of Specified Dataset
## Input:   the dataset to be check
## Output:  a list of numeric and factor column names
## Use of Output: obtain the returned result 
classifyColTypes <- function(dtToClassify) {
  dtNames <- names(dtToClassify)
  numericCols <- dtNames[ sapply(dtNames, function(var) is.numeric(dtToClassify[[var]])) ]
  factorCols <- dtNames[ sapply(dtNames, function(var) is.factor(dtToClassify[[var]])) ]
  resultList <- list(numericList = numericCols, factorList = factorCols)
  return(resultList)
}

########################          GLOBAL OBJECTS          ########################
# Read in the data set
allWoodData <- read.delim("2015-yield-tables-Canterbury.tsv", header = TRUE, sep = "\t")
## Fix column names
names(allWoodData) <- gsub("\\.\\.", ".", names(allWoodData))
##  Get Canterbury Data
canterSource <- subset(allWoodData, Wood.Supply.Region == "Canterbury") 
canterSource <- droplevels.data.frame(canterSource)
canterSourceColsType <- classifyColTypes(canterSource)


# multiplier <- 1.5

# basicDataCleansing <- function() {
# DROP COLUMNS THAT HAVE ONLY ONE LEVEL
canterCleansed <<- canterSource[, sapply(canterSource, function(col) length(unique(col))) > 1]
canterCleansedColsType <- classifyColTypes(canterCleansed)

# numericColsCleansed <<- unlist(lapply(canterCleansed, is.numeric)) 
# numericColsCleansed <<- numericColsCleansed[ numericColsCleansed == TRUE]
# 
# factorColsCleansed <<- unlist(lapply(canterCleansed, is.factor))
# factorColsCleansed <<- factorColsCleansed[ factorColsCleansed == TRUE]
# }


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
plotGVisColChart <- function(dataToPlot, factorList) {
  gVisColChartList <- list()
  # TEST
  # dataToPlot <- canterCleansed
  # factorList <- canterCleansedColsType$factorList
  
  for (i in factorList) {
    currentFreqDF <- as.data.frame( table(dataToPlot[, i]) ) #, dnn = c("tes", "Freq")
    currentFreqDF$Freq.style <- c("orange") #rainbow(nrow(currentFreqDF))
    
    ## Fix names
    names(currentFreqDF)[names(currentFreqDF) == "Var1"] <- i
    
    currentVisColPlot <- gvisColumnChart(currentFreqDF, xvar = i, yvar = c("Freq", "Freq.style")
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

# i <- "Species"
# i <- "Age.years."
# print(i)
    
    