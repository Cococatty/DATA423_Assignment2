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
# Read in the data set
allWoodData <- read.delim("2015-yield-tables-Canterbury.tsv", header = TRUE, sep = "\t")
canterburyWood <- subset(allWoodData, Wood.Supply.Region == "Canterbury") 
canterburyWood <- droplevels.data.frame(canterburyWood)
numericColsCanta <<- unlist(lapply(canterburyWood, is.numeric)) 
factorColsCanta <<- unlist(lapply(canterburyWood, is.factor))
  
# multiplier <- 1.5

# basicDataCleansing <- function() {
# DROP COLUMNS THAT HAVE ONLY ONE LEVEL
canterburyWoodCleaned <<- canterburyWood[, sapply(canterburyWood, function(col) length(unique(col))) > 1]
## Fix column names
names(canterburyWoodCleaned) <- gsub("\\.\\.", ".", names(canterburyWoodCleaned))

numericColsCleansed <<- unlist(lapply(canterburyWoodCleaned, is.numeric)) 
numericColsCleansed <<- numericColsCleansed[ numericColsCleansed == TRUE]

factorColsCleansed <<- unlist(lapply(canterburyWoodCleaned, is.factor))
factorColsCleansed <<- factorColsCleansed[ factorColsCleansed == TRUE]
# }


########      PLOT GOOGLE VIS COLUMN CHART (BARCHART)
plotGVisColChart <- function(dataToPlot, factorList) {
  gVisColChartList <- list()
  dataToPlot <- canterburyWoodCleaned
  factorList <- factorColsCleansed
  
  for (i in names(factorList)) {
    currentFreqDF <- as.data.frame( table(dataToPlot[, i]) )
                                    #, dnn = c("tes", "Freq")
                                    
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
  return(visPlotList)
}

dev <- function() {
  ?gvisColumnChart
}

# i <- "Species"
# i <- "Age.years."
# print(i)
    
    