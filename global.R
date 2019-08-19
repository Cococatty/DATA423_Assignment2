#  Loading required library
#

# install.packages("semantic.dashboard")
# install.packages("summarytools")
library(readr, quietly = TRUE)
library(shiny, quietly = TRUE)
library(dashboard, quietly = TRUE)
library(data.table, quietly = TRUE)

library(summarytools)
library(googleVis)


# Read in the data set
sourceData <- read_csv(file = "FinalData.csv")
sourceDT <<- data.table(sourceData) 

# str(sourceDT)
# str(formattedDT)

cleanseData <- function() {
  formattedDT <- sourceDT[, ':=' (
    ID = ID,
    Gender = as.factor(Gender),
    Grade = factor(Grade),
    Horoscope = factor(Horoscope),
    Subject = factor(Subject),
    IntExt = factor(IntExt),
    OptPest = factor(IntExt),
    ScreenTime = ScreenTime,
    Sleep = Sleep,
    PhysActive = PhysActive,
    HrsHomework = HrsHomework,
    SpendTime1 = factor(SpendTime1),
    SpendTime2 = factor(SpendTime2),
    Self1 = Self1,
    Self1_factor = factor(Self1),
    Self2 = Self2,
    Self2_factor = factor(Self2),
    Career = Career,
    Career_factor = factor(Career),
    Superpower = Superpower,
    Superpower_factor = factor(Superpower)
    )]
  
  factorsDT <<- Filter(is.factor, formattedDT)
  factorNames <- names(factorsDT)
  overallResult <<- ""
  
  for ( i in 1:length(factorNames) ) {
    v <- factorNames[i]
    currentValues <- formattedDT[, get(v)]
    currentLevels <- length(unique(currentValues))
    lowerCaseLevels <- length(unique(tolower(currentValues)))
    # print(i)
    
    if (currentLevels != lowerCaseLevels) {
      overallResult <<- paste(overallResult
                              , v, "have", currentLevels
                              , "unique values; with lower case modification, there exist"
                              , lowerCaseLevels, "unique values."
                              , "Therefore, value cleansing is required."
                              , sep = "\n")
      
      ## Change values to all lower cases
      formattedDT <<- formattedDT[, as.character(v) := tolower(currentValues)]
    }
  }
}

cleanseData()
View(formattedDT)

basicEDA <- function() {
  # str(sourceDT)
  # summary(sourceDT)
 
}


development <- function() {
  View(sourceDT)
  names(sourceDT)
  
  summary(sourceDT)
  
  view(dfSummary(sourceDT))
  
  summaryTEST <- dfSummary(sourceDT)
  str(summaryTEST)
  
 
}


  # print(overallResult)
# formattedDT <<- 
pieChartsEDA <- function() {
  names(formattedDT)
  summary(formattedDT$ID)
  
  # piechartDF <- data.frame(formattedDT[, -c("ID")])
  
  lenCol <- length(names(formattedDT))
  levels(formattedDT)
  str(formattedDT)
  str(formattedDT$Gender)
  is.factor(formattedDT$Gender)
  class(formattedDT$Gender)
  class(formattedDT)
  levels(formattedDT$Gender)
  
  library(plyr)
  
  totalNumFactor <- length(factorNames)
  
  par(mfrow=c(round(totalNumFactor/4), 4))
  
  ## Initial plot
  freqData <- count(factorsData, vars = factorNames[1])
  # pie(freqData$freq, labels = freqData$Gender)
  ?pie
  pieCharts <- gvisPieChart(freqData)
  freqData <- count(factorsData, vars = factorNames[2])
  nextChart <- gvisPieChart(freqData)
  
  pieCharts <- gvisMerge(nextChart, pieCharts
                         , horizontal=FALSE, tableOptions="bgcolor=\"#AABBCC\""
                         # ,tableOptions="cellspacing=10"
                         # horizontal = TRUE, tableOptions = "border=\"0\"",
                         # chartid = i
  )
  plot(pieCharts)
  # rm(pieCharts)
  # for (i in factorNames[2:4]) {
  #   # print(i)
  #   freqData <- count(factorsData, vars = i)
  #   # print(freqData)
  #   currentPie <- gvisPieChart(freqData)
  #   plot(currentPie)
  #   # pie(freqData)
  #   pieCharts <- gvisMerge(currentPie, pieCharts
  #                          , horizontal=TRUE,
  #                          tableOptions="cellspacing=10"
  #                          # horizontal = TRUE, tableOptions = "border=\"0\"",
  #                          # chartid = i
  #                          )
  # }
  plot(pieCharts)  
  # ?par
}

checkVariables <- function(threshhold) {
  novelDT <- data.table(colName = character(), classType = character(), ratio = numeric(), result = character())
  
  # for (col in colnames(refreshAppDT())) {
  #   classPro <- class(refreshAppDT()[, col])
  #   ratio <-
  #     round(length(unique(refreshAppDT()[, col])) / nrow(refreshAppDT()), 2)
  #   
  #   if (ratio == 1) {
  #     result <- "continuous"
  #   } else if (ratio > input$novelThreshold) {
  #     result <- "as good as continuous"
  #   } else {
  #     result <- "not continuous"
  #   }
  #   novelDT <- rbind(novelDT, list(col, classPro, ratio, result))
  # }
  # novelDT
}

###########             Explore and EDA Tab TEXT              ###########


# edaFirstImpression <- 

# df <- read_csv('https://raw.githubusercontent.com/lgellis/STEM/master/DATA-ART-1/Data/FinalData.csv', col_names = TRUE)
# 
# detach("package:shinythemes", unload = TRUE)