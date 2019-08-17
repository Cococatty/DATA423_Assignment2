#  Loading required library
#

# install.packages("semantic.dashboard")
# install.packages("summarytools")
library(readr, quietly = TRUE)
library(shiny, quietly = TRUE)
library(dashboard, quietly = TRUE)
library(data.table, quietly = TRUE)

library(summarytools)


# Read in the data set
sourceDT <- read_csv(file = "FinalData.csv")


development <- function() {
  View(sourceDT)
  names(sourceDT)
  
  summary(sourceDT)
  
  view(dfSummary(sourceDT))
  
  summaryTEST <- dfSummary(sourceDT)
  str(summaryTEST)
  
  
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

# df <- read_csv('https://raw.githubusercontent.com/lgellis/STEM/master/DATA-ART-1/Data/FinalData.csv', col_names = TRUE)
# 
# detach("package:shinythemes", unload = TRUE)