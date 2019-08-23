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

multiplier <- 1.5

basicDataCleansing <- function() {
  # DROP COLUMNS THAT HAVE ONLY ONE LEVEL
  canterburyWoodCleaned <<- canterburyWood[, sapply(canterburyWood, function(col) length(unique(col))) > 1]
  numericCols <<- unlist(lapply(canterburyWoodCleaned, is.numeric)) 
  factorCols <<- unlist(lapply(canterburyWoodCleaned, is.factor))
}


dev <- function() {
  summary(canterburyWood)
  dfSummary(canterburyWood)
    
    
  ?subset
  class(canterburyWood)
  str(canterburyWood)
  # type(canterburyWood)
  levels(canterburyWood$Pulplog.thinnings..m3.ha.)
  
  str(canterburyWood)
  unique(canterburyWood)
  
  lapply(canterburyWood, unique)
  
  levels(woodData)
  View(canterburyWood)



}



# attach(Diamonds2)
# library(rpart)
# library(rpart.plot)
# formula <- Color ~ .
# fit <- rpart(formula, Diamonds2, method = "class") # predicted class; predicted probability of survivals; percentage of observations
# rpart.plot(fit, type = 4)

# or use draw.tree; can you find the difference
# install.packages("cluster")
# library(maptree)
# library(cluster)
# plot(as.party(fit))
# draw.tree(fit)





# boxplot(woodData[, c(numericCols)])
# 
# boxplot <- gvisCandlestickChart(woodData[, c(numericCols)], 
#                                options=list(legend='none'))

# detach(Diamonds2)
