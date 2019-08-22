#  Loading required library
#

# install.packages("semantic.dashboard")
# install.packages("summarytools")
library(readr, quietly = TRUE)
library(shiny, quietly = TRUE)
library(dashboard, quietly = TRUE)

library(Stat2Data)
library(data.table, quietly = TRUE)
library(ggplot2, quietly = TRUE)
library(summarytools, quietly = TRUE)
library(googleVis, quietly = TRUE)


# Read in the data set

data("Diamonds2")
diamondsData <- Diamonds2
  # diamonds
summary(diamondsData)


dfSummary(diamondsData)


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






multiplier <- 1.5
numericCols <- unlist(lapply(diamondsData, is.numeric)) 
factorCols <- unlist(lapply(diamondsData, is.factor)) 

# boxplot(diamondsData[, c(numericCols)])
# 
# boxplot <- gvisCandlestickChart(diamondsData[, c(numericCols)], 
#                                options=list(legend='none'))

# detach(Diamonds2)
