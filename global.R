#Load the readr library to bring in the dataset
library(readr)

#Download the data set
# df <- read_csv('https://raw.githubusercontent.com/lgellis/STEM/master/DATA-ART-1/Data/FinalData.csv', col_names = TRUE)
sourceDT <- read_csv(file = "FinalData.csv")
View(sourceDT)
