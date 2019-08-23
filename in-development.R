
Column <- gvisColumnChart(canterburyWoodCleaned[, factorCols])
Column <- gvisColumnChart(canterburyWoodCleaned[, Owner.size ])
names(canterburyWoodCleaned)
plot(Column)
?table

data_gathered <- diamondsDT %>%
  gather(cut, value, 2:4)

plotDT <- melt(diamondsData$value, id.vars="cut")
head(mdat)
ggplot(mdat, aes(variable, value, fill=day)) + 
  geom_bar(stat="identity", position="dodge")

ggplot() + 
  geom_bar(stat = "identity")

t <- table(cut= diamondsDT[, "cut"])
class(t)
str(t)
unlist(t)
dimnames(t)
boxplot(t)


gv

df <- diamondsData %>%
  # filter(color %in% c("J", "D")) %>%
  group_by(Clarity) %>%
  summarise(counts = n())  %>%
  group_by(Color)

diamondDF <- data.frame(Color = diamondsData$Color, )
crosstab()

tplot <- gvisBarChart( as.data.frame(t) )
plot(tplot)
?ftable
ftable(Titanic, row.vars = 1:2, col.vars = "Survived")
gvisBarChart(ftable(Titanic, row.vars = 1:2, col.vars = "Survived"))

df=data.frame(country=c("US", "GB", "BR"), 
              val1=c(10,13,14), 
              val2=c(23,12,32))

names(diamondsData)
t <- table(diamondsData[, factorCols], dnn = c("Color","Clarity"))

t1 <- gvisColumnChart(df)
plot(t1)
