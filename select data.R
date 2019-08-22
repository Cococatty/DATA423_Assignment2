
install.packages("boot")
library(boot)
data("DanishWelfare")
summary(DanishWelfare)
View(DanishWelfare)

data("DanishWelfare")
ftable(xtabs(Freq ~ ., data = DanishWelfare))
ftable(xtabs(Alcohol ~ ., data = DanishWelfare))

library(lme4)
data("VerbAgg")
View(VerbAgg)
summary(tips)
summary(VerbAgg)


library("carData")
data("WVS")
summary(WVS)

library("carData")
data("Mathlevel")
summary(Mathlevel)

data("Diamonds2")
data("Diamonds")
data("diamonds2")
# summary(diamonds)
summary(Diamonds2)
View(Diamonds2)
# summary(Diamonds)
# summary(diamonds)
