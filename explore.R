
Pie1 <- gvisPieChart(CityPopularity)
plot(Pie1)



require(grDevices)
pie(rep(1, 24), col = rainbow(24), radius = 0.9)

pie.sales <- c(0.12, 0.3, 0.26, 0.16, 0.04, 0.12)
names(pie.sales) <- c("Blueberry", "Cherry",
                      "Apple", "Boston Cream", "Other", "Vanilla Cream")
pie(pie.sales) # default colours
pie(pie.sales, col = c("purple", "violetred1", "green3",
                       "cornsilk", "cyan", "white"))
pie(pie.sales, col = gray(seq(0.4, 1.0, length = 6)))
pie(pie.sales, density = 10, angle = 15 + 10 * 1:6)
pie(pie.sales, clockwise = TRUE, main = "pie(*, clockwise = TRUE)")
segments(0, 0, 0, 1, col = "red", lwd = 2)
text(0, 1, "init.angle = 90", col = "red")

n <- 200
pie(rep(1, n), labels = "", col = rainbow(n), border = NA,
    main = "pie(*, labels=\"\", col=rainbow(n), border=NA,..")

## Another case showing pie() is rather fun than science:
## (original by FinalBackwardsGlance on http://imgur.com/gallery/wWrpU4X)
pie(c(Sky = 78, "Sunny side of pyramid" = 17, "Shady side of pyramid" = 5),
    init.angle = 315, col = c("deepskyblue", "yellow", "yellow3"), border = FALSE)



##---------------------               NEW
##
##

install.packages("recipes")
library(recipes)
data(biomass)
summary(biomass)

biomass_te <- biomass[biomass$dataset == "Testing",]
exNumericCols <- unlist(lapply(biomass_te, is.numeric)) 
std <- scale(biomass_te[,exNumericCols, drop = FALSE], center = TRUE, scale = TRUE)
d <- tidyr::gather(as.data.frame(std))
d <- data.table(d)
ggplot(mapping = aes(x = d$key, y = d$value, fill = d$key)) +
  geom_boxplot(coef = 1.5, outlier.colour = "red") +
  labs(title = paste("Raw Uni-variable boxplots at IQR multiplier of", 1.5),
       x = "Standardised variable value", y = "Std Value") +
  coord_flip()
class(d)





