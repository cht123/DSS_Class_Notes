setwd("~/Classes/DSS/04_Explatory_Data_Analysis")

# get data
pollution <- read.csv("avgpm25.csv", colClasses = c("numeric", "character", "factor", "numeric", "numeric"))

head(pollution)


####Explatory Graphs
#Simple summaries of one dimension

## Five number summary
summary(pollution$pm25)

## Boxplot
boxplot(pollution$pm25, col = "blue")
abline(h = 12, col = "red") # sets a horizontal line at 12

boxplot(pm25~region, data=pollution, col= c("red", "green"))

## Histogram
hist(pollution$pm25, col = "green", breaks = 100) # breaks is optional
abline(v=12, lwd = 4, col = "red") # lwd sets the weight of the line for the cutoff
abline(v = median(pollution$pm25), col = "magenta", lwd = 2)
rug(pollution$pm25)

## Multiple Histograms
par(mfrow = c(2,1), mar = c(4,4,2,1))
hist(subset(pollution, region == "east")$pm25, col = "green")
hist(subset(pollution, region == "west")$pm25, col = "green")

## Density plot

## Barplot
barplot(table(pollution$region), col = "wheat", main = "Number of Counties in Each Region")

## Scatterplot
with(pollution, plot(latitude, pm25, col = region))
abline(h = 12, lwd = 2, lty = 2)

par(mfrow = c(1,2), mar = c(5,4,2,1))
with(subset(pollution, region == "west"), plot(latitude, pm25, main = "West"))
with(subset(pollution, region == "east"), plot(latitude, pm25, main = "East"))

## 3 Key plotting systems
# Base
# Lattice
# ggplot2

