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

airquality <- transform(airquality, Month = factor(Month))
boxplot(Ozone~Month, airquality, xlab = "Month", ylab = "Ozone (ppb)")




## Histogram
hist(pollution$pm25, col = "green", breaks = 100) # breaks is optional
abline(v=12, lwd = 4, col = "red") # lwd sets the weight of the line for the cutoff
abline(v = median(pollution$pm25), col = "magenta", lwd = 2)
rug(pollution$pm25)

## Multiple Histograms
par(mfrow = c(2,1), mar = c(4,4,2,1))
hist(subset(pollution, region == "east")$pm25, col = "green")
hist(subset(pollution, region == "west")$pm25, col = "green")

library(datasets)
hist(airquality$Ozone)



## Density plot

## Barplot
barplot(table(pollution$region), col = "wheat", main = "Number of Counties in Each Region")

## Scatterplot
with(pollution, plot(latitude, pm25, col = region))
abline(h = 12, lwd = 2, lty = 2)

with(airquality, plot(Wind, Ozone, type = "n")) # n sets up the plot without adding anything to it
title(main="Ozone and Wind in New York City")
with(subset(airquality, Month==5), points(Wind, Ozone, col = "blue"))
with(subset(airquality, Month!=5), points(Wind, Ozone, col = "red"))
legend("topright", pch = 1, col = c("red", "blue"), legend = c("May", "Other months"))
model <- lm(Ozone~Wind, airquality) # create a linear model
abline(model, lwd = 2) # plot the model on the graph

# Create multiple scatterplots
par(mfrow = c(1,2), mar = c(4,4,2,1), oma = c(0,0,2,0)) # set outer margin to enable outer label
with(airquality, {
  plot(Wind, Ozone, main = "Wind and Ozone")
  text(18, 125, "Cool Text") # enables addition of text at specified point
  legend("topright", legend = "Data", pch = 20)
  plot(Wind, Solar.R, main = "Ozone and Solar Radiation")
  mtext("Ozone and Weather in New York City", outer = TRUE) # create outer label
})


par(mfrow = c(1,2), mar = c(5,4,2,1))
with(subset(pollution, region == "west"), plot(latitude, pm25, main = "West"))
with(subset(pollution, region == "east"), plot(latitude, pm25, main = "East"))


## Plotting demonstration

x <- rnorm(100)
y <- x + rnorm(100)
g <- gl(2, 50, labels = c("Male", "Female"))
plot(x,y, type = "n")
points(x[g=="Male"], y[g=="Male"], col = "Blue")
points(x[g=="Female"], y[g=="Female"], col = "Pink", pch = 20)


## 3 Key plotting systems
# Base
# Lattice
# ggplot2


