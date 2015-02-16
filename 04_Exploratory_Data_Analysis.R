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
plot(x)


###################################### sample analysis ############################################

# base table
mheight <- data.frame(rnorm(100, mean = 70, sd = 3))
names(mheight) <- 'height'
mweight <- data.frame((mheight * (2.5 + rnorm(100, mean = 1, sd = 0.2))))
names(mweight) <- 'weight'
mgender <- data.frame(gender = gl(1, 100, labels = c("Male")))
mdata <- bind_cols(mheight, mweight, mgender)

wheight <- data.frame(rnorm(100, mean = 60, sd = 2))
names(wheight) <- 'height'
wweight <- data.frame((wheight * (1.65 + rnorm(100, mean = 1, sd = 0.2))))
names(wweight) <- 'weight'
wgender <- data.frame(gender = gl(1, 100, labels = c("Female")))
wdata <- bind_cols(wheight, wweight, wgender)

library(dplyr)
data <- bind_rows(mdata, wdata)

# calc stats
mean_vec <- data.frame(mean_vec = rep(mean(data$height), length(data$height)))
sd_vec <- data.frame(sd_vec = rep(sd(data$height), length(data$height)))
norm_vec <- data.frame(norm_vec = ((data$height - mean_vec$mean_vec)/sd_vec$sd_vec))

data <- bind_cols(data, mean_vec, sd_vec, norm_vec)

plot(data$height,data$weight, type = "n", xlab = "height", ylab = "weight", main = "Weight versus height")
points(data$height[data$gender=="Male"], data$weight[data$gender=="Male"], col = "Blue", pch = 20)
points(data$height[data$gender=="Female"], data$weight[data$gender=="Female"], col = "Pink", pch = 20)
abline(h = 200, lwd = 2, lty = 2)
abline(v = 63, lwd = 2, lty = 2)
mod <- lm(data$weight~data$height)
abline(mod, col = "green", lwd = 3)
legend("topleft", pch = 20, col = c("blue", "pink"), legend = c("Male", "Female"))
library(dplyr)
library(tidyr)
tl <- data %>% group_by(gender) %>% summarise(num = n(), na.rm = T) %>% spread(gender, num)
boxplot(data$weight~data$gender, xlab = "Gender", ylab = "Weight", main = "Boxplot of weight by gender", col = c("pink", "blue"))
mtext(paste("R score "|mod, 100, 250)

      
      ## 3 Key plotting systems
# Base
# Lattice
# ggplot2

# Lattice Plotting
library(lattice)
library(datasets)
class(xyplot(Ozone~Wind, data = airquality))

airquality <- transform(airquality, Month = factor(Month))
xyplot(Ozone~Wind|Month, data = airquality, layout = c(5,1))

# plotting functions in Lattice Plot
set.seed(10)
x <- rnorm(100)
f <- rep(0:1, each = 50)
y <- x + f - f *x + rnorm(100, sd = 0.5)
f <- factor(f, labels = c("Group 1", "Group 2"))
xyplot(y~x|f, layout = c(2,1))

#add median line
xyplot(y~x|f, panel = function(x,y,...) {
  panel.xyplot(x,y,...)
  panel.abline(h = median(y), lty = 2)
})

# add linear model line
xyplot(y~x|f, panel = function(x,y,...) {
  panel.xyplot(x,y,...)
  panel.lmline(x,y, col = 2)
})


##GGPLOT
library(ggplot2)
str(mpg)
qplot(displ, hwy,data = mpg)
qplot(displ, hwy,data = mpg, color = drv)
qplot(displ, hwy,data = mpg, color = drv, geom = c("point","smooth"))

# only specify a single vairable to create a histogram
qplot(hwy, data = mpg, fill = drv)

# adding facets
qplot(displ, hwy, data = mpg, facets = .~drv)
qplot(hwy, data = mpg, facets = .~drv)
qplot(hwy, data = mpg, facets = drv~., binwidth = 2)

qplot(displ, hwy, data = mpg, geom = c("point", "smooth"), method = "lm", facets = .~drv)

# standard gggplot
str(mpg)
g <- ggplot(mpg, aes(displ, hwy))
#g <- g + geom_point(aes(color = class), size = 4, alpha = 1/2) # sets color by factor variable
g <- g + geom_point(color = "steelblue", size = 4, alpha = 1/2) # sets color for all variables
g <- g + facet_grid(.~drv)
g <- g + geom_smooth(method = "lm", size = 1, linetype = 2)
g <- g + theme_bw()
g <- g + labs(title = "Mileage Analysis", x = "Displacement", y = "Highway Mileage")
g

# setting limits
# in base plot
testdat <- data.frame(x = 1:100, y = rnorm(100))
testdat[50,2] <- 100
plot(testdat$x, testdat$y, type = "l", ylim = c(-3,3))

g <- ggplot(testdat, aes(x=x,y=y))
#g + geom_line() + ylim(-3,3) # this subsets the data and removes any values outside the limits
g + geom_line() + coord_cartesian(ylim = c(-3, 3)) # leaves the outlier in the data but off the oage

## Week 2 Quiz
#1. Under the lattice graphics system, what do the primary plotting functions like xyplot() and bwplot() return?
# object trellis

#2 what does this return
library(nlme)
library(lattice)
xyplot(weight ~ Time | Diet, BodyWeight)


#3
#Annotation of plots in any plotting system involves adding points, lines, or text to the plot, in addition to customizing axis labels or adding titles. Different plotting systems have different sets of functions for annotating plots in this way. Which of the following functions can be used to annotate the panels in a multi-panel lattice plot?
#lpoints()

#4
library(lattice)
library(datasets)
data(airquality)
p <- xyplot(Ozone ~ Wind | factor(Month), data = airquality)
p

#5
#In the lattice system, which of the following functions can be used to finely control the appearance of all lattice plots?
# trellis.par.set()

#6 
#Question 6
#What is ggplot2 an implementation of?
#the Grammar of Graphics developed by Leland Wilkinson

#7
library(datasets)
data(airquality)
library(ggplot2)
airquality <- transform(airquality, Month = factor(Month))
qplot(Wind, Ozone, data = airquality, facets = . ~ Month)

#8
#What is a geom in the ggplot2 system?
#a plotting object like point, line, or other shape

#9 
library(ggplot2)
g <- ggplot(movies, aes(votes, rating))
#g + geom_line()
print(g)
#ggplot does not yet know what type of layer to add to the plot.

#10
qplot(votes, rating, data = movies) + geom_smooth()
