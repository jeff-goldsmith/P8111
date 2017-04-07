####################################################################
# Jeff Goldsmith
# Jan 19 2016
#
# R code used in P8111 Lecture 01
####################################################################

## clear work space
rm(list = ls())

## set working directory
setwd("~/Desktop/")

## load heights data
# install.packages("alr3")  ## note - you only have to do this once
library(alr3)
library(ggplot2)
library(dplyr)

data(heights)


####################################################################
## initial data looking
####################################################################

## some summary looks at the height data set
heights

heights = tbl_df(heights)
heights
str(heights)

dim(heights)

## look at daughters' heights only
summary(heights[,2])
ggplot(heights, aes(x = Dheight)) + geom_histogram(fill = "#0000FF", alpha = .5, color = "black") +
  labs(x = "Daughter Height", y = "Count", title = "Histogram of Daughter Heights")

## plot daughter height against mother height
ggplot(heights, aes(x = Mheight, y = Dheight)) + geom_point(color = "red") +
  labs(x = "Mother Height", y = "Daughter Height")



####################################################################
## some "regression" analyses
####################################################################

plot0 = ggplot(heights, aes(x = Mheight, y = Dheight)) + geom_point(color = "red") +
  labs(x = "Mother Height", y = "Daughter Height")

## order predictors and outcome
heights = arrange(heights, Mheight)
x = heights$Mheight
y = heights$Dheight

## f(x) = E(y)
fx = mean(y)

plot1 = plot0 + geom_hline(aes(yintercept = fx), color = "blue", size =1.2)
plot1

## interpolation
fx = y

plot2 = plot0 + geom_path(aes(x = x, y = fx), color = "blue")
plot2

## bin means
x.bin = as.numeric(cut(x, breaks = 5))
fx = sapply(x.bin, function(u) mean(y[which(x.bin == u)]))

plot3 = plot0 + geom_path(aes(x = x, y = fx), color = "blue", size = 1.2)
plot3

## smooth curve 1
fit.loess = loess(y ~ x, span = .15)
fx = predict(fit.loess, data.frame(x = x))

plot4 = plot0 + geom_path(aes(x = x, y = fx), color = "blue", size = 1.2)
plot4

## smooth curve 2
fit.loess = loess(y ~ x, span = .75)
fx = predict(fit.loess, data.frame(x = x))

plot5 = plot0 + geom_path(aes(x = x, y = fx), color = "blue", size = 1.2)
plot5

## line 1
fx = 40 + .4 * x

plot6 = plot0 + geom_path(aes(x = x, y = fx), color = "blue", size = 1.2)
plot6

## line 2
fit.lm = lm(y~x)
fx = predict(fit.lm, data.frame(x = x))

plot7 = plot0 + geom_path(aes(x = x, y = fx), color = "blue", size = 1.2)
plot7
## plot0 + stat_smooth(method = "lm")

####################################################################
## simple linear regression
####################################################################

linmod = lm(Dheight ~ Mheight, data = heights)
summary(linmod)
glance(linmod)
tidy(linmod)

####################################################################
####################################################################
####################################################################
####################################################################
####################################################################
####################################################################