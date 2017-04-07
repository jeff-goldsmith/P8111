####################################################################
# Jeff Goldsmith
# Feb 4 2016
#
# R code used in P8111 Lecture 05
####################################################################

## clear work space
rm(list = ls())

## set working directory
setwd("~/Desktop/")

## load libraries
library(dplyr)
library(ggplot2)

####################################################################
## plot for interpretation
####################################################################

## setting seed ensures reproducibility of random data
set.seed(1)

## get data, estimate linear model
data = data.frame(x = rnorm(30, 3, 3)) %>% 
  mutate(y = 2+.6*x + rnorm(30, 0, 1))
linmod = lm(y ~ x, data = data)

## make a plot
ggplot(data, aes(x = x, y = y)) + geom_point(color = "red", size = 3) +
  geom_hline(aes(yintercept = mean(y)), color = "blue", linetype = 2, size = 1) +
  geom_smooth(method = "lm", se = FALSE, color = "blue", size = 1)

####################################################################
## what's "in" linmod
####################################################################

## fit the model
linmod = lm(y ~ x, data = data)

## the object and the summary
linmod
summary(linmod)

## names() tells you what's in an object
names(linmod)

linmod$residuals
linmod$fitted.values

## names() on the summary of linmod
names(summary(linmod))
summary(linmod)$coef
summary(linmod)$r.squared

## ANOVA table for linmod
anova(linmod)
1 - 18.30 / (84.02 + 18.30)

####################################################################
## influence of outliers on slope
####################################################################

## setting seed ensures reproducibility of random data
set.seed(1)

## get data, estimate linear model
data = data.frame(x = rnorm(30, 3, 3)) %>% 
  mutate(y = 2+.6*x + rnorm(30, 0, 1),
         y = replace(y, 4, -2))

## make a plot
ggplot(data, aes(x = x, y = y)) + geom_point(color = "red", size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "blue", size = 1, linetype = 2) +
  geom_abline(intercept = coef(linmod)[1], slope = coef(linmod)[2], color = "blue", size = 1)



## setting seed ensures reproducibility of random data
set.seed(1)

## get data, estimate linear model
data = data.frame(x = rnorm(30, 3, 3)) %>% 
  mutate(y = 2+.6*x + rnorm(30, 0, 1),
         y = replace(y, 23, -2))

## make a plot
ggplot(data, aes(x = x, y = y)) + geom_point(color = "red", size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "blue", size = 1, linetype = 2) +
  geom_abline(intercept = coef(linmod)[1], slope = coef(linmod)[2], color = "blue", size = 1)



####################################################################
####################################################################
####################################################################
####################################################################
####################################################################
####################################################################