####################################################################
# Jeff Goldsmith
# Jan 28 2016
#
# R code used in P8111 Lecture 04
####################################################################

## clear work space
rm(list = ls())

## set working directory
setwd("~/Desktop/")

## load libraries
library(broom)
library(dplyr)
library(ggplot2)

####################################################################
## plot for interpretation
####################################################################

set.seed(1)

data = data.frame(X = rnorm(30, 3, 3)) %>% mutate(Y = 2+.6*X +rnorm(30, 0, 1))
ggplot(data, aes(x = X, y = Y)) + geom_smooth(method = 'lm', se = FALSE, size = 2) + 
  geom_point(alpha = .1) + theme_bw() + 
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0)

####################################################################
## a few possible scatterplots
####################################################################

## plot 1
plot.data = data.frame(x = rnorm(100),
                       y = rnorm(100))

ggplot(plot.data, aes(x = x, y = y)) + geom_point()


## plot 2

plot.data = data.frame(x = rnorm(100)) %>% mutate(y = -2-2*x + rnorm(100, 0, .5))
ggplot(plot.data, aes(x = x, y = y)) + geom_point()

## plot 3
plot.data = data.frame(x = rnorm(100)) %>% mutate(y = -2+2*(x+1)^2 + rnorm(100, 0, 2))
ggplot(plot.data, aes(x = x, y = y)) + geom_point()

## plot 4
plot.data = mutate(plot.data, x = replace(x, 1, 4), y = replace(y, 1, 22), y = replace(y, order(x)[50], 15))
ggplot(plot.data, aes(x = x, y = y)) + geom_point()


####################################################################
## plot for interpretation -- this code uses "old fashioned" plotting
####################################################################

set.seed(1)

data = data.frame(X = rnorm(30, 3, 3)) %>% mutate(Y = 2+.6*X +rnorm(30, 0, 1))
data$fitted = lm(Y~X, data = data) %>% fitted

ggplot(data, aes(x = X, y = Y)) + geom_smooth(method = 'lm', se = FALSE, size = 1) + 
  geom_point(color = "red", size = 2) + geom_point(aes(y = fitted), size = 2) + theme_bw() + 
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0)


####################################################################
## recreate linear model output using formulas
####################################################################

set.seed(1)

data = data.frame(x = rnorm(30, 3, 3)) %>% mutate(y = 2+.6*x +rnorm(30, 0, 1))
linmod = lm(y~x, data = data)
summary(linmod)
tidy(linmod)
glance(linmod)

(beta1 = sum((x - mean(x))*(y - mean(y))) / sum((x - mean(x))^2))
(beta0 = mean(y) - beta1*mean(x))


####################################################################
####################################################################
####################################################################
####################################################################
####################################################################
####################################################################