####################################################################
# Jeff Goldsmith
# April 26 2016
#
# R code used in P8111 Lecture 25
####################################################################

## clear work space
rm(list = ls())

library(boot)
library(dplyr)
library(ggplot2)
library(gridExtra)

## set working directory
setwd("~/Desktop/")

####################################################################
## simulate some binary data
####################################################################

set.seed(11201)

beta0 = 1
beta1 = .75

x = rnorm(100, 0, 3)
pi = inv.logit(beta0 + beta1 *x)
y = rbinom(100, 1, pi)

data = data.frame(x = x, y = y)

## linear model plot
ggplot(data, aes(x = x, y = y)) + geom_point() + 
  theme_bw() + ylim(-.25, 1.25) + 
  stat_smooth(method = "lm", se = FALSE)


####################################################################
## logistic regression
####################################################################

model = glm(y~x, family = binomial(link = "logit"), data = data)
summary(model)

data = mutate(data, fitted = fitted(model), 
              fitted_logit = logit(fitted))

p1 = ggplot(data, aes(x = x, y = y)) + geom_point() + 
  theme_bw() + ylim(-.25, 1.25) + 
  geom_line(aes(x = x, y = fitted), color = "blue")
p2 = ggplot(data, aes(x = x, y = fitted_logit)) + geom_line(color = "blue") + 
  theme_bw()

grid.arrange(p1, p2, nrow = 1, ncol = 2)




####################################################################
####################################################################
####################################################################
####################################################################
####################################################################
####################################################################