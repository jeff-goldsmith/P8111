####################################################################
# Jeff Goldsmith
# April 12 2016
#
# R code used in P8111 Lecture 21
####################################################################

## clear work space
rm(list = ls())

library(SemiPar)
library(lme4)
library(gee)
library(ggplot2)

####################################################################
## pig weight data
####################################################################

data(pig.weights)

## plot of data

ggplot(pig.weights, aes(x = num.weeks, y = weight, group = id.num)) + 
  geom_point() + geom_path(alpha = .4) +
  labs(x = "Week", y = "Weight")

## OLS fit
ggplot(pig.weights, aes(x = num.weeks, y = weight, group = id.num)) + 
  geom_point() + geom_path(alpha = .4) +
  labs(x = "Week", y = "Weight") + 
  stat_smooth(method = "lm", aes(group = NULL), se = FALSE, color = "red", size = 1.1)

## marginal model fit
marg.mod = gee(weight ~ num.weeks, id = id.num,corstr = "exchangeable", data = pig.weights)
summary(marg.mod)

## random effects model fit
ranmod = lmer(weight ~ (1 | id.num) + num.weeks, data = pig.weights)

pig.weights$ranmod.fit = fitted(ranmod)
ggplot(pig.weights, aes(x = num.weeks, y = weight, group = id.num)) + 
  geom_point() + geom_path(alpha = .4) +
  labs(x = "Week", y = "Weight") + 
  stat_smooth(method = "lm", aes(group = NULL), se = FALSE, color = "red", size = 1.1) + 
  geom_line(aes(y = ranmod.fit), color = "blue", alpha = .5)

summary(ranmod)

ranef = data.frame(int = ranef(ranmod)$id.num[,1])
ggplot(ranef, aes(x = int)) + geom_histogram(binwidth = 2, color = "blue", alpha = .4)


####################################################################
####################################################################
####################################################################
####################################################################
####################################################################
####################################################################