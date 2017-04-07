####################################################################
# Jeff Goldsmith
# April 7 2016
#
# R code used in P8111 Lecture 20
####################################################################

## clear work space
rm(list = ls())

library(SemiPar)
library(lme4)
library(dplyr)
library(ggplot2)

####################################################################
## plot of CD4 data
####################################################################

data = read.table("CD4.txt", header = TRUE)

ggplot(data, aes(x = month, y = cd4)) + geom_point()

ggplot(data, aes(x = month, y = cd4, group = ID)) + geom_point() + geom_line(alpha = .4)

ggplot(data, aes(x = month, y = cd4, group = ID)) + geom_point() + geom_line(alpha = .4) +
  geom_line(data = subset(data, ID %in% unique(data$ID)[1:10]), color = "red", size = 1) 

####################################################################
## pig weight data
####################################################################

## plots

data(pig.weights)

ggplot(pig.weights, aes(x = num.weeks, y = weight, group = id.num)) + 
  geom_point() + geom_path(alpha = .4) +
  labs(x = "Week", y = "Weight")

ggplot(pig.weights, aes(x = num.weeks, y = weight, group = id.num)) + 
  geom_point() + geom_path(alpha = .4) +
  labs(x = "Week", y = "Weight") + 
  stat_smooth(method = "lm", aes(group = NULL), se = FALSE, color = "red", size = 1.1)
  
  
## random intercept model

ranmod = lmer(weight ~ (1 | id.num) + num.weeks, data = pig.weights)

pig.weights$ranmod.fit = fitted(ranmod)
ggplot(pig.weights, aes(x = num.weeks, y = weight, group = id.num)) + 
  geom_point() + geom_path(alpha = .4) +
  labs(x = "Week", y = "Weight") + 
  stat_smooth(method = "lm", aes(group = NULL), se = FALSE, color = "red", size = 1.1) + 
  geom_line(aes(y = ranmod.fit), color = "blue", alpha = .5)

summary(ranmod)


####################################################################
####################################################################
####################################################################
####################################################################
####################################################################
####################################################################