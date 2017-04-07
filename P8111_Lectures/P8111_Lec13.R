####################################################################
# Jeff Goldsmith
# March 3 2016
#
# R code used in P8111 Lecture 13
####################################################################

## clear work space
rm(list = ls())

## load libraries
library(dplyr)
library(ggplot2)
library(gridExtra)


####################################################################
## fit three models with differing error structures; create
## residual and qqplots for each
####################################################################

set.seed(1)

## generate x's and y's
data.example = data.frame(x = runif(200, 0, 10)) %>%
  mutate(y1 = 1+2*x + rnorm(200),
         y2 = 1+2*x + rnorm(200) * x,
         y3 = 1+(x-4)^2 + rnorm(200, mean = 0, sd = 5)) %>%
  arrange(x)

## fit models
fit1 = lm(y1~x, data = data.example)
fit2 = lm(y2~x, data = data.example)
fit3 = lm(y3~x, data = data.example)

## save fitted values and residuals
data.example = data.example %>%
  mutate(fitted1 = fitted(fit1), resid1 = resid(fit1),
         fitted2 = fitted(fit2), resid2 = resid(fit2),
         fitted3 = fitted(fit3), resid3 = resid(fit3))

## plot data
p = list()

p[[1]] = ggplot(data.example, aes(x = x, y = y1)) + geom_point() + theme_bw()
p[[2]] = ggplot(data.example, aes(x = x, y = y2)) + geom_point() + theme_bw()
p[[3]] = ggplot(data.example, aes(x = x, y = y3)) + geom_point() + theme_bw()

grid.arrange(p[[1]], p[[2]], p[[3]], nrow = 1, ncol = 3)

## plot residuals against fitted values
p = list()

p[[1]] = ggplot(data.example, aes(x = fitted1, y = resid1)) + geom_point() + theme_bw()
p[[2]] = ggplot(data.example, aes(x = fitted2, y = resid2)) + geom_point() + theme_bw()
p[[3]] = ggplot(data.example, aes(x = fitted3, y = resid3)) + geom_point() + theme_bw()

grid.arrange(p[[1]], p[[2]], p[[3]], nrow = 1, ncol = 3)

## qqplot
p = list()

p[[1]] = ggplot(data.example, aes(sample = resid1)) + stat_qq() + theme_bw()
p[[2]] = ggplot(data.example, aes(sample = resid2)) + stat_qq() + theme_bw()
p[[3]] = ggplot(data.example, aes(sample = resid3)) + stat_qq() + theme_bw()

grid.arrange(p[[1]], p[[2]], p[[3]], nrow = 1, ncol = 3)



####################################################################
## leverage plots 1
####################################################################

set.seed(1)

data.example = data.frame(x = runif(100, 0, 10)) %>%
  mutate(y = 1+2*x + rnorm(100)) %>%
  mutate(x = replace(x, 1, 12.5), 
         y = replace(y, 1, 10))

fit = lm(y~x, data = data.example)

ggplot(data.example, aes(x=x, y=y)) + geom_point() + 
  stat_smooth(method = "lm", se = FALSE) + theme_bw()

## plot hat values
data.example = mutate(data.example,
                      hatvals = hatvalues(fit),
                      index = seq_along(hatvals))

ggplot(data.example, aes(x=index, y=hatvals)) + geom_point() + 
  ylim(0,0.09) + geom_hline(yintercept = 2*2/101) + 
  theme_bw()


####################################################################
## leverage plots 2
####################################################################

set.seed(1)

data.example = data.frame(x = runif(100, 0, 10)) %>%
  mutate(y = 1+2*x + rnorm(100)) %>%
  mutate(x = replace(x, 1, 5.5), 
         y = replace(y, 1, 3))

fit = lm(y~x, data = data.example)

ggplot(data.example, aes(x=x, y=y)) + geom_point() + 
  stat_smooth(method = "lm", se = FALSE) + theme_bw()

## plot hat values
data.example = mutate(data.example,
                      hatvals = hatvalues(fit),
                      index = seq_along(hatvals))

ggplot(data.example, aes(x=index, y=hatvals)) + geom_point() + 
  ylim(0,0.09) + geom_hline(yintercept = 2*2/101) + 
  theme_bw()


####################################################################
## leverage plots 3
####################################################################

set.seed(1)

data.example = data.frame(x = rnorm(100, 1, .5)) %>%
  mutate(y = 3 + rnorm(100)) %>%
  mutate(x = replace(x, 1, 10), 
         y = replace(y, 1, 20))

fit = lm(y~x, data = data.example)

ggplot(data.example, aes(x=x, y=y)) + geom_point() + 
  stat_smooth(method = "lm", se = FALSE) + theme_bw()

## plot hat values
data.example = mutate(data.example,
                      hatvals = hatvalues(fit),
                      index = seq_along(hatvals))

ggplot(data.example, aes(x=index, y=hatvals)) + geom_point() + 
  geom_hline(yintercept = 2*2/101) + 
  theme_bw()


####################################################################
## cook's distance
####################################################################

## plot hat values
data.example = mutate(data.example,
                      cooks = cooks.distance(fit),
                      index = seq_along(cooks))

ggplot(data.example, aes(x=index, y=cooks)) + geom_point() + 
  geom_hline(yintercept = 2*2/101) + 
  theme_bw()


####################################################################
####################################################################
####################################################################
####################################################################
####################################################################
####################################################################