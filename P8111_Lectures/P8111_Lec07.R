####################################################################
# Jeff Goldsmith
# Feb 11 2016
#
# R code used in P8111 Lecture 07
####################################################################

## clear work space
rm(list = ls())

## load libraries
library(dplyr)
library(tidyr)
library(broom)
library(ggplot2)
library(splines)

####################################################################
## nonlinear data
####################################################################

set.seed(1)

data.nonlin = data.frame(x = runif(100, 0, 1)) %>%
  mutate(y = -30*(x-.5)^2 + 100*(x-.5)^4 + rnorm(100, 0, .3))

## data plot
ggplot(data.nonlin, aes(y=y, x=x)) + geom_point() + theme_bw()

####################################################################
## fit as a polynomial
####################################################################

data.nonlin = mutate(data.nonlin, 
                     x.pow2 = x^2, x.pow3 = x^3, x.pow4 = x^4)

## fit data with quartic, plot results
quartfit = lm(y ~ x + x.pow2 + x.pow3 + x.pow4, data = data.nonlin)
tidy(quartfit)

mutate(data.nonlin, fitted = fitted(quartfit)) %>%
  ggplot(., aes(y=y, x=x)) + geom_point() + 
  geom_line(aes(y = fitted), color = "red") + theme_bw()

## plot polynomial basis functions
data.nonlin %>%
  select(-y) %>%
  mutate(pow.0 = 1, x.pow1 = x) %>%
  gather(key = power, value = value, -x) %>%
  mutate(power = factor(power)) %>%
  ggplot(., aes(x = x, y = value)) + geom_line() +
  facet_grid(. ~ power, scales = "free") +
  theme_bw()

## fit data with quadratic, plot results
quadfit = lm(y ~ x + x.pow2, data = data.nonlin)
mutate(data.nonlin, fitted = fitted(quadfit)) %>%
  ggplot(., aes(y=y, x=x)) + geom_point() + 
  geom_line(aes(y = fitted), color = "red") + theme_bw()

## fit data with degree 20 poly, plot results
twentyfit = lm(y ~ poly(x, 20), data = data.nonlin)
mutate(data.nonlin, fitted = fitted(twentyfit)) %>%
  ggplot(., aes(y=y, x=x)) + geom_point() + 
  geom_line(aes(y = fitted), color = "red") + theme_bw()


####################################################################
## fit as a piecewise linear
####################################################################

set.seed(1)

data.nonlin = data.frame(x = runif(100, 0, 1)) %>%
  mutate(y = -30*(x-.5)^2 + 100*(x-.5)^4 + rnorm(100, 0, .3))

data.nonlin = mutate(data.nonlin, 
                     spline_2 = (x - .2) * (x >= .2),
                     spline_5 = (x - .5) * (x >= .5),
                     spline_8 = (x - .8) * (x >= .8))

piecewise.fit = lm(y ~ x + spline_2 + spline_5 + spline_8, data = data.nonlin)
tidy(piecewise.fit)

mutate(data.nonlin, fitted = fitted(piecewise.fit)) %>%
  ggplot(., aes(y=y, x=x)) + geom_point() + 
  geom_line(aes(y = fitted), color = "red") + theme_bw()

## plot piecewise basis functions
data.nonlin %>%
  select(-y) %>%
  mutate(spline_int = 1, spline_0 = x) %>%
  gather(key = power, value = value, -x) %>%
  mutate(power = factor(power), power = relevel(power, ref = "spline_int")) %>%
  ggplot(., aes(x = x, y = value)) + geom_line() +
  facet_grid(. ~ power, scales = "free") +
  theme_bw()


####################################################################
## fit as a bspline
####################################################################

set.seed(1)

data.nonlin = data.frame(x = runif(100, 0, 1)) %>%
  mutate(y = -30*(x-.5)^2 + 100*(x-.5)^4 + rnorm(100, 0, .3))

data.nonlin = data.nonlin %>% bind_cols(., data.frame(ns(.[['x']], df = 5))) %>%
  rename(sp.1 = X1, sp.2 = X2, sp.3 = X3, sp.4 = X4, sp.5 = X5)

bspline.fit = lm(y ~ sp.1 + sp.2 + sp.3 + sp.4 + sp.5, data = data.nonlin)
tidy(bspline.fit)

mutate(data.nonlin, fitted = fitted(bspline.fit)) %>%
  ggplot(., aes(y=y, x=x)) + geom_point() + 
  geom_line(aes(y = fitted), color = "red") + theme_bw()

## plot bspline basis functions
data.nonlin %>%
  select(-y, -fitted) %>%
  mutate(sp.0 = 1) %>%
  gather(key = power, value = value, -x) %>%
  ggplot(., aes(x = x, y = value)) + geom_line() +
  facet_grid(. ~ power, scales = "free") +
  theme_bw()

####################################################################
####################################################################
####################################################################
####################################################################
####################################################################
####################################################################