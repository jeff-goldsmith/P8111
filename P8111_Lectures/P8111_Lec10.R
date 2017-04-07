####################################################################
# Jeff Goldsmith
# Feb 23 2016
#
# R code used in P8111 Lecture 10
####################################################################

## clear work space
rm(list = ls())

## set working directory
setwd("~/Desktop/")

## load libraries
library(dplyr)
library(reshape)
library(ggplot2)
library(gridExtra)

####################################################################
## mlb data example of LRTs
####################################################################

download.file("http://www.openintro.org/stat/data/mlb11.RData", destfile = "mlb11.RData")
load("mlb11.RData")
head(mlb11)

## fit the full model
linmod = lm(runs ~ at_bats + hits + homeruns + stolen_bases, data = mlb11)

## compare to a null model with two predictors
linmod.null1 = lm(runs ~ hits + homeruns, data = mlb11)
anova(linmod.null1, linmod)

## LRT
Delta = -2*(logLik(linmod.null1) - logLik(linmod))
1-pchisq(Delta, 2)


####################################################################
## illustration of confidence ellipses
####################################################################

library(ellipse)

CI.ellipse = as.data.frame(ellipse(linmod,c(2,3)))
est = as.data.frame(t(as.matrix(coef(linmod)[2:3])))

## plot the joint confidence region
ggplot(CI.ellipse, aes(x = at_bats, y = hits)) + geom_path() +
  geom_hline(yintercept = confint(linmod)[3,], linetype = 2) + 
  geom_vline(xintercept = confint(linmod)[2,], linetype = 2) +
  geom_point(data = est, size = 4)


####################################################################
## polynomial example of pointwise intervals
####################################################################

## generate data
set.seed(1)

x = runif(100, 0, 1)
x = sort(x)
y = -30*(x-.5)^2 + 100*(x-.5)^4 + rnorm(100, 0, .3)
plot.dat = as.data.frame(cbind(y,x))

## fit data with quartic
quartfit = lm(y~poly(x,4))
plot.dat$quart_fit = quartfit$fitted.values

## plot of fit
plot = ggplot(plot.dat, aes(y=y, x=x)) + geom_point(size = 2) + 
  geom_line(aes(y=quart_fit, x = x), color = "red", size = 1) 
plot

## use quartfit to find variance matrix of estimated coefs
VarMat = (summary(quartfit)$sigma)^2 * summary(quartfit)$cov.unscaled

x = sort(x)
x.poly = cbind(1, poly(x, 4))

## ci's at a few new x's
fitted = x.poly[50,] %*% quartfit$coef
se = sqrt(x.poly[50,] %*% VarMat %*% x.poly[50,])
ybound = fitted +c(-2, 2) * se
plot + geom_segment(aes(x = x[50], y = ybound[1], xend = x[50], yend = ybound[2]), color = "blue", size = 1.1)

## ci's at all new x's
fitted.overall = x.poly %*% quartfit$coef
se.fitted = sqrt(diag(x.poly %*% VarMat %*% t(x.poly)))

plot.dat$UB = fitted.overall + 2 * se.fitted
plot.dat$LB = fitted.overall - 2 * se.fitted

plot + geom_segment(aes(x = x[50], y = ybound[1], xend = x[50], yend = ybound[2]), color = "blue", size = 1.1) +
  geom_line(data = plot.dat, aes(y=UB, x = x), color = "blue", linetype = 2) +
  geom_line(data = plot.dat, aes(y=LB, x = x), color = "blue", linetype = 2) 


####################################################################
## mother/daughter height to show difference in CI and PI
####################################################################

library(alr3)
data(heights)

x = heights$Mheight
y = heights$Dheight

linmod = lm(y~x)

## get pointwise CIs
x.design = cbind(1, x)
VarMat = summary(linmod)$sigma * summary(linmod)$cov.unscaled

fitted = x.design %*% linmod$coef
se.fitted = sqrt(diag(x.design %*% VarMat %*% t(x.design)))

UB = fitted + 2 * se.fitted
LB = fitted - 2 * se.fitted
plot.dat = as.data.frame(cbind(x, y, fitted, UB, LB))

plot = ggplot(plot.dat, aes(x = x, y = y)) + geom_point(color = "red") +
  geom_line(aes(x = x, y = fitted), color = "blue") + 
  geom_line(aes(x = x, y = UB), color = "blue", linetype = 2) + 
  geom_line(aes(x = x, y = LB), color = "blue", linetype = 2)


## plot prediction interval for one subject
se.pred = sqrt(summary(linmod)$sigma^2 * (1 + x.design[200,] %*% summary(linmod)$cov.unscaled %*% x.design[200,]))
ybound = fitted[200] +c(-2, 2) * se.pred

plot + geom_segment(aes(x = x[200], y = ybound[1], xend = x[200], yend = ybound[2]), color = "blue", size = 1.1)


####################################################################
####################################################################
####################################################################
####################################################################
####################################################################
####################################################################