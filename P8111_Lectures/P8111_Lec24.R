####################################################################
# Jeff Goldsmith
# March 5, 2015
#
# R code used in P8111 Lecture 14
####################################################################

## clear work space
rm(list = ls())

## set working directory
setwd("~/Desktop/")

####################################################################
## simulate a simple linear regression with measurement error
####################################################################

## generate data
set.seed(14)

x = rnorm(30, 3, 3)
w = x + rnorm(30,0,3)
y = 2+.6*x +rnorm(30, 0, 1)

## fit regressions
linmod.x = lm(y~x)
linmod.w = lm(y~w)

## plot data
par(mai = c(.1, .1, .1, .1))
plot(x,y, axes = FALSE, col = "black", pch = 19, xlab = "", ylab = "", xlim = range(c(x, w)))
points(w,y, col = "red", pch = 19)
abline(h = 0, lwd = 2); abline(v = 0, lwd = 2)
abline(linmod.x, col = "blue", lwd = 2)
abline(linmod.w, col = "blue", lwd = 2, lty = 2)


####################################################################
## simulate a regression calibration model
####################################################################

## generate data
set.seed(14)

x = rnorm(30, 3, 3)
w = x + rnorm(30,0,3)
y = 2+.6*x +rnorm(30, 0, 1)
z = x + rnorm(30,0,2)
fitted = fitted(lm(w~z))

## fit regressions
linmod.x = lm(y~x)
linmod.w = lm(y~w)
linmod.fitted = lm(y~fitted)

## plot data
par(mai = c(.1, .1, .1, .1))
plot(x,y, axes = FALSE, col = "black", pch = 19, xlab = "", ylab = "", xlim = range(c(x, w)))
points(w,y, col = "red", pch = 19)
abline(h = 0, lwd = 2); abline(v = 0, lwd = 2)
abline(linmod.x, col = "blue", lwd = 2)
abline(linmod.w, col = "blue", lwd = 2, lty = 2)
abline(linmod.fitted, col = "blue", lwd = 2, lty = 2)


####################################################################
## perform SIMEX
####################################################################

## generate data
set.seed(14)

sig2u = 9

x = rnorm(30, 3, 3)
w = x + rnorm(30, 0, sqrt(sig2u))
y = 2+.6*x +rnorm(30, 0, 1)

lam = seq(.5, 5, by = .5)
beta.lam = matrix(NA, nrow = 1000, ncol = length(lam))

for(l in 1:length(lam)){
  for(i in 1:1000){
    wb = w + rnorm(30, 0, sqrt(lam[l] * sig2u))
    beta.lam[i,l] = coef(lm(y ~ wb))[2]
  }
}

lam = c(0, lam)
mean.beta = c(coef(lm(y ~ w))[2], apply(beta.lam,2,mean))

lam2 = lam^2
lam3 = lam^3

simex.coef = coef(lm(mean.beta ~ lam + lam2 + lam3))

new.lam = seq(-1, 5, by = .1)
simex.fit = cbind(1, new.lam, new.lam^2, new.lam^3) %*% simex.coef

quartz(width = 6, height = 4)
par(mai = c(.8, .8, .1, .1))

plot(lam, mean.beta, xlim = range(new.lam), ylim = range(0, simex.fit), pch = 19, col = c(2, rep(1, length(lam)-1 )),
  xlab = expression(lambda), ylab = expression(beta[1]))
points(new.lam, simex.fit, type = 'l', lty = 2)

####################################################################
####################################################################
####################################################################
####################################################################
####################################################################
####################################################################