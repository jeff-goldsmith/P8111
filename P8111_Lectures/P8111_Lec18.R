####################################################################
# Jeff Goldsmith
# March 31 2015
#
# R code used in P8111 Lecture 18
####################################################################

## clear work space
rm(list = ls())

library(ggplot2)
library(dplyr)
library(mgcv)

## set working directory
setwd("~/Desktop/")

## load nepalese data
data = read.csv("http://jeffgoldsmith.com/P8111/P8111_HWs/nepalese.csv")
data = data[order(data$weight),]

## divide data into training and validation data
set.seed(1)
data$GROUP = sample(c(0,1), prob = c(.2, .8), size = dim(data)[1], replace = TRUE)
data.train = subset(data, GROUP == 1)
data.valid = subset(data, GROUP == 0)


## plot arm circ against weight
p1 = ggplot(data, aes(x = weight, y = armc, color = as.factor(GROUP))) + geom_point(alpha = .5) +
  scale_color_manual(values = c("red", "black"), guide = FALSE)
p1

####################################################################
## some regression analyses of armcirc vs weight
####################################################################

CV = rep(NA, 6)

## f(x) = E(y)
fx = mean(data.train$armc)

p1 + geom_hline(yintercept = fx, color = "blue", size = 1.5)
CV[1] = mean((data.valid$armc - fx)^2)


## line
fx = lm(armc~weight, data = data.train)
p1 + stat_smooth(data = data.train, method = lm, se = FALSE, color = "blue", size = 1.5)

CV[2] = mean((data.valid$armc - predict(fx, newdata = data.valid))^2)


## poly
fx = lm(armc~poly(weight, 4), data = data.train)
plot.fx = data.frame(weight = data.train$weight, fx = fitted(fx))
p1 + geom_line(data = plot.fx, aes(x = weight, y = fx, color = NULL), color = "blue", size = 1.5)

CV[3] = mean((data.valid$armc - predict(fx, newdata = data.valid))^2)


## spline
data.train = mutate(data.train, weight.sp = (weight>7)*(weight - 7))
data.valid = mutate(data.valid, weight.sp = (weight>7)*(weight - 7))

fx = lm(armc ~ weight + weight.sp, data = data.train)
plot.fx = data.frame(weight = data.train$weight, fx = fitted(fx))
p1 + geom_line(data = plot.fx, aes(x = weight, y = fx, color = NULL), color = "blue", size = 1.5)

CV[4] = mean((data.valid$armc - predict(fx, newdata = data.valid))^2)



## penalized spline
fx = gam(armc ~ s(weight), data = data.train)
plot.fx = data.frame(weight = data.train$weight, fx = fitted(fx))
p1 + geom_line(data = plot.fx, aes(x = weight, y = fx, color = NULL), color = "blue", size = 1.5)

CV[5] = mean((data.valid$armc - predict(fx, newdata = data.valid))^2)


## kernel smoother
plot.fx = with(data.train, as.data.frame(ksmooth(weight, armc, kernel = "normal", bandwidth = 2)))
p1 + geom_line(data = plot.fx, aes(x = x, y = y, color = NULL), color = "blue", size = 1.5)

fx.cv = with(data.train, ksmooth(weight, armc, kernel = "normal", bandwidth = 2, x.points = data.valid$weight)$y)
CV[6] = mean((data.valid$armc[order(data.valid$weight)] - fx.cv)^2)


## compare CV
CV = data.frame(x = 1:6, CV = CV)
ggplot(CV, aes(x = x, y = CV)) + geom_point() + geom_line() + labs(x = "") + 
  scale_x_continuous(breaks = 1:6, labels = c("Obs Mean", "SLR", "Poly", "Spline", "P-Spline", "Kernel"))


####################################################################
## extended case study of arm circumference
####################################################################

## penalized spline
fx = gam(armc ~ s(weight), data = data.train)
plot.fx = data.frame(weight = data.train$weight, fx = fitted(fx))
p1 + geom_line(data = plot.fx, aes(x = weight, y = fx, color = NULL), color = "blue", size = 1.5)
summary(fx)

## penalized spline -- different penalty and spline basis
fx = gam(armc ~ s(weight, k = 100), data = data.train, sp = (.0001))
plot.fx = data.frame(weight = data.train$weight, fx = fitted(fx))
p1 + geom_line(data = plot.fx, aes(x = weight, y = fx, color = NULL), color = "blue", size = 1.5)
summary(fx)


## separate boys and girls
ggplot(data.train, aes(x = weight, y = armc, color = as.factor(sex))) + geom_point(alpha = .5) +
  scale_color_manual(values = c("blue", "red"), guide = FALSE) +
  stat_smooth(method = "gam", formula = y ~ s(x), se = FALSE, size = 1.5)

fx = gam(armc ~ sex + sex * weight + s(weight), data = data.train) 
summary(fx)
summary(lm(armc ~ sex + sex * weight, data = data.train))

plot(fx)

## separate age groups
data.train = mutate(data.train, ageGroup = cut(age, breaks = c(0, 22, 32, 40, 50, 100), labels = 1:5))
data.valid = mutate(data.valid, ageGroup = cut(age, breaks = c(0, 22, 32, 40, 50, 100), labels = 1:5))

ggplot(data.train, aes(x = weight, y = armc, color = ageGroup)) + geom_point(alpha = .5) +
  stat_smooth(method = "gam", formula = y ~ s(x), se = FALSE, size = 1.5) + 
  scale_color_discrete(guide = FALSE)

## smooth effects of age and weight
fx = gam(armc ~ s(age) + s(weight), data = data.train) 
summary(fx)

CV[7,] = c(7, mean((data.valid$armc - predict(fx, newdata = data.valid))^2))

par(mfrow = c(1,2))
plot(fx)

## linear model for comparison
fx = lm(armc ~ age + weight, data = data.train)
summary(fx)

CV[8,] = c(8, mean((data.valid$armc - predict(fx, newdata = data.valid))^2))


## compare CV
ggplot(CV, aes(x = x, y = CV)) + geom_point() + geom_line() + labs(x = "") + 
  scale_x_continuous(breaks = 1:8, labels = c("Obs Mean", "SLR", "Poly", "Spline", "P-Spline", "Kernel", "Smooth", "MLR"))


####################################################################
####################################################################
####################################################################
####################################################################
####################################################################
####################################################################