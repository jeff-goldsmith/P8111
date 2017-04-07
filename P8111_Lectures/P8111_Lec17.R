####################################################################
# Jeff Goldsmith
# March 29 2016
#
# R code used in P8111 Lecture 17
####################################################################

## clear work space
rm(list = ls())

## load libraries
library(dplyr)
library(reshape)
library(ggplot2)

####################################################################
## load lidar data
####################################################################

library(MASS)
library(SemiPar)
data(lidar)

x = lidar$range

p1 = ggplot(lidar, aes(x = range, y = logratio)) + geom_point()
p1

####################################################################
## construct spline basis and fit piecewise linear model
####################################################################

range = lidar$range
y = lidar$logratio

knots <- c(550, 625)
X.des = cbind(1, range, sapply(knots, function(k) 
                ((range - k > 0) * (range - k))))

lm.lin = lm(y ~ X.des - 1)
summary(lm.lin)

lidar.pred = data.frame(range = range, fitted = fitted(lm.lin))
p1 + geom_path(data = lidar.pred, aes(x = range, y = fitted), color = "red")

####################################################################
## again, with different knots
####################################################################

knots <- c(500, 650)
X.des = cbind(1, range, sapply(knots, function(k) 
                ((range - k > 0) * (range - k))))

lm.lin = lm(y ~ X.des - 1)
summary(lm.lin)

lidar.pred = data.frame(range = range, fitted = fitted(lm.lin))
p1 + geom_path(data = lidar.pred, aes(x = range, y = fitted), color = "red")


####################################################################
## construct spline basis and fit piecewise quadratic model
####################################################################

knots <- c(500, 600, 675)
X.des = cbind(1, range, range^2, sapply(knots, function(k) 
                ((range - k > 0) * (range - k)^2)))

lm.lin = lm(y ~ X.des - 1)
summary(lm.lin)

lidar.pred = data.frame(range = range, fitted = fitted(lm.lin))
p1 + geom_path(data = lidar.pred, aes(x = range, y = fitted), color = "red")


####################################################################
## b-spline basis
####################################################################

library(splines)

bspline = bs(x = range, df = 6, degree = 3, intercept = TRUE)
rownames(bspline) = range
colnames(bspline) = paste0("Basis ", 1:6)

bspline.m = melt(bspline)

ggplot(bspline.m, aes(x = X1, y = value, group = X2, color = X2)) + geom_path()




####################################################################
## define CV groups, construct design matrices for training and validation data
####################################################################

set.seed(1001)

GROUP = lidar$GROUP = sample(c(0,1), size = length(y), prob = c(.8, .2), replace = TRUE)

knots <- quantile(range, seq(0, 1, length = 45))[-c(1, 45)]
X.full = cbind(1, range, sapply(knots, function(k) ((range - k > 0) * (range - k))))

X.train = X.full[which(GROUP != 1), ]; y.train = y[which(GROUP != 1)]
X.valid = X.full[which(GROUP == 1), ]; y.valid = y[which(GROUP == 1)]


####################################################################
## construct penalty matrix; estimate ridge regression for a few lambdas
####################################################################

P = diag(1, dim(X.full)[2], dim(X.full)[2])
P[1,1] = 0

lambda = 10^(-2:7)

beta.lm = solve(t(X.train) %*% X.train)%*% t(X.train) %*% y.train
beta.lam1 = solve(t(X.train) %*% X.train + lambda[4]* P)%*% t(X.train) %*% y.train
beta.lam2 = solve(t(X.train) %*% X.train + lambda[10]* P)%*% t(X.train) %*% y.train
beta.lam3 = solve(t(X.train) %*% X.train + lambda[6]* P)%*% t(X.train) %*% y.train


####################################################################
## plot results
####################################################################

p1 = ggplot(lidar, aes(x = range, y = logratio, color = as.factor(GROUP))) + geom_point() +
  scale_color_manual(values = c("black", "red"), guide = "none")
p1

lidar.pred = data.frame(range = range[which(GROUP == 0)], fitted = X.train %*% beta.lm)
p1 + geom_path(data = lidar.pred, aes(x = range, y = fitted), color = "blue")

lidar.pred$fitted = X.train %*% beta.lam1
p1 + geom_path(data = lidar.pred, aes(x = range, y = fitted), color = "blue")

lidar.pred$fitted = X.train %*% beta.lam2
p1 + geom_path(data = lidar.pred, aes(x = range, y = fitted), color = "blue")

lidar.pred$fitted = X.train %*% beta.lam3
p1 + geom_path(data = lidar.pred, aes(x = range, y = fitted), color = "blue")



####################################################################
## find cross-validation curve for one fold only
####################################################################

grid.lam = seq(-2, 7, by = .2)
lambda = 10^(grid.lam)

CV = rep(NA, length(lambda))

for(l in 1:length(lambda)){
  
  beta.cur = solve(t(X.train) %*% X.train + lambda[l]* P)%*% t(X.train) %*% y.train
  CV[l] = mean((y.valid - X.valid %*% beta.cur)^2 )
  
}

## plot CV curve for ridge regression
plot.dat = data.frame(grid = grid.lam, CV = CV)

ggplot(plot.dat, aes(x = grid, y = CV)) + geom_path() + 
  labs(x = expression(log[10](lambda)), ylab = "CV MSE")






####################################################################
## kernel smoothing
####################################################################
p1 = ggplot(lidar, aes(x = range, y = logratio)) + geom_point()

kern.smooth1 = as.data.frame(ksmooth(range, y, kernel = "normal", bandwidth = 50))
p1 + geom_path(data = kern.smooth1, aes(x = x, y = y), color = "blue")

kern.smooth2 = as.data.frame(ksmooth(range, y, kernel = "normal", bandwidth = 10))
p1 + geom_path(data = kern.smooth2, aes(x = x, y = y), color = "blue")

kern.smooth2 = as.data.frame(ksmooth(range, y, kernel = "normal", bandwidth = 500))
p1 + geom_path(data = kern.smooth2, aes(x = x, y = y), color = "blue")


####################################################################
####################################################################
####################################################################
####################################################################
####################################################################
####################################################################