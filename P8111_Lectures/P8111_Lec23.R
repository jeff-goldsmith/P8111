####################################################################
# Jeff Goldsmith
# April 19 2016
#
# R code used in P8111 Lecture 23
####################################################################

## clear work space
rm(list = ls())

library(lme4)

####################################################################
## two simulated data examples
####################################################################

I = 100
J = 5

Z.des = matrix(c(rep(1, J), rep(0, I*J)), nrow = I*J, ncol = I)
bi = rnorm(I, 0, 25)
eps = rnorm(I*J, 0, 1)
yij = Z.des%*% bi + eps 

plot(Z.des%*% bi + eps, xlab = "ij", ylab = expression(y_[ij]), pch = 18)

bi = rnorm(I, 0, 1)
yij = Z.des%*% bi + eps 
plot(Z.des%*% bi + eps, xlab = "ij", ylab = expression(y_[ij]), pch = 18)

subjs = Z.des %*% (1:I)

lmer(yij ~ (1 | subjs))


####################################################################
## three level data
####################################################################

set.seed(121)

I = 20
J = 10
K = 10

ZI.des = matrix(c(rep(1, J*K), rep(0, I*J*K)), nrow = I*J*K, ncol = I)
ZIJ.des = matrix(c(rep(1, K), rep(0, I*J*K)), nrow = I*J*K, ncol = I*J)
bi = rnorm(I, 0, 50)
bij = rnorm(I*J, 0, 25)
eps = rnorm(I*J, 0, 1)
yij = ZI.des %*% bi + ZIJ.des%*% bij + eps 

dev.new(width = 7, height = 3.5)
par(mai = c(.7,.7,.2,.2))
plot(yij, xlab = "ij", ylab = expression(y_[ij]), pch = 18)
abline(v = c(J*K * (0:I)), lty = 2)

L1 = ZI.des %*% (1:I)
L2 = ZIJ.des %*% (1:(I*J))

nested.mod = lmer(yij ~ (1 | L1) + (1 | L2))
summary(nested.mod)

####################################################################
## effect of bayes
####################################################################

set.seed(2008)

grid = seq(-5, 25, length = 1000)

n = 10

l.var = 25
l.mean = 20
y = rnorm(n, mean = l.mean, sd = sqrt(l.var))
like = dnorm(grid, mean = mean(y), sd = sqrt(l.var/n))

p.var = 50
p.mean = 2
prior = dnorm(grid, mean = p.mean, sd = sqrt(p.var))

post.mean = mean(y) * p.var / (p.var + l.var/n) + p.mean * (l.var/n) / (p.var + l.var/n)
post.var = (p.var * l.var / n) / (p.var + l.var/n)
post = dnorm(grid, mean = post.mean, sd = sqrt(post.var))
plot(grid, post, type = 'l', lwd = 3, xlab = expression(mu), ylab = expression(p(mu)))
points(grid, prior, type = 'l', col = 2, lwd = 3)
points(grid, like, type = 'l', col = 4, lwd = 3)


####################################################################
####################################################################
####################################################################
####################################################################
####################################################################
####################################################################