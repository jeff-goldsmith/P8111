####################################################################
# Jeff Goldsmith
# Feb 25 2016
#
# R code used in P8111 Lecture 11
####################################################################

## clear work space
rm(list = ls())

## set working directory
setwd("~/Desktop/")

## load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)

####################################################################
## intuition for bootstrapping
####################################################################

n = 200
set.seed(5)

## generate a single sample
data.noncst = data.frame(x = rexp(n, rate = .1)) %>%
  mutate(y = 2 + 3*x + x * rnorm(n))

ggplot(data.noncst, aes(x=x, y=y)) + geom_point() + 
  geom_smooth(method = "lm", fill = "lightblue") + theme_bw()


## generate many samples from the true distribution
p <- list()
for(i in 1:16){
  data.noncst.cur = data.frame(x = rexp(n, rate = .1)) %>%
    mutate(y = 2 + 3*x + x * rnorm(n))
  p[[i]] <- ggplot(data.noncst.cur, aes(x=x, y=y)) + geom_point() + 
    geom_smooth(method = "lm", fill = "lightblue") + theme_bw()
}
do.call(grid.arrange, p)

## generate many samples from the true distribution
beta.hat = data.frame(
  b0 = rep(NA, 1000),
  b1 = rep(NA, 1000))

for(i in 1:1000){
  data.noncst.cur = data.frame(x = rexp(n, rate = .1)) %>%
    mutate(y = 2 + 3*x + x * rnorm(n))
  beta.hat[i,] = coef(lm(y~x, data = data.noncst.cur))
}

ggplot(data.noncst, aes(x=x, y=y)) + geom_point() + 
  geom_abline(data = beta.hat, aes(intercept = b0, slope = b1), alpha = .1) + 
  geom_smooth(method = "lm", fill = "lightblue") + theme_bw()


## implement the bootstrap on the original dataset
beta.hat = data.frame(
  b0 = rep(NA, 1000),
  b1 = rep(NA, 1000))

for(i in 1:1000){
  data.cur = sample_frac(data.noncst, size = 1, replace = TRUE)
  beta.hat[i,] = coef(lm(y~x, data.cur))
}  

ggplot(data.noncst, aes(x=x, y=y)) + geom_point() + 
  geom_abline(data = beta.hat, aes(intercept = b0, slope = b1), alpha = .1) + 
  geom_smooth(method = "lm", fill = "lightblue") + theme_bw()



####################################################################
## prestige example for the bootstrap
####################################################################

library(car)

## fit the model
linmod = lm(prestige ~ income, data = Prestige)

## plot data and regression fit
ggplot(Prestige, aes(x = income, y = prestige)) + geom_point() + 
  geom_smooth(method = "lm") + theme_bw()

## define a vector for the bootstrapped estimates
betaHatBS = data.frame(b1.est = rep(NA, 10000))

## use a loop to do the bootstrap
for(i in 1:10000){
  data.cur = sample_frac(Prestige, size = 1, replace = TRUE)
  betaHatBS$b1.est[i] = lm(prestige ~ income, data = data.cur)$coef[2]
}

betaHatBS$b1.null = rnorm(10000, linmod$coef[2], summary(linmod)$coef[2,2])

## make a plot of bootstrap and normal-based distributions
ggplot(betaHatBS, aes(x = b1.est)) + geom_density(fill = "blue", alpha = .3) + 
  geom_density(aes(x = b1.null), fill = "red", alpha = .3) + theme_bw()


####################################################################
## permutation testing
####################################################################

n = 20
set.seed(1)

## generate a single sample
data.noncst = data.frame(x = rexp(n, rate = .1)) %>%
  mutate(y = 2 + 1 * x + x * rnorm(n))

ggplot(data.noncst, aes(x=x, y=y)) + geom_point() + 
  geom_smooth(method = "lm", fill = "lightblue") + theme_bw()


## generate a few permuted datasets
p <- list()
for(i in 1:16){
  data.noncst.cur = mutate(data.noncst, x = sample(x, length(x), replace = FALSE))
  p[[i]] <- ggplot(data.noncst.cur, aes(x=x, y=y)) + geom_point() + 
    geom_smooth(method = "lm", fill = "lightblue") + theme_bw()
}
do.call(grid.arrange, p)


## do enough permutations to test
obs.coef = coef(lm(y ~ x, data = data.noncst))[2]

b1 = data.frame(b1.null = rep(NA, 10000))
for(i in 1:10000){
  data.noncst.cur = mutate(data.noncst, x = sample(x, length(x), replace = FALSE))
  b1$b1.null[i] = coef(lm(y ~ x, data = data.noncst.cur))[2]
}

ggplot(b1, aes(x = b1.null)) + geom_density(fill = "blue", alpha = .3) +
  geom_vline(xintercept = obs.coef, col = "red") + theme_bw()

2 * mean(b1>obs.coef)

####################################################################
## cross validation
####################################################################

## generate data
set.seed(1)

data.nonlin = data.frame(x = runif(100, 0, 1)) %>%
  mutate(y = -30*(x-.5)^2 + 100*(x-.5)^4 + rnorm(100, 0, .3))

## code for generating an overly-rich piecewise spline basis
PW.basis = sapply(seq(.1, .9, by = 0.05), function(u) (data.nonlin$x - u) * (data.nonlin$x >= u))
colnames(PW.basis) = paste0("spline_", substr(as.character(seq(.1, .9, by = 0.05)), 3, 5))
PW.basis = as.data.frame(PW.basis)

data.nonlin = bind_cols(data.nonlin, PW.basis)

MSEs = data.frame(
  model1 = rep(NA, 100),
  model2 = rep(NA, 100),
  model3 = rep(NA, 100),
  model4 = rep(NA, 100),
  model5 = rep(NA, 100)
)

for(i in 1:100){

  set.seed(i)
  data.nonlin = mutate(data.nonlin, 
                       cv_group = sample(1:100, 100, replace = FALSE) <= 80,
                       cv_group = factor(cv_group, levels = c(TRUE, FALSE), 
                                         labels = c("train", "test")))

  data.train = filter(data.nonlin, cv_group == "train")
  data.test = filter(data.nonlin, cv_group == "test")
  
  fit.1 = lm(y ~ x, data = data.train)
  MSEs[i,1] = mean((data.test$y - predict(fit.1, newdata = data.test))^2)

  fit.2 = lm(y ~ x + spline_5, data = data.train)
  MSEs[i,2] = mean((data.test$y - predict(fit.2, newdata = data.test))^2)
  
  fit.3 = lm(y ~ x + spline_2 + spline_5 + spline_8, data = data.train)
  MSEs[i,3] = mean((data.test$y - predict(fit.3, newdata = data.test))^2)
  
  fit.4 = lm(y ~ x + spline_15 + spline_5 + spline_9, data = data.train)
  MSEs[i,4] = mean((data.test$y - predict(fit.4, newdata = data.test))^2)
  
  fit.5 = lm(y ~ x + spline_1 + spline_15 + spline_2 + spline_25 + spline_3 + spline_35 + 
               spline_4 + spline_45 + spline_5 + spline_55 + spline_6 + spline_65 + 
               spline_7 + spline_75 + spline_8 + spline_85 + spline_9, 
               data = data.train)
  MSEs[i,5] = mean((data.test$y - predict(fit.5, newdata = data.test))^2)

}

slice(MSEs, 1) %>% gather(key = fit, value = MSE, model1:model5) %>%
  ggplot(aes(x = fit, y = MSE)) + geom_point() + theme_bw()

gather(MSEs, key = fit, value = MSE, model1:model5) %>%
  ggplot(aes(x = fit, y = MSE)) + geom_violin(alpha = .4, fill = "blue") +
  theme_bw() + ylim(0, 1.5)
  
apply(MSEs, 2, mean)

####################################################################
####################################################################
####################################################################
####################################################################
####################################################################
####################################################################