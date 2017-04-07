####################################################################
# Jeff Goldsmith
# Feb 18 2016
#
# R code used in P8111 Lecture 09
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
## load MLB data
####################################################################

## download data file
setwd("~/Desktop")
download.file("http://www.openintro.org/stat/data/mlb11.RData", destfile = "mlb11.RData")
load("mlb11.RData")

mlb11 %>% tbl_df

####################################################################
## fit a model
####################################################################

linmod = lm(runs ~ at_bats + hits + homeruns + stolen_bases, data = mlb11)
summary(linmod)

## compare to a null model with two predictors
linmod.null1 = lm(runs ~ hits + homeruns, data = mlb11)
anova(linmod.null1, linmod)

## compare to a null model with everything but stolen_bases
linmod.null2 = lm(runs ~ at_bats + hits + homeruns, data = mlb11)
anova(linmod.null2, linmod)

## compare to a null model with only the intercept
linmod.null3 = lm(runs ~ 1, data = mlb11)
anova(linmod.null3, linmod)


####################################################################
## simulations looking at LSEs
####################################################################

## fix simulation elements
nrep = 10000
beta0 = 0
beta1 = 1
n = c(10, 100, 1000)

## matrices to hold results
NormLSEs = NonNormLSEs = data.frame(n10 = rep(NA, nrep),
                                    n100 = rep(NA, nrep),
                                    n1000 = rep(NA, nrep))

## simulate data; compute and save LSEs
for(samp.size in 1:3){
  x = rnorm(n[samp.size], 0, 1) 
    for(i in 1:nrep){
    y = beta0 + beta1 * x + rnorm(n[samp.size], 0, 1)
    NormLSEs[i,samp.size] = lm(y~x)$coef[2]
  }
}

for(samp.size in 1:3){
  x = rnorm(n[samp.size], 0, 1)    
    for(i in 1:nrep){
    y = beta0 + beta1 * x + (10/3 * (rbinom(n[samp.size], 1, .1)) - 1/3)
    NonNormLSEs[i,samp.size] = lm(y~x)$coef[2]
  }
}

## arrange and plot results of simulation for normal errors
NormLSEs = gather(NormLSEs, key = SampSize, value = LSE) %>%
  mutate(Normalized = sqrt(rep(c(10,100,1000), each = nrep)) * (LSE - 1))

p1 = ggplot(NormLSEs, aes(LSE, fill = SampSize)) + 
  geom_density(alpha = .2) +
  labs(title = "LSEs") + 
  theme_bw()
p2 = ggplot(NormLSEs, aes(Normalized, fill = SampSize)) + 
  geom_density(alpha = .2) +
  labs(title = "Normalized LSEs") + 
  theme_bw()
grid.arrange(p1, p2, ncol = 2)


## arrange and plot results of simulation for non-normal errors
NonNormLSEs = gather(NonNormLSEs, key = SampSize, value = LSE) %>%
  mutate(Normalized = sqrt(rep(c(10,100,1000), each = nrep)) * (LSE - 1))

p1 = ggplot(NonNormLSEs, aes(LSE, fill = SampSize)) + 
  geom_density(alpha = .2) +
  labs(title = "LSEs") + 
  theme_bw()
p2 = ggplot(NonNormLSEs, aes(Normalized, fill = SampSize)) + 
  geom_density(alpha = .2) +
  labs(title = "Normalized LSEs") + 
  theme_bw()
grid.arrange(p1, p2, ncol = 2)



####################################################################
## plot of FWER
####################################################################

alpha = .05
k = 1:100
fwer = 1-(1-alpha)^k
plot.dat = as.data.frame(cbind(k ,fwer))

ggplot(plot.dat, aes(x = k, y = fwer)) + geom_path() + geom_hline(yintercept = 1)


####################################################################
## nonlinear example
####################################################################

set.seed(1)

data.nonlin = data.frame(x = runif(100, 0, 1)) %>%
  mutate(y = -30*(x-.5)^2 + 100*(x-.5)^4 + rnorm(100, 0, .3))

## data plot
ggplot(data.nonlin, aes(y=y, x=x)) + geom_point() + theme_bw()

PW.basis = sapply(seq(.1, .9, by = 0.05), function(u) (data.nonlin$x - u) * (data.nonlin$x >= u))
colnames(PW.basis) = paste0("spline_", substr(as.character(seq(.1, .9, by = 0.05)), 3, 5))
PW.basis = as.data.frame(PW.basis)
data.nonlin = bind_cols(data.nonlin, PW.basis)

## fit three models
piecewise.underfit = lm(y ~ x, data = data.nonlin)
tidy(piecewise.underfit)

piecewise.fit = lm(y ~ x + spline_15 + spline_5 + spline_9, data = data.nonlin)
tidy(piecewise.fit)

piecewise.overfit = lm(y ~ x + spline_1 + spline_15 + spline_2 + spline_25 + spline_3 + spline_35 + 
                         spline_4 + spline_45 + spline_5 + spline_55 + spline_6 + spline_65 + 
                         spline_7 + spline_75 + spline_8 + spline_85 + spline_9, 
                       data = data.nonlin)
tidy(piecewise.overfit)

## add fitted values to data
data.nonlin = mutate(data.nonlin, 
                     underfit = fitted(piecewise.underfit),
                     fit = fitted(piecewise.fit),
                     overfit = fitted(piecewise.overfit))


## f test comparing to null linear fit to alternative quartic fit
anova(piecewise.underfit, piecewise.fit)
ggplot(data.nonlin, aes(y=y, x=x)) + geom_point() + 
  geom_line(aes(y = underfit), color = "red") + 
  geom_line(aes(y = fit), color = "red") + 
  theme_bw()


## f test comparing to null quartic fit to alternative twenty fit
anova(piecewise.fit, piecewise.overfit)
ggplot(data.nonlin, aes(y=y, x=x)) + geom_point() + 
  geom_line(aes(y = fit), color = "red") + 
  geom_line(aes(y = overfit), color = "red") + 
  theme_bw()

## anova table for all three
anova(piecewise.underfit, piecewise.fit, piecewise.overfit)


####################################################################
####################################################################
####################################################################
####################################################################
####################################################################
####################################################################