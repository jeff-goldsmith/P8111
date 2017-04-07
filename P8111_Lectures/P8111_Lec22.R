####################################################################
# Jeff Goldsmith
# April 14 2016
#
# R code used in P8111 Lecture 22
####################################################################

## clear work space
rm(list = ls())

library(SemiPar)
library(lme4)
library(ggplot2)
library(readr)
library(dplyr)
library(splines)

####################################################################
## pig weight data example of fixed vs random effects
####################################################################

data(pig.weights)

X.des = model.matrix( ~ -1 + as.factor(id.num) + num.weeks, data = pig.weights)
lm.fixed = lm(weight ~ -1 + X.des, data = pig.weights)
fixed = coef(lm.fixed)[1:48] - mean(coef(lm.fixed)[1:48])

ranef.mod = lmer(weight ~ (1 | id.num) + num.weeks, data = pig.weights)
random = ranef(ranef.mod)$id.num[,1]

plot.dat = data.frame(value = c(fixed, random), yval = rep(c(1,0), each = 48), pair = rep(1:48, 2))

ggplot(plot.dat, aes(x = value, y = yval, group = pair)) + geom_line() + geom_point() + 
  geom_vline(xintercept = 0, color = "red", size = 1.2) + theme_bw() + 
  scale_y_continuous(breaks = c(0,1), labels = c("random", "fixed")) + labs(y = "", x = "")


####################################################################
## pig weight data
####################################################################

data(pig.weights)

## plot of data

## random effects model fit
ranmod = lmer(weight ~ (1 | id.num) + num.weeks, data = pig.weights)

pig.weights$ranmod.fit = fitted(ranmod)
ggplot(pig.weights, aes(x = num.weeks, y = weight, group = id.num)) + 
  geom_point() + geom_path(alpha = .4) +
  labs(x = "Week", y = "Weight") + 
  stat_smooth(method = "lm", aes(group = NULL), se = FALSE, color = "red", size = 1.1) + 
  geom_line(aes(y = ranmod.fit), color = "blue", alpha = .5)

summary(ranmod)

ranef = data.frame(int = ranef(ranmod)$id.num[,1])
ggplot(ranef, aes(x = int)) + geom_histogram(binwidth = 2, color = "blue", alpha = .4)

## random slope model fit
ranmod = lmer(weight ~ (1 + num.weeks | id.num) + num.weeks, data = pig.weights)
pig.weights$ranmod.fit = fitted(ranmod)
ggplot(pig.weights, aes(x = num.weeks, y = weight, group = id.num)) + 
  geom_point() + geom_path(alpha = .4) +
  labs(x = "Week", y = "Weight") + 
  stat_smooth(method = "lm", aes(group = NULL), se = FALSE, color = "red", size = 1.1) + 
  geom_line(aes(y = ranmod.fit), color = "blue", alpha = .5)

ranef = data.frame(int = ranef(ranmod)$id.num[,1], slope = ranef(ranmod)$id.num[,2])
ggplot(ranef, aes(x = int, y = slope)) + geom_point()


####################################################################
## CD4 data example
####################################################################

## load and plot data
cd4 = read_csv(url("http://jeffgoldsmith.com/P8111/P8111_HWs/P8111_CD4.csv"))
IDS = unique(data$ID)[which(table(data$ID) != 1)]
cd4 = cd4 %>% filter(ID %in% IDS)

ggplot(cd4, aes(x = month, y = cd4, group = ID)) + geom_point() + geom_line(alpha = .4)
  
## OLS models
## linear first
lin.mod = lm(cd4 ~ month, data = cd4)

ggplot(data, aes(x = month, y = cd4, group = ID)) + geom_point() + geom_line(alpha = .4) + 
  stat_smooth(method = "lm", aes(group = NULL), se = FALSE, color = "red", size = 1.1) 
summary(lin.mod)
AIC(lin.mod)
  
## spline

bs.mod = lm(cd4 ~ bs(month, 5), data = cd4)

ggplot(cd4, aes(x = month, y = cd4, group = ID)) + geom_point() + geom_line(alpha = .4) + 
  stat_smooth(method = "lm", formula = y ~ bs(x, 5), aes(group = NULL), se = FALSE, color = "red", size = 1.1) 
summary(bs.mod)
AIC(bs.mod)
  
## random intercept
ranint.mod = lmer(cd4 ~ (1 | ID) + month, data = cd4)
cd4$ranint.mod = fitted(ranint.mod)
fitted = data.frame(month = cd4$month, fitted = getME(ranint.mod,'X') %*% fixef(ranint.mod))

ggplot(cd4, aes(x = month, y = cd4, group = ID)) + geom_point() + geom_line(alpha = .2) + 
  geom_line(data = fitted, aes(group = NULL, y = fitted), color = "red", size = 1.1) + 
  geom_line(aes(y = ranint.mod), color = "blue", alpha = .5)
summary(ranint.mod)
AIC(ranint.mod)
  

## random slope
ranslope.mod = lmer(cd4 ~ (1 + month | ID) + month, data = cd4)
cd4$ranslope.mod = fitted(ranslope.mod)
fitted = data.frame(month = cd4$month, fitted = getME(ranslope.mod,'X') %*% fixef(ranslope.mod))

ggplot(cd4, aes(x = month, y = cd4, group = ID)) + geom_point() + geom_line(alpha = .2) + 
  geom_line(data = fitted, aes(group = NULL, y = fitted), color = "red", size = 1.1) + 
  geom_line(aes(y = ranslope.mod), color = "blue", alpha = .5)
summary(ranslope.mod)
AIC(ranslope.mod)

## random intercept, slope + bs
ranbs.mod = lmer(cd4 ~ (1 + month | ID) + bs(month, 5), data = cd4)
cd4$ranbs.mod = fitted(ranbs.mod)
fitted = data.frame(month = cd4$month, fitted = getME(ranbs.mod,'X') %*% fixef(ranbs.mod))

ggplot(cd4, aes(x = month, y = cd4, group = ID)) + geom_point() + geom_line(alpha = .2) + 
  geom_line(data = fitted, aes(group = NULL, y = fitted), color = "red", size = 1.1) + 
  geom_line(aes(y = ranbs.mod), color = "blue", alpha = .5)
summary(ranbs.mod)

summary(ranbs.mod)
AIC(ranbs.mod)
  
  
  
####################################################################
####################################################################
####################################################################
####################################################################
####################################################################
####################################################################