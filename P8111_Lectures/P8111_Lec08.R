####################################################################
# Jeff Goldsmith
# Feb 16 2016
#
# R code used in P8111 Lecture 08
####################################################################

## clear work space
rm(list = ls())

## load libraries
library(dplyr)
library(ggplot2)
library(gridExtra)
library(broom)
library(alr3)

## load data
data(heights)


####################################################################
## load MLB data
####################################################################

## download data file
setwd("~/Desktop")
download.file("http://www.openintro.org/stat/data/mlb11.RData", destfile = "mlb11.RData")
load("mlb11.RData")

mlb11 %>% tbl_df

####################################################################
## make some plots
####################################################################

p1 = ggplot(mlb11, aes(x = runs, y = hits)) + geom_point() + 
  labs(x = "Runs", y = "Hits") + theme_bw()
p2 = ggplot(mlb11, aes(x = homeruns, y = hits)) + geom_point() + 
  labs(x = "Homeruns", y = "Hits") + theme_bw()
p3 = ggplot(mlb11, aes(x = stolen_bases, y = hits)) + geom_point() + 
  labs(x = "Stolen Bases", y = "Hits") + theme_bw()

grid.arrange(p1, p2, p3, ncol = 3)

####################################################################
## fit a model
####################################################################

linmod = lm(runs ~ at_bats + hits + homeruns + stolen_bases, data = mlb11)
tidy(linmod)


####################################################################
## fit a model "by hand"
####################################################################

X = cbind(1, mlb11$at_bats, mlb11$hits, mlb11$homeruns, mlb11$stolen_bases)
y = (mlb11$runs)

betaHat = solve(t(X) %*% X) %*% t(X) %*% y
betaHat

fitted = X %*% betaHat
sigmaHat = sqrt(t(y - fitted) %*% (y - fitted) / (30-4-1))
sigmaHat

VarBeta = as.numeric(sigmaHat^2) * (solve(t(X) %*% X))
VarBeta
sqrt(diag(VarBeta))




####################################################################
## regression analyses
####################################################################

linmod = lm(Dheight~Mheight, data = heights)
tidy(linmod)

## plot daughter height against mother height
ggplot(heights, aes(x = Mheight, y = Dheight)) + geom_point(color = "red") +
  labs(x = "Mother Height", y = "Daughter Height") +
  geom_smooth(method = "lm", se = FALSE, size = 2) + 
  theme_bw()


####################################################################
## mother's height in meters rather than inches
####################################################################

heights = mutate(heights, Mheight_m = Mheight * .0254)

linmod = lm(Dheight~Mheight_m, data = heights)
tidy(linmod)

## plot daughter height against mother height
ggplot(heights, aes(x = Mheight_m, y = Dheight)) + geom_point(color = "red") +
  labs(x = "Mother Height", y = "Daughter Height") +
  geom_smooth(method = "lm", se = FALSE, size = 2) + 
  theme_bw()


####################################################################
## effects of collinearity
####################################################################

linmod.col = lm(Dheight ~ Mheight + Mheight_m, data = heights)
summary(linmod.col)

X = as.matrix(cbind(1, select(heights, Mheight, Mheight_m)))
solve(t(X) %*% X)

## some measurement error on the meters
heights = mutate(heights, Mheight_m = Mheight_m + rnorm(1375, mean = 0, sd = .01))
summarize(heights, cor = cor(Mheight, Mheight_m))

X = as.matrix(cbind(1, select(heights, Mheight, Mheight_m)))
solve(t(X) %*% X)

ggplot(heights, aes(x = Mheight, y=Mheight_m)) + geom_point() + 
  labs(x = "Height in inches", y = "Height in meters") + 
  theme_bw()

linmod.me = lm(Dheight ~ Mheight + Mheight_m, data = heights)
tidy(linmod.me)




####################################################################
####################################################################
####################################################################
####################################################################
####################################################################
####################################################################