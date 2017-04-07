####################################################################
# Jeff Goldsmith
# Feb 9 2016
#
# R code used in P8111 Lecture 06
####################################################################

## clear work space
rm(list = ls())

## set working directory
setwd("~/Desktop/")

## load libraries
library(ggplot2)
library(broom)

####################################################################
## multiple linear regression
####################################################################

set.seed(1)

data.mlr = data.frame(age = rnorm(40, 66, 6),
                      sex = rep(c("male", "female"), each = 20)) %>%
  mutate(weight = 6 + 2.5 * age + 20 * (sex == "male") + rnorm(40, 0, 6)) %>%
  tbl_df

ggplot(data = data.mlr, aes(y=weight, x = age, color = sex)) + 
  geom_point(size = 3) + geom_smooth(method = "lm", se = FALSE) + 
  theme_bw()

## second plot; lines only
ggplot(data = data.mlr, aes(y=weight, x = age, color = sex)) + 
  geom_smooth(method = "lm", se = FALSE) + 
  theme_bw()

## description, modeling
summary(data.mlr)

linmod = lm(weight ~ age + sex, data = data.mlr)
tidy(linmod)
summary(linmod)

head(data.mlr)
model.matrix(linmod) %>% head

tail(data.mlr)
model.matrix(linmod) %>% tail

####################################################################
## interactions
####################################################################

set.seed(1)

data.mlr = data.frame(age = rnorm(40, 66, 6),
                      sex = rep(c("male", "female"), each = 20)) %>%
  mutate(weight = -20 + 2.5 * age - 60 * (sex == "male") + 2 * (sex == "male") * age + rnorm(40, 0, 6)) %>%
  tbl_df

ggplot(data = data.mlr, aes(y=weight, x = age, color = sex)) + 
  geom_point(size = 3) + geom_smooth(method = "lm", se = FALSE) + 
  theme_bw()

## second plot; lines only
ggplot(data = data.mlr, aes(y=weight, x = age, color = sex)) + 
  geom_smooth(method = "lm", se = FALSE) + 
  theme_bw()

## description, modeling
summary(data.mlr)

linmod = lm(weight ~ age * sex, data = data.mlr)
tidy(linmod)

head(data.mlr)
model.matrix(linmod) %>% head

tail(data.mlr)
model.matrix(linmod) %>% tail



####################################################################
## categorical predictors
####################################################################

## clear work space
rm(list = ls())

## set working directory
setwd("~/Desktop/")

## load data
load("BPDat.RDA")

## see what we've loaded
head(BP)
summary(BP)

## tidy data
BP = BP %>% rename(Treatment = x1, BP = x2) %>%
  mutate(Treatment = factor(Treatment, levels = 1:3, 
                            labels = c("placebo", "exercise", "drug"))) %>%
  filter(BP != 999)

## explore data
summary(BP)
BP %>% group_by(Treatment) %>% summarize(n = n(), 
                                         group_mean = mean(BP), 
                                         group_median = median(BP))

ggplot(BP, aes(x = Treatment, y = BP)) + geom_boxplot() + 
  theme_bw()

## linear model
lm(BP ~ Treatment, data = BP) %>% tidy
lm(BP ~ Treatment, data = BP) %>% model.matrix %>% head

## linear model: releveling
BP %>% mutate(Treatment = relevel(Treatment, ref = "exercise")) %>%
  lm(BP ~ Treatment, data = .) %>% 
  tidy

## linear model: no intercept
lm(BP ~ 0 + Treatment, data = BP) %>% tidy


####################################################################
####################################################################
####################################################################
####################################################################
####################################################################
####################################################################