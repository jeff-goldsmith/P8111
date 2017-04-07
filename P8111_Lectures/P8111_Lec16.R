####################################################################
# Jeff Goldsmith
# Mar 24 2016
#
# R code used in P8111 Lecture 16
####################################################################

## clear work space
rm(list = ls())

## load libraries
library(MASS)
library(dplyr)
library(reshape2)
library(ggplot2)
library(glmnet)


####################################################################
## load the life expectancy data and run a model using all variables
####################################################################

data(state)
statedata = data.frame(state.x77,row.names=state.abb)
model.full = lm(Life.Exp ~., data=statedata)
coef(model.full)

summary(model.full)

####################################################################
## try a few ridge regression penalties
####################################################################

library(MASS)

model.ridge1 = lm.ridge(Life.Exp ~., data=statedata, lambda = 1000000)
coef(model.ridge1)

model.ridge2 = lm.ridge(Life.Exp ~., data=statedata, lambda = .0000001)
coef(model.ridge2)

####################################################################
## use cross-validation to choose lambda in ridge regression
####################################################################

set.seed(12)

grid.lam = seq(-2, 14, by = .1)
lam = 2^(grid.lam)

COEFS = matrix(NA, nrow = 7, ncol = length(lam))
MSE = matrix(NA, nrow = 10, ncol = length(lam))

for(k in 1:10){
    
  statedata = mutate(statedata, 
                     cv_group = sample(1:50, 50, replace = FALSE) <= 40,
                     cv_group = factor(cv_group, levels = c(TRUE, FALSE), 
                                       labels = c("train", "test")))
 
  for(l in 1:length(lam)){
      
    data.train = filter(statedata, cv_group == "train") %>% dplyr::select(., -cv_group)
    data.test = filter(statedata, cv_group == "test") %>% dplyr::select(-cv_group)
    
    model.cur = lm.ridge(Life.Exp ~ ., data=data.train, lambda = lam[l])
    predictions = as.matrix(cbind(1, dplyr::select(data.test, -Life.Exp))) %*% 
      coef(model.cur)
    MSE[k,l] = mean((dplyr::select(data.test, Life.Exp) - predictions)^2)

    COEFS[,l] = coef(lm.ridge(Life.Exp ~., data = dplyr::select(statedata, -cv_group), lambda = lam[l]))[-1]
    
  }
  
}


## plot CV curve for ridge regression
plot.dat = data.frame(grid = grid.lam, MSE = apply(MSE, 2, mean))

ggplot(plot.dat, aes(x = grid, y = MSE)) + geom_path() + 
  labs(x = expression(log[2](lambda)), ylab = "CV MSE")

## coefficient plot for ridge regression
rownames(COEFS) = colnames(dplyr::select(statedata, -Life.Exp, -cv_group))
colnames(COEFS) = grid.lam
plot.dat = melt(COEFS)

ggplot(plot.dat, aes(x = Var2, y = value, group = Var1, color = Var1)) + geom_path() +
  labs(x = expression(log[2](lambda)), ylab = "Coefficient")


## output of final model
Lam.Final = lam[which.min(apply(MSE, 2, mean))]
model.ridge3 = lm.ridge(Life.Exp ~., data=statedata, lambda = Lam.Final)
coef(model.ridge3)


####################################################################
## try a few lasso penalties
####################################################################

rm(list = ls())

data(state)
statedata = data.frame(state.x77,row.names=state.abb)

X = as.matrix(statedata[,-4])
y = statedata[,4]

model.lasso1 = glmnet(X, y, lambda = 0.00001)
coef(model.lasso1)

model.lasso2 = glmnet(X, y, lambda = 0.01)
coef(model.lasso2)

model.lasso3 = glmnet(X, y, lambda = 10)
coef(model.lasso3)

####################################################################
## use cross-validation to choose lambda
####################################################################

set.seed(12)

grid.lam = seq(-9, 0, by = .1)
lam = 2^(grid.lam)

GROUP = sample(rep(1:5, each = 10), 50)
MSE = matrix(NA, nrow = 50, ncol = length(lam))
COEFS = matrix(NA, nrow = 7, ncol = length(lam))

COEFS = matrix(NA, nrow = 7, ncol = length(lam))
MSE = matrix(NA, nrow = 10, ncol = length(lam))

for(k in 1:10){
  
  statedata = mutate(statedata, 
                     cv_group = sample(1:50, 50, replace = FALSE) <= 40,
                     cv_group = factor(cv_group, levels = c(TRUE, FALSE), 
                                       labels = c("train", "test")))
  
  for(l in 1:length(lam)){
    
    data.train = filter(statedata, cv_group == "train") %>% dplyr::select(., -cv_group)
    data.test = filter(statedata, cv_group == "test") %>% dplyr::select(-cv_group)
    
    X.test = dplyr::select(data.train, -Life.Exp) %>% as.matrix
    y.test = dplyr::select(data.train, Life.Exp) %>% as.matrix %>% as.vector
    model.cur = glmnet(X.test, y.test, lambda = lam[l])
    
    predictions = predict(model.cur, newx = dplyr::select(data.test, -Life.Exp) %>% as.matrix)
    y.test = dplyr::select(data.test, Life.Exp) %>% as.matrix %>% as.vector
    
    MSE[k,l] = mean((y.test - predictions)^2)

    X = dplyr::select(statedata, -Life.Exp, -cv_group) %>% as.matrix
    y = dplyr::select(statedata, Life.Exp, -cv_group) %>% as.matrix %>% as.vector
    
    COEFS[,l] = coef(glmnet(X, y, lambda = lam[l]))[-1]
    
  }
  
}


## plot CV curve for ridge regression
plot.dat = data.frame(grid = grid.lam, MSE = apply(MSE, 2, mean))

ggplot(plot.dat, aes(x = grid, y = MSE)) + geom_path() + 
  labs(x = expression(log[2](lambda)), ylab = "CV MSE")

## coefficient plot for ridge regression
rownames(COEFS) = colnames(dplyr::select(statedata, -Life.Exp, -cv_group))
colnames(COEFS) = grid.lam
plot.dat = melt(COEFS)

ggplot(plot.dat, aes(x = Var2, y = value, group = Var1, color = Var1)) + geom_path() +
  labs(x = expression(log[2](lambda)), ylab = "Coefficient")


## output of final model
Lam.Final = lam[which(apply(MSE, 2, mean) == min(apply(MSE, 2, mean)))]
model.lasso4 = glmnet(X, y, lambda = Lam.Final)
coef(model.lasso4)



####################################################################
####################################################################
####################################################################
####################################################################
####################################################################
####################################################################