####################################################################
# Jeff Goldsmith
# March 22 2016
#
# R code used in P8111 Lecture 15
####################################################################

## clear work space
rm(list = ls())

####################################################################
## load the life expectancy data and run backward selection
####################################################################

data(state)
statedata = data.frame(state.x77,row.names=state.abb)
g = lm(Life.Exp ~., data=statedata)
summary(g)
AIC(g)


g = lm(Life.Exp ~ . - Area, data=statedata)
summary(g)
AIC(g)

g = lm(Life.Exp ~ . - (Area + Illiteracy), data=statedata)
summary(g)
AIC(g)

g = lm(Life.Exp ~ . - (Area + Illiteracy + Income), data=statedata)
summary(g)
AIC(g)

g = lm(Life.Exp ~ . - (Area + Illiteracy + Income + Population), data=statedata)
summary(g)
AIC(g)

####################################################################
####################################################################
####################################################################
####################################################################
####################################################################
####################################################################