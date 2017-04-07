####################################################################
# Jeff Goldsmith
# Jan 26 2016
#
# R code used in P8111 Lecture 03
####################################################################

## clear work space
rm(list = ls())

## set working directory
setwd("~/Desktop/")

## load packages
library(ggplot2)
library(dplyr)

## load the "iris" dataset
data(iris)

####################################################################
## histograms and density plots; box and violin plots
####################################################################

p = ggplot(iris, aes(x = Sepal.Length))
p + geom_histogram( fill = "blue", alpha = .5, bins = 10)

# try fill values, both in geoms and by changing the base plot
# try some alpha values

p = ggplot(iris, aes(x = Species, y = Sepal.Length))
p + geom_violin(aes(fill = Species), alpha = .5, color = "blue") + geom_boxplot()

####################################################################
## scatterplots
####################################################################

p = ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width))
p + geom_point()

## aesthetics -- set color to blue; change point size; set alpha level; change x axis label and limits
p = ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width))
p + geom_point(color = "blue", size = 10, alpha = .67767685)

## deal with factors by color (aesthetic mapping) or facetting or both
p = ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species))
p + geom_point() + facet_grid(. ~ Species)


## changing the iris dataset
iris = iris %>% mutate(Species = toupper(Species))
p + geom_point(aes(color = Species))

## add a smooth
p + geom_point(alpha = .5) + 
  geom_smooth( method = "lm", se = FALSE) + facet_grid(. ~ Species) + 
  xlab("Sepal Length")


####################################################################
## build a plot from scratch
####################################################################

rm(list = ls())
data(iris)

## add a "size" variable to iris by cutting the Sepal.Length variable into small, medium and large,
## plot petal width by petal length, coloring and facetting by size and adding a lm line

iris %>% mutate(size = cut(Sepal.Length, 3, label = c("small", "medium", "large"))) %>%
  ggplot(aes(x = Petal.Length, y = Petal.Width, color = size)) + geom_point() + facet_grid(. ~ size) + 
    geom_smooth(method = "lm")

####################################################################
####################################################################
####################################################################
####################################################################
####################################################################
####################################################################