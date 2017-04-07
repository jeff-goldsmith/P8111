####################################################################
# Jeff Goldsmith
# Jan 21 2016
#
# R code used in P8111 Lecture 02
####################################################################

## clear work space
rm(list = ls())

## load packages
install.packages("dplyr")
library(dplyr)

## create a first data frame

data = data.frame(
  seq = 1:10,
  let = letters[1:10],
  bool = 1:10 < 5
)

## do some exploration
dim(data)
head(data)
summary(data)

data$seq
summary(data$seq)
hist(data$seq)

## convert to tbl_df; explor
data = tbl_df(data)
data
glimpse(data)


## create a similar but larger data frame
data = data.frame(
  seq = 1:20,
  let = letters[1:20],
  bool = 1:20 < 5
)

data = tbl_df(data)



## FILTER rows by seq
data[which(data$seq < 10),]

filter(data, seq <= 15 & bool == FALSE)


## FILTER rows by bool
filter(data, bool == TRUE)

## ARRANGE rows by seq (descending)
arrange(data, desc(seq))

## SELECT the let column
select(data, let)


## SELECT seq and bool
select(data, -let)

## SELECT everything except bool
select(data, seq, let)
select(data, -bool)


## RENAME all columns
data = rename(data, Seq = seq, Let = let, Logical = bool)

colnames(data) = c('some', 'thing', 'else')



## MUTATE to add a column of random numbers
head(  tbl_df(   mutate(data, random = rnorm(20))    )   )

## MUTATE to add a column of standard normals and a column that indicates >0
data = mutate(data, random = rnorm(20),
             seq_squared = Seq^2,
             bool = random > 0)


## SUMMARIZE the data to determine sample size, and the mean and standard deviation of the standard normal
summarize(data, N = n(), Mean = mean(random), SD = sd(random) )

## GROUP the data by bool, and re-summarize
summarize(filter(data, bool == FALSE), N = n(), Mean = mean(random), SD = sd(random) )
summarize(filter(data, bool == TRUE), N = n(), Mean = mean(random), SD = sd(random) )

data = group_by(data, bool, Logical)
summarize(data, N = n(), Mean = mean(random), SD = sd(random) )

## clear workspace

rm(list = ls())

## re-create the dataframe 'data'

data = data.frame(
  seq = 1:20,
  let = letters[1:20],
  bool = 1:20 < 5
)

data = tbl_df(data)

## rename columns, add random normal sample, arrange by sample, remove rows for which sample 
## isn't between -1 and 1, create new logical variable (sample <0), remove 'Logical', 
## group by new logical variable (bool), and summarize as above.


rename(data, Seq = seq, Let = let, Logical = bool) %>%
  mutate(., random = rnorm(20)) %>%
  filter(., random > -1 & random < 1) %>%
  arrange(., random) %>%
  mutate(., bool = random < 0) %>%
  select(., -Logical) %>%
  group_by(., bool) %>%
  summarize(., N = n(), Mean = mean(random), SD = sd(random) )


data = rename(data, Seq = seq, Let = let, Logical = bool)
data = mutate(data.1, random = rnorm(20))
data = arrange(data.2, random)

arrange(   mutate(  rename(data, Seq = seq, Let = let, Logical = bool), random = rnorm(20)),    random)


## clear workspace

rm(list = ls())

## load the "iris" dataset

data(iris)
iris = tbl_df(iris)
iris
glimpse(iris)
str(iris)

## remove entries with petal length < 1.5, remove sepal width and petal width, 
## rename sepal length and petal length to "SepaL" and "peTal", group by species,
## and summarize to include n, mean sepal length, and the correlation between sepal and petal length

filter(iris, Petal.Length >= 1.5) %>%
  select(., -c(Sepal.Width, Petal.Width)) %>%
  rename(., SepaL = Sepal.Length, peTal = Petal.Length) %>%
  group_by(., Species) %>%
  summarize(., n = n(), mean = mean(SepaL), cor = cor(SepaL, peTal))

## repeat, but exclude entries with petal length < 2 and > 6

filter(iris, Petal.Length >= 2 & Petal.Length <= 6) %>%
  select(., -c(Sepal.Width, Petal.Width)) %>%
  rename(., SepaL = Sepal.Length, peTal = Petal.Length) %>%
  group_by(., Species) %>%
  summarize(., n = n(), mean = mean(SepaL), cor = cor(SepaL, peTal))

## create new variables Sepal.Sum and Petal.Sum that are the sum of the respective widths and lengths,
## focus only on setosa plants, and compute the mean and variance of the new sum variables

mutate(iris, Sepal.Sum = Sepal.Length + Sepal.Width, 
             Petal.Sum = Petal.Length + Petal.Width) %>%
  filter(Species == "setosa") %>%
  summarize(mean.sepal.sum = mean(Sepal.Sum), var.sepal.sum = var(Sepal.Sum),
            mean.petal.sum = mean(Petal.Sum), var.petal.sum = var(Petal.Sum))


## create new variables Sepal.Sum and Petal.Sum that are the sum of the respective widths and lengths,
## group by the median of sepal's sum, and compute the sample size and mean of petal length in each group

mutate(iris, Sepal.Sum = Sepal.Length + Sepal.Width, 
             Petal.Sum = Petal.Length + Petal.Width,
             Thresh.Sepal = Sepal.Sum > median(Sepal.Sum)) %>%
  group_by(Thresh.Sepal) %>%
  summarize(n = n(), mean.petal = mean(Petal.Length))




####################################################################
####################################################################
####################################################################
####################################################################
####################################################################
####################################################################