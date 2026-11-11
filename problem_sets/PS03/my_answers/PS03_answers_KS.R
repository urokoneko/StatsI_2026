                                                                                        #####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)
# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# read in data
inc.sub <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_2026/main/datasets/incumbents_subset.csv")
## Q1-1
fit1 = lm(formula = voteshare ~ difflog, data = inc.sub)
summary(fit1)
## Q1-2
library(ggplot2)
ggplot(data = inc.sub, aes(x = difflog, y = voteshare)) + 
  geom_point() +
  labs(title = "Incumbent Vote Share vs. Spending Difference")
# add the regression line
ggplot(data = inc.sub, aes(x = difflog, y = voteshare)) + 
  geom_point() +
  geom_smooth(method= "lm") +
  labs(title = "Incumbent Vote Share vs. Spending Difference")
## Q1-3 
res1 <- residuals(fit1)
head(res1)



## Q2-1
fit2 = lm(formula = presvote ~ difflog, data = inc.sub)
summary(fit2)
## Q2-2
ggplot(data = inc.sub, aes(x = difflog, y = presvote)) + 
  geom_point() +
  labs(title = "Presidential Vote Share vs. Spending Difference")
# add the regression line
ggplot(data = inc.sub, aes(x = difflog, y = presvote)) + 
  geom_point() +
  geom_smooth(method= "lm") +
  labs(title = "Presidential Vote Share vs. Spending Difference")
## Q2-3 
res2 <- residuals(fit2)
head(res2)


## Q3-1
fit3 = lm(formula = voteshare ~ presvote, data = inc.sub)
summary(fit3)
## Q3-2
ggplot(data = inc.sub, aes(x = presvote, y = voteshare)) + 
  geom_point() +
  labs(title = "Incumbent Vote vs. Presidential Vote Share")
# add the regression line
ggplot(data = inc.sub, aes(x = presvote, y = voteshare)) + 
  geom_point() +
  geom_smooth(method= "lm") +
  labs(title = "Incumbent Vote vs. Presidential Vote Share")


## Q4-1
fit4 = lm(res1 ~ res2)
summary(fit4)
## Q4-2
ggplot(data = inc.sub, aes(x = res2, y = res1)) + 
  geom_point() +
  labs(title = "Residual of Question 1 vs. Residual of Question 2")
# add a regression line
ggplot(data = inc.sub, aes(x = res2, y = res1)) + 
  geom_point() +
  geom_smooth(method= "lm") +
  labs(title = "Residual of Question 1 vs. Residual of Question 2")



## Q5-1
fit5 = lm(formula = voteshare ~ difflog + presvote, data = inc.sub)
summary(fit5)
