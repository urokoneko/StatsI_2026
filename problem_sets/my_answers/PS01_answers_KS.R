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

lapply(c(),  pkgTest)

#####################
# Problem 1
#####################
# Question 1
y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)
y_mean <- mean(y)
y_sd <- sd(y)
y_se <- y_sd / sqrt(length(y)) # standard error of the mean
t_value <- qt(0.95, df = length(y) - 1) # confidence coefficient = 0.90
CI_lower <- (y_mean) - t_value * (y_se)
CI_upper <- (y_mean) + t_value * (y_se)

# Question 2
t_stat = (y_mean - 100) / y_se # t-statistic   
p_value <- pt(t_stat, df = length(y) - 1, lower.tail = FALSE)

#####################
# Problem 2
#####################
library(ggplot2)
expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_2026/main/datasets/expenditure.txt", header=T)
#summary(expenditure)

#Question 1
p1 <- ggplot(expenditure, aes(x = X1, y = Y)) +
  geom_point(color = "steelblue") +
  labs(x = "Personal Income", y = "Expenditure", title = "Correlation between Personal Inceme(X1) and Expenditure(Y)") +
  theme_minimal()
p2 <- ggplot(expenditure, aes(x = X2, y = Y)) +
  geom_point(color = "darkgreen") +
  labs(x = "Financially Insecure", y = "Expenditure", title = "Correlation between Financial Insecurity(X2) and Expenditure(Y)") +
  theme_minimal()
p3 <- ggplot(expenditure, aes(x = X3, y = Y)) +
  geom_point(color = "coral") +
  labs(x = "Urban Population", y = "Expenditure", title = "Correlation between Urban Population(X3) and Expenditure(Y)") +
  theme_minimal()
expenditure_cor <- cor(expenditure[,c("Y", "X1", "X2", "X3")])

# Question 2
expenditure$Region <- factor(
  expenditure$Region,
  levels = c(1, 2, 3, 4),
  labels = c("Northeast", "North Central", "South", "West")
)

boxplot_expenditure <- ggplot(expenditure, aes(x = factor(Region) , y = Y)) +
  geom_boxplot() +
  labs(title = "Expenditure on Shelters/Housing Assistance by Region", x = NULL, y = "Expenditure")
boxplot_expenditure

#Question 3
p4 <- ggplot(expenditure, aes(x = X1, y = Y)) +
  geom_point(aes(color = factor(Region), shape = factor(Region))) +
  labs(x = "Personal Income", y = "Expenditure", title = "Correlation between Personal Inceme(X1) and Expenditure(Y) by Region") +
  theme_minimal()

