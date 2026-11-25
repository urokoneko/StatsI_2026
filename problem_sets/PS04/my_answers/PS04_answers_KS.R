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
lapply(c("car"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Q1-A
install.packages("car")
library(car)
data(Prestige)
help(Prestige)

professional <- ifelse(Prestige$type == 'prof', 1, 0)
# Q1-B
model1 <- lm(prestige ~ income * professional, data = Prestige)
summary(model1)
# Q1-F
marginal1 <- (0.0032 - 0.0023) * 1000
# Q1-G
marginal2 <- 37.78 - 0.0023 * 6000

# Q2-A
t_val1 <- 0.042 / 0.016
p_val1 <- 2 * pt(-abs(t_val1), df = 131 - 2 - 1) 
t_crit1 <- qt(1 - 0.05 / 2, df = 131 - 2 - 1)
c(t_val1, p_val1, t_crit1)
# Q2-B
t_val2 <- 0.042 / 0.013
p_val2 <- 2 * pt(-abs(t_val2), df = 131 - 2 - 1)
t_crit2 <- qt(1 - 0.05 / 2, df = 131 - 2 - 1)
c(t_val2, p_val2, t_crit2)
