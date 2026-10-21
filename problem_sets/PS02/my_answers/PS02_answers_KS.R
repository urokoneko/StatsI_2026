# rm(list = ls())
##### Question 1: Political Science
result <- matrix(c(14, 7, 6, 7, 7, 1), nrow = 2, ncol = 3)
upper <- sum(result[1, ])
lower <- sum(result[2, ])
ns <- sum(result[ ,1])
bribe <- sum(result[ ,2])
warning <- sum(result[ ,3])
total <- upper + lower

# expected frequency
# row total * column total * grand total
u_ns <- total * upper/total * ns/total # upper x not stopped
u_bribe <- total * upper/total * bribe/total # upper x bribe
u_warning <- total * upper/total * warning/total # upper x warning

l_ns <- total * lower/total * ns/total # lower x not stopped
l_bribe <- total * lower/total * bribe/total # lower x bribe
l_warning <- total * lower/total * warning/total # lower x warning

# Quection 1A
# contribution of each cells <- (Observed - Expected)^2 / Expected
u_ns_contrib <- (14 - u_ns)^2 / u_ns 
u_bribe_contrib <-  (6 - u_bribe)^2 / u_bribe
u_warning_contrib <- (7 - u_warning)^2 / u_warning
l_ns_contrib <- (7 - l_ns)^2 / (l_ns)
l_bribe_contrib <- (7 - l_bribe)^2 / l_bribe
l_warning_contrib <- (1 - l_warning)^2 / l_warning

# chi test statistic value
chi_square <- u_ns_contrib + u_bribe_contrib + u_warning_contrib + 
  l_ns_contrib + l_bribe_contrib + l_warning_contrib

# Question 1B
# p-value
p_value <- pchisq(chi_square, df = (2 - 1)*(3 - 1), lower.tail = FALSE)

# Question 1C
# standardized residual <- (Observed - Expected) / sqrt(Expected)
u_ns_residual <- (14 - u_ns) / sqrt(u_ns) 
u_bribe_residual <-  (6 - u_bribe) / sqrt(u_bribe) 
u_warning_residual <- (7 - u_warning) / sqrt(u_warning)
l_ns_residual <- (7 - l_ns) / sqrt(l_ns)
l_bribe_residual <- (7 - l_bribe) / sqrt(l_bribe)
l_warning_residual <- (1 - l_warning) / sqrt(l_warning)

##### Question 2: Economics
women_data <- read.csv("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv")
head(women_data)

# Question 2B
model <- lm(water ~ female, data = women_data)
summary(model)
