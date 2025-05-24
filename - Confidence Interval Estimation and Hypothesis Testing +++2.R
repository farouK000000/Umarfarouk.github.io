# Title: "Lecture 3: Confidence Interval Estimation and Hypothesis Testing"
# Author: "Dr William Godfred Cantah"
# Date: "18th June 2024"

install.packages("devtools")
install.packages("shiny")
install.packages("usethis")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("dplyr")

# Load necessary libraries
library(PoEdata)
library(ggplot2)
library(dplyr)
library(knitr)

# Confidence intervals in the food model
data("food")  # Load the "food" dataset
alpha <- 0.05  # Specified significance level
mod_1 <- lm(food_exp ~ income, data = food)  # Linear model of food expenditure based on income
b2 <- coef(mod_1)[[2]]  # Extract the coefficient of income
df <- df.residual(mod_1)  # Degrees of freedom
smod_1 <- summary(mod_1)  # Summary of the linear model
se_b2 <- coef(smod_1)[2, 2]  # Standard error of the coefficient of income
tc <- qt(1 - alpha / 2, df)  # Critical value from the t-distribution
low_b <- b2 - tc * se_b2  # Lower bound of the confidence interval
upp_b <- b2 + tc * se_b2  # Upper bound of the confidence interval

# Print the results
cat("Coefficient of income (b2):", b2, "\n")  #Prints the estimated coefficient of income
cat("Standard error of b2:", se_b2, "\n")     #Prints the standard error of the estimated coefficient.
cat("Critical value (tc):", tc, "\n")         #Prints the critical value from the t-distribution.
cat("95% Confidence Interval for b2:\n")      #Prints a label for the confidence interval.
cat("Lower bound:", low_b, "\n")              #Prints the lower bound of the confidence interval.
cat("Upper bound:", upp_b, "\n")              #Prints the upper bound of the confidence interval.

# Another approach through the built-in function
confint(mod_1)  # Calculate confidence intervals using built-in function
lowb_b2 <- confint(mod_1)[2, 1]  # Lower bound from built-in function
uppb_b2 <- confint(mod_1)[2, 2]  # Upper bound from built-in function

# Confidence intervals in repeated samples
data("table2_2")  # Load the "table2_2" dataset
alpha <- 0.05  # Significance level
mod_1 <- lm(y1 ~ x, data = table2_2)  # Linear model to determine degrees of freedom
df <- df.residual(mod_1)  # Degrees of freedom
tc <- qt(1 - alpha / 2, df)  # Critical value from the t-distribution

# Initiate four vectors that will store the results
lowb_1 <- rep(0, 10)  # Repeat 0 ten times for lower bounds of intercept
uppb_1 <- rep(0, 10)  # Repeat 0 ten times for upper bounds of intercept
lowb_2 <- rep(0, 10)  # Repeat 0 ten times for lower bounds of slope
uppb_2 <- rep(0, 10)  # Repeat 0 ten times for upper bounds of slope

# Loop through each column in the dataset (each set of income)
for (i in 2:ncol(table2_2)) {
  dat <- data.frame(cbind(table2_2[, 1], table2_2[, i]))  # Create a data frame for each sample
  names(dat) <- c("x", "y")  # Name the columns
  mod_1 <- lm(y ~ x, data = dat)  # Linear model for each sample
  smod_1 <- summary(mod_1)  # Summary of the model
  b1 <- coef(mod_1)[[1]]  # Intercept
  b2 <- coef(mod_1)[[2]]  # Slope
  se_b1 <- coef(smod_1)[1, 2]  # Standard error of the intercept
  se_b2 <- coef(smod_1)[2, 2]  # Standard error of the slope
  lowb_1[i] <- b1 - tc * se_b1  # Lower bound of the intercept
  uppb_1[i] <- b1 + tc * se_b1  # Upper bound of the intercept
  lowb_2[i] <- b2 - tc * se_b2  # Lower bound of the slope
  uppb_2[i] <- b2 + tc * se_b2  # Upper bound of the slope
}

# Create a data frame for the results
table <- data.frame(lowb_1, uppb_1, lowb_2, uppb_2)
# Print the table using kable for a neat display
kable(table, caption = "Confidence intervals for $b_{1}$ and $b_{2}$", align = "c")

# Hypothesis tests
data("food")  # Load the "food" dataset

alpha <- 0.05  # Significance level
mod_1 <- lm(food_exp ~ income, data = food)  # Linear model of food expenditure based on income
smod_1 <- summary(mod_1)  # Summary of the model
coef(smod_1)  # Coefficients of the model
b2 <- coef(smod_1)[2, 1]  # Coefficient of income
se_b2 <- sqrt(vcov(mod_1)[2, 2])  # Standard error of the coefficient of income
df <- df.residual(mod_1)  # Degrees of freedom
t <- b2 / se_b2  # Test statistic
tc <- qt(1 - alpha / 2, df)  # Critical value from the t-distribution
t > tc  # Check if the test statistic is greater than the critical value

# Plotting the density function and the values of t
curve(dt(x, df), -2.5 * se_b2, 2.5 * se_b2, xlab = "t", ylab = "")  # Plot t-distribution
abline(v = c(-tc, tc, t), col = c("red", "red", "blue"), lty = c(2, 2, 3))  # Add lines for critical values and test statistic
legend("topleft", legend = c("-tc", "tc", "t"), lty = c(2, 2, 3), col = c("red", "red", "blue"))  # Add a legend

# Testing the null hypothesis that beta2 is less than 5.5
c <- 5.5  # Hypothesized value
aplha <- 0.05  # Significance level (note the typo, should be alpha)
t <- (b2 - c) / se_b2  # Test statistic
tc <- qt(1 - aplha, df)  # Critical value
curve(dt(x, df), -2.5 * se_b2, 2.5 * se_b2, xlab = "t", ylab = "")  # Plot t-distribution
abline(v = c(tc, t), col = c("red", "blue"), lty = c(2, 3))  # Add lines for critical value and test statistic
legend("topleft", legend = c("tc", "t"), lty = c(2, 3), col = c("red", "blue"))  # Add a legend
t > tc  # Check if the test statistic is greater than the critical value

# Testing the null hypothesis that beta2 is greater than 15
c <- 15  # Hypothesized value
alpha <- 0.05  # Significance level
t <- (b2 - c) / se_b2  # Test statistic
tc <- qt(alpha, df)  # Critical value
curve(dt(x, df), -2.5 * se_b2, 2.5 * se_b2, xlab = "t", ylab = "")  # Plot t-distribution
abline(v = c(t, tc), col = c("blue", "red"), lty = c(3, 2))  # Add lines for test statistic and critical value
legend("topleft", legend = c("t", "tc"), lty = c(3, 2), col = c("blue", "red"))  # Add a legend

# The p-value

# Right-tail test,  H0: ß2 < c, A: ß2 > c
c <- 5.5  # Hypothesized value
t <- (b2 - c) / se_b2  # Test statistic
p <- 1 - pt(t, df)  # p-value for the right-tail test

# Left-tail test
c <- 15  # Hypothesized value
t <- (b2 - c) / se_b2  # Test statistic
p <- pt(t, df)  # p-value for the left-tail test

# Two-tail test
c <- 0  # Hypothesized value
t <- (b2 - c) / se_b2  # Test statistic
p <- 2 * (1 - pt(abs(t), df))  # p-value for the two-tail test

# Testing linear combinations of parameters
alpha <- 0.05  # Significance level
x <- 20  # Value of the independent variable
mod_1 <- lm(food_exp ~ income, data = food)  # Linear model
tc <- qt(1 - alpha / 2, df)  # Critical value
df <- df.residual(mod_1)  # Degrees of freedom
b1 <- coef(mod_1)[[1]]  # Intercept
b2 <- coef(mod_1)[[2]]  # Slope
var_b1 <- vcov(mod_1)[1, 1]  # Variance of the intercept
var_b2 <- vcov(mod_1)[2, 2]  # Variance of the slope
cov_b1b2 <- vcov(mod_1)[1, 2]  # Covariance of the intercept and slope
L <- b1 + b2 * x  # Linear combination of parameters
var_L <- var_b1 + x^2 * var_b2 + 2 * x * cov_b1b2  # Variance of the linear combination
se_L <- sqrt(var_L)  # Standard error of the linear combination
low_bL <- L - tc * se_L  # Lower bound of the confidence interval
upp_bL <- L + tc * se_L  # Upper bound of the confidence interval
low_bL  # Lower bound
upp_bL  # Upper bound

# Function to perform the analysis with different significance levels
analyze_with_alpha <- function(alpha) {
  # Confidence intervals for the food model
  tc <- qt(1 - alpha / 2, df)
  low_b <- b2 - tc * se_b2
  upp_b <- b2 + tc * se_b2
  cat("Confidence interval for alpha =", alpha, ":\n")
  cat("Lower bound:", low_b, "\nUpper bound:", upp_b, "\n\n")
  
  # Hypothesis test for beta2 less than 5.5
  c <- 5.5
  t <- (b2 - c) / se_b2
  tc <- qt(1 - alpha, df)
  cat("Hypothesis test for beta2 < 5.5 with alpha =", alpha, ":\n")
  cat("Test statistic:", t, "\nCritical value:", tc, "\nReject H0:", t > tc, "\n\n")
  
  # Hypothesis test for beta2 greater than 15
  c <- 15
  t <- (b2 - c) / se_b2
  tc <- qt(alpha, df)
  cat("Hypothesis test for beta2 > 15 with alpha =", alpha, ":\n")
  cat("Test statistic:", t, "\nCritical value:", tc, "\nReject H0:", t < tc, "\n\n")
}

# Perform the analysis with different significance levels
analyze_with_alpha(0.01)
analyze_with_alpha(0.05)
analyze_with_alpha(0.10)

# Enjoy!!!


tinytex::install_tinytex(bundle = 'TinyTeX')
