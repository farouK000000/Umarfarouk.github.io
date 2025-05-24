title: "Lecture 1: Simple Linear Regression"




### Load necessary libraries####
install.packages("devtools")  # Install devtools package if not already installed
install.packages("git2rdata")  # Install git2rdata package if not already installed
install.packages("ggplot2")  # Install ggplot2 package if not already installed
install.packages("dplyr")  # Install dplyr package if not already installed
install.packages("outreg")
install_git("https://github.com/ccolonescu/PoEdata")  # Install PoEdata package from GitHub
install.packages("remotes")
remotes::install_github("ccolonescu/PoEdata")
library(corrplot)
library(outreg)
library(devtools)  # Tools for development with R
library(git2rdata)  # Git integration for R data
library(PoEdata)  # Loads datasets from the PoEdata package
library(ggplot2)  # A plotting system for R
library(dplyr)  # Data manipulation package
require(pastecs)
options(scipen = 999)

# Load the 'food' dataset from PoEdata package
data("food")
head(food)  # Display the first few rows of the 'food' dataset to check the data
View(food)

### Data Summary and Cleaning####
str(food)  # Structure of the dataset
food <- na.omit(food)  # Remove any rows with missing values
attach(food)


#### Descriptive statistics #######
summary(food$food_exp)  # Summary statistics of the dataset

stat.desc(food)
round(stat.desc(food, norm = TRUE), 4)

# Plot income vs food expenditure to visualize the relationship
plot(income, food_exp, type = "p",
     xlab = "Weekly income in $100", ylab = "Weekly food expenditure in $",
     xlim = c(0, max(food$income)), ylim = c(0, max(food$food_exp)),
     main = "Scatter Plot of Income vs Food Expenditure")

# Enhanced Plot using ggplot2
ggplot(food, aes(x = income, y = food_exp)) +
  geom_point(color = "blue", size = 2) +  # Scatter plot
  geom_smooth(method = lm, se = FALSE, color = "red") +  # Linear regression line
  xlab("Weekly Income in $100") + ylab("Weekly Food Expenditure in $") +
  ggtitle("Income vs Food Expenditure with Regression Line") +
  theme_minimal()  # Use a minimal theme


### correlation #### 
cor.test(food$food_exp, food$income, method = 'spearman')
cor(food$food_exp, food$income, method = "pearson")


M = cor(food)
set.seed(0)

corrplot(M, method = 'color', col = COL2(n=20), cl.length = 21, order = 'AOE',
         addCoef.col = 'grey')

## add p-values on no significant coefficients
testRes = cor.mtest(food, conf.level = 0.95)
testRes

## leave blank on no significant coefficient
corrplot(M, p.mat = testRes$p, method = 'circle', type = 'lower', insig ='blank',
         addCoef.col ='black', number.cex = 0.8, order = 'AOE', diag = FALSE)



corrplot(M, p.mat = testRes$p, insig = 'p-value')

## add all p-values
corrplot(M, p.mat = testRes$p, insig = 'p-value', sig.level = -1)

## add significant level stars
corrplot(M, p.mat = testRes$p, method = 'color', diag = FALSE, type = 'upper',
         sig.level = c(0.001, 0.01, 0.05), pch.cex = 0.9,
         insig = 'label_sig', pch.col = 'grey20', order = 'AOE')


#### Estimating a linear regression ####
mod <- lm(food_exp ~ income, data = food)

# Display model summary and coefficients
summary(mod)  # Summary of the linear model
coef(mod)  # Model coefficients


# Extract coefficients
b1 <- coef(mod)[1]  # Intercept coefficient
b2 <- coef(mod)[2]  # Slope coefficient

# Display coefficients
# Plot the data and add the regression line
plot(food$income, food$food_exp, type = "p",
     xlab = "Weekly income in $100", ylab = "Weekly food expenditure in $",
     xlim = c(0, max(food$income)), ylim = c(0, max(food$food_exp)),
     main = "Income vs Food Expenditure with Regression Line")
curve(b1 + b2*x, add = TRUE, col = "red", lwd = 2)  # Add regression curve
#abline(b1, b2, col = "blue", lwd = 2)  # Add regression line


# Model diagnostics
# Generate residuals from the model
# residuals <- residuals(mod)

# Add residuals to the food dataframe for plotting
# food$residuals <- residuals

# Plot residuals vs. fitted values
ggplot(food, aes(x = fitted(mod), y = residuals)) +
  geom_point(color = "blue", size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  xlab("Fitted Values") +
  ylab("Residuals") +
  ggtitle("Residuals vs. Fitted Values") +
  theme_minimal()

# Plot residuals vs. income
ggplot(food, aes(x = income, y = residuals)) +
  geom_point(color = "blue", size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  xlab("Income in $100") +
  ylab("Residuals") +
  ggtitle("Residuals vs. Income") +
  theme_minimal()

# Assuming you have the residuals in a variable called residuals in the food dataframe
mean_res <- mean(food$residuals)
sd_res <- sd(food$residuals)

ggplot(food, aes(x = residuals)) +
  geom_histogram(aes(y = ..density..), binwidth = 10, fill = "grey", color = "black") +
  stat_function(fun = dnorm, args = list(mean = mean_res, sd = sd_res), color = "blue", size = 1) +
  xlab("Residuals") +
  ylab("Density") +
  ggtitle("Histogram of Residuals with Normal Distribution Curve") +
  theme_minimal()

# Plot QQ-plot of residuals to check for normality
qqnorm(residuals)
qqline(residuals, col = "red")


par(mfrow = c(2, 2))  # Set up a 2x2 plotting area
plot(mod)  # Plot diagnostic plots for the linear model


# Calculate predicted values and R-squared manually
food_exp_hat <- b1 + b2 * food$income  # Predicted values
SST <- sum((food$food_exp - mean(food$food_exp))^2)  # Total Sum of Squares
SSR <- sum((food_exp_hat - mean(food$food_exp))^2)  # Regression Sum of Squares
SSE <- sum((food$food_exp - food_exp_hat)^2)  # Error Sum of Squares
r_sqr <- SSR / SST  # R-squared

# Display R-squared
print(paste("R-squared: ", round(r_sqr, 4)))


# Prediction with the linear regression model
new_x <- data.frame(income = c(20, 25, 27))  # New income values for prediction
y_hat <- predict(mod, new_x)  # Predict food expenditure for new income values
names(y_hat) <- c("$2000", "$2500", "$2700")  # Name the predicted values
print(y_hat)  # Display predicted values


# Compute the means of income and food expenditure
mean_income <- mean(food$income)
mean_food_exp <- mean(food$food_exp)

# Compute the elasticity
elasticity <- b2 * (mean_income / mean_food_exp)

# Display the elasticity
print(paste("Elasticity of food expenditure with respect to income: ", round(elasticity, 4)))

# Take the natural log of 'food_exp' and 'income'
food <- food %>%
  mutate(log_food_exp = log(food_exp),
         log_income = log(income))

# View the first few rows of the modified dataset
head(food)

# Plot the log-transformed variables
ggplot(food, aes(x = log_income, y = log_food_exp)) +
  geom_point(color = "blue") +
  xlab("Log of Income") +
  ylab("Log of Food Expenditure") +
  ggtitle("Log of Food Expenditure vs. Log of Income") +
  theme_minimal()

# Fit a linear model using the log-transformed variables
mod_log <- lm(log_food_exp ~ log_income, data = food)
# Display model summary and coefficients
summary(mod_log)  # Summary of the linear model
coef(mod_log)  # Model coefficients
# Generate residuals from the log-transformed model
log_residuals <- residuals(mod_log)

# Add residuals to the food dataframe for plotting
food$log_residuals <- log_residuals

# Plot residuals vs. fitted values for log-transformed model
ggplot(food, aes(x = fitted(mod_log), y = log_residuals)) +
  geom_point(color = "blue", size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  xlab("Fitted Values (Log Model)") +
  ylab("Residuals") +
  ggtitle("Residuals vs. Fitted Values (Log Model)") +
  theme_minimal()

# Plot residuals vs. log of income
ggplot(food, aes(x = log_income, y = log_residuals)) +
  geom_point(color = "blue", size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  xlab("Log of Income") +
  ylab("Residuals") +
  ggtitle("Residuals vs. Log of Income") +
  theme_minimal()

# Assuming you have the residuals in a variable called residuals in the food dataframe
mean_logres <- mean(food$log_residuals)
sd_logres <- sd(food$log_residuals)

ggplot(food, aes(x = log_residuals)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.1, fill = "grey", color = "black") +
  stat_function(fun = dnorm, args = list(mean = mean_logres, sd = sd_logres), color = "blue", size = 1) +
  xlab("Log Residuals") +
  ylab("Log Frequency") +
  ggtitle("Histogram of Log Residuals with Normal Distribution Curve") +
  theme_minimal()

# Plot QQ-plot of residuals to check for normality
qqnorm(log_residuals)
qqline(log_residuals, col = "red")


par(mfrow = c(2, 2))  # Set up a 2x2 plotting area
plot(mod_log)  # Plot diagnostic plots for the linear model

##COMPARING REGRESSION TABLES:

fitlist <- list(mod_log, mod)
outreg(fitlist)

# Repeated samples to assess regression coefficients
set.seed(123)  # Set seed for reproducibility
N <- nrow(food)  # Number of observations in the dataset
C <- 50  # Desired number of subsamples
S <- 38  # Desired sample size
sum_b2 <- 0
for (i in 1:C) {  # Loop over the number of subsamples
  set.seed(3 * i)  # Different seed for each subsample
  subsample <- food[sample(1:N, size = S, replace = TRUE), ]  # Create subsample with replacement
  mod_2 <- lm(food_exp ~ income, data = subsample)  # Fit model to subsample
  sum_b2 <- sum_b2 + coef(mod_2)[2]  # Sum slope coefficients
}
print(paste("Average slope coefficient: ", round(sum_b2 / C, 3)))  # Average slope coefficient across subsamples


# Estimated variances and covariances of the regression coefficients
vcov_matrix <- vcov(mod)  # Variance-covariance matrix of the coefficients
var_b1_hat <- vcov_matrix[1, 1]  # Variance of the intercept
var_b2_hat <- vcov_matrix[2, 2]  # Variance of the slope
cov_b1_b2_hat <- vcov_matrix[1, 2]  # Covariance between intercept and slope

# Display variances and covariance
print(paste("Variance of b1 (Intercept): ", round(var_b1_hat, 4)))
print(paste("Variance of b2 (Slope): ", round(var_b2_hat, 4)))
print(paste("Covariance between b1 and b2: ", round(cov_b1_b2_hat, 4)))


# Non-linear relationships
data("br")  # Load 'br' dataset
mod_3 <- lm(price ~ I(sqft^2), data = br)  # Quadratic model for price vs square feet
plot(br$sqft, br$price, 
     xlab = "Total square feet", ylab = "Sale price, $", col = "grey",
     main = "Price vs Square Feet with Quadratic Regression Line")
b1 <- coef(mod_3)[1]  # Intercept coefficient
b2 <- coef(mod_3)[2]  # Slope coefficient
curve(b1 + b2*x^2, col = "red", add = TRUE)  # Add quadratic regression curve


# Plot histograms of price and log(price)
par(mfrow = c(1, 2))  # Set up a 1x2 plotting area
hist(br$price, col = "grey", main = "Histogram of Price", xlab = "Price")  # Histogram of price
hist(log(br$price), col = "grey", main = "Histogram of Log(Price)", xlab = "Log(Price)")  # Histogram of log(price)


# Linear model for log(price) vs square feet
mod_4 <- lm(log(price) ~ sqft, data = br)
b1 <- coef(mod_4)[1]  # Intercept coefficient
b2 <- coef(mod_4)[2]  # Slope coefficient
plot(br$sqft, log(br$price), 
     xlab = "Total square feet", ylab = "Log(Sale price)", col = "grey",
     main = "Log(Price) vs Square Feet with Exponential Regression Line")
curve(b1 + b2*x, col = "blue", add = TRUE)  # Add linear regression line for log(price)

# Indicator Variables]

# Indicator variables are used to represent categorical variables in regression models. They take on values of 0 or 1 to indicate the presence or absence of a category.

data(utown)
price0bar <- mean(utown$price[which(utown$utown==0)])
price1bar <- mean(utown$price[which(utown$utown==1)])
mod5 <- lm(price~utown, data=utown)
b1 <- coef(mod5)[[1]]
b2 <- coef(mod5)[[2]]

plot(utown$utown, utown$price, 
     xlab = "Utown", ylab = "Price", col = "grey",
     main = "Price vs Utown with Indicator Variable")
abline(b1, b2, col = "blue", lwd = 2)  # Add regression line
abline(h = price0bar, col = "red", lwd = 2)  # Add horizontal line for price0bar
abline(h = price1bar, col = "green", lwd = 2)  # Add horizontal line for price1bar



# Enjoy the analysis and results!

