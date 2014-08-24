##############################################################################################################
# Author: Wayward-Economist
# Description: This script corresponds to the markdown document "Course_Project". 
#              It includes the code for the course project for Coursera's Regression Models course.
# Last Modified: 8/18/14
##############################################################################################################

## ---- libraries ----
## Load the required libraries into R.
require(stats); require(graphics); require(ggplot2)
require(grid); require(gridExtra); require(corrplot)
require(glmnet); require(car); require(usdm)
require(boot)

## ---- load_and_process_data ----
## Load the data into the current enviroment; this is done for personal preference.
mtcars <- mtcars
str(mtcars)

## ---- create_mpg_charts ----
## This chunk creates the exploritory chunks for the miles per gallon variable.
chart.1 <- ggplot(mtcars, aes(x = mpg)) + 
  geom_histogram(aes(y = ..density..), binwidth = 1, colour = "black", fill = "white") +
  geom_density(alpha = .2, fill = "red") + 
  ggtitle("Distribution of miles per Gallon") +
  xlab("Miles per Gallon") + ylab("Density")

chart.2 <- ggplot(mtcars, aes(x = log(mpg))) + 
  geom_density(alpha = .2, fill = "red") + 
  ggtitle("Density of Ln(Miles per Gallon)") +
  xlab("ln(Miles per Gallon)") + ylab("Density")

## ---- explore_indy_variables ----
## Conduct some exploritory analysis on the 
summary(mtcars)
pairs.plot <- pairs(mtcars, panel = panel.smooth, main = "Pairwise Plots: mtcars data")
cors <- cor(mtcars)
cor.plot <- corrplot(cors)

## ---- explore_multicolinearity ----
## Examine multicolinearilty in the data.
vif(mtcars[, -1])

## ---- lasso_regression ----
## Use a lasso regression in order to select variables.
x <- model.matrix(log(mpg) ~ ., mtcars)
y <- mtcars$mpg
grid       <- 10 ^ seq(10, -2, length = 100)
lasso.cv   <- cv.glmnet(x, y, alpha = 1)
lasso.fit  <- glmnet(x, y, alpha = 1, lambda = grid)
lasso.coef <- predict(lasso.fit, type = "coefficients", s = lasso.cv$lambda.min)
lasso.coef

## ---- more_exploritory_analyis ----
## 
pairs.plot.2 <- pairs(mtcars[, c(1, 2, 4, 6, 9)], panel = panel.smooth, main = "Pairwise Plots: mtcars subset")
vif(mtcars[, c(2, 4, 6, 9)])

## ---- process_data
## convert numeric variables to factor.
mtcars$cyl  <- as.factor(mtcars$cyl)
mtcars$vs   <- as.factor(mtcars$vs)
mtcars$am   <- as.factor(mtcars$am)
mtcars$gear <- as.factor(mtcars$gear)
mtcars$carb <- as.factor(mtcars$carb)

## ---- linear_model ----
## Estimate the relationship between the miles per gallon and the selected covariates.
glm.fit <- glm(log(mpg) ~ cyl + hp + wt + am, data = mtcars)
summary(glm.fit)

## ---- regression_diagnostics ----
## Examine the linear regression preformed above.
chart.3 <- qplot(predict(glm.fit), glm.fit$residuals)
chart.4 <- qplot(mtcars$cyl, glm.fit$residuals)
chart.5 <- qplot(mtcars$hp, glm.fit$residuals)
chart.6 <- qplot(mtcars$wt, glm.fit$residuals)
chart.7 <- qplot(mtcars$am, glm.fit$residuals)

## ---- nested_model_selection ----
## This section analyzes several other models. 
lm.fit.1 <- lm(log(mpg) ~ cyl + hp + wt + am, data = mtcars)
lm.fit.2 <- lm(log(mpg) ~ cyl + hp + wt * am, data = mtcars)
lm.fit.3 <- lm(log(mpg) ~ cyl + hp + wt * am + I(wt ^ 2), data = mtcars)
lm.fit.4 <- lm(log(mpg) ~ cyl + hp + wt * am + I(wt ^ 2) * am, data = mtcars)
anova(lm.fit.1, lm.fit.2, lm.fit.3, lm.fit.4)

## ---- cross_validation ----
## Preform leave-one-out-cross-validation to check for the MSE.
glm.LOCV <- cv.glm(mtcars, glm.fit)



