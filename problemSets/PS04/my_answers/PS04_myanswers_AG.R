# Name: Aryan Goyal
# Student number:18306046

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

#Importing the stargazer library to use for making tables in Latex
library(stargazer)
library(car)
# Set wd for current folder
getwd()
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# Read in data
data(Prestige)
help(Prestige)

# 1a) Creating a new variable professional by recoding the variable type
# with professionals = 1 and white collar/blue collar = 0

Prestige$professional <- ifelse(Prestige$type == "prof", 1, 0)

head(Prestige) # Inspecting the data and we can see our new variable

# 1b) running a linear model where Prestige is our outcome variable
# and income, professional and income X professional (interaction term) are the
# three explanatory variables

q1 <- lm(prestige~income+
                  professional+
                  income*professional, data=Prestige) 
summary(q1) 

stargazer(q1) #In order to get a table for LaTex

#Code from: https://www.statology.org/plot-multiple-linear-regression-in-r/
avPlots(q1) #add variable plot for multiple linear regression

# 1c) Prediction equation based on 1b)
# Prestige = 21.1422589 + (0.0031709*income) + (37.7812800*professional)- (0.0023257(income*professional))

# 1d) Interpretation of the coefficient for income

# The coefficient is significant at the 0.001 level. 
# Holding other explanatory variables constant, a 1 dollar increase in income
# leads to a 0.0031709 scale points increase in Prestige on average. 

# 1e) Interpretation of the coefficient of professional

# The coefficient is significant at the 0.001 level.
# Holding other explanatory variables constant, we can say that the
# prestige score for professionals is 37.7812800 scale points higher than 
# the prestige score for non-professionals on average

# 1f) What is the effect of a $1,000 increase in income on 
# prestige score for professional occupations? when profession = 1

# Equation 1 where the income is 1000
# Prestige = 21.1422589 + (0.0031709*1000) + (37.7812800*1) - (0.0023257(1000*1))
# = 59.7687389


# Equation 2 where income increases by 1000
# Prestige = 21.1422589 + (0.0031709*2000) + (37.7812800*1) - (0.0023257(2000*1))
# = 60.6139389

# Based on the 2 above equations, the difference between them represents the 
# change in y-hat associated with a 1000$ increase in income
# 60.6139389 - 59.7687389 = 0.8452

#Therefore, A 1000$ increase in income corresponds to a 0.8452 scale points increase in prestige
# based on our multiple linear regression model 

# Trying to do this in R

coefficients <- coef(q1)

# Find the coefficient of income and the interaction term for professional occupations
coefficient_income <- coefficients["income"]
coefficient_interaction <- coefficients["income:professional"]

# Calculate the change in predicted prestige for a $1,000 increase in income
change_in_prestige <- ((coefficient_income + coefficient_interaction) * 1000)

# Print the change in predicted prestige
print(change_in_prestige)
# We get the same value: 0.8452

# 1g) the marginal effect of professional
# jobs when the variable income takes the value of 6, 000

# Equation 1 (for non-professional)
# Prestige = 21.1422589 + (0.0031709*6000) + (37.7812800*0) -(0.0023257(6000*0))
# = 40.1676589

# Equation 2 (for professionals)
# Prestige = 21.1422589 + (0.0031709*6000) + (37.7812800*1) -(0.0023257(6000*1))
# = 63.9947389

# The difference between the two equations represents the change in y-hat
# 63.9947389 - 40.1676589 = 23.82708
# Therefore, for a person earning 6000$, prestige increases by 23.82708 scale points
# if the job is categorized as professional

# Trying to do this in R
data1g <- data.frame(income = 6000, professional = c(0, 1))
prestige1g <- predict(q1, newdata = data1g)
change1g <- diff(prestige1g)
print(change1g)

# Q2 

# Q2a) 

# In order to find the partial effect of a predictor variable on our 
# outcome variable, I use the t-test for estimated coefficients.

#My null hypothesis is that there is no discerible linear relationship 
# between having yard signs in a precinct and vote share 
#(after controlling for the effects of other predictor variables), 
# H0: Beta_j = 0
  
# My alternative hypothesis is that there is a discernible linear relationship 
# between the predictor variable and outcome variable 

# First, I calculate the test statistic using the values in 
# the provided regression results.

# t = (0.042)/(0.016)
# Therefore, t = 2.625
  
# Next, we determine the degrees of freedom, df = n - k - 1
# where n is the number of observations
# and k is the number of predictors (excluding the intercept)

# Therefore, df = 30 - 2 - 1 = 27

# Based on the degrees of freedom and our chosen significance level ($\alpha=0.05$), 
# I find the critical t-value

# Based on the t-distribution table, I find the two-tailed critical value to 
# be 2.05 at 27 degrees of freedom. 
# I used the table from this website: 
#  (https://faculty.washington.edu/heagerty/Books/Biostatistics/TABLES/t-Tables/)

# The absolute value of my test statistic, 2.625 is greater than the 
# critical value, 2.05. Therefore, we can reject the null hypothesis and 
# find support for the alternative that there is a discernible linear 
# relationship between precincts with yard signs and vote share

# Q2b)
# Similar Q2a) I conduct the t-test for estimated coefficients

# My null hypothesis is that there is no discernible linear relationship 
# between having yard signs in the adjacent precinct and vote share 
# (after controlling for the effects of other predictor variables)

# My alternative hypothesis is that there is discernible linear relationship 
# between the predictor variable and outcome variable
  
# Based on the provided regression results, I calculate the test statistic:
# t= 0.042/0.013
# t = 3.2308
        
# Next, we determine the degrees of freedom,
# df = n - k - 1
# df = 76 - 2 - 1
# df = 73
      
# Based on the degrees of freedom and our chosen significance level, 
# I find the critical t-value
      
# Based on the t-distribution table, I find the two-tailed critical value to be 
# 1.99 at 73 degrees of freedom.
      
# The absolute value of my test statistic, 3.2308 is greater than the 
# critical value, 1.99 at 73 degrees of freedom. 
# Therefore, we can reject the null hypothesis and 
# find support for the alternative that there is a discernible linear 
# relationship between being next to precincts with yard signs and vote share. 
      
# Answer 2c)

# I interpret the coefficient for the constant term substantively.
# The y-intercept represents the value of the outcome variable when all predictor variables are equal to zero. In this case, this means that our y-interecept estimates the vote share if there were 0 lawn signs put up in any precincts. This means that Ken Cuccinelli's vote share would be 0.302 scale points if no lawn signs were put up.

# On the basis of this, I calculate the percentage change in the 
# outcome variable associated with the treatment condition 
# compared to the baseline condition (represented by the constant term). 

# = (0.042/0.302) * 100$
# = 13.91%

# This value suggests that, on average, precincts where yard signs 
# were put up had a 13.91% higher vote share for 
# Cuccinelli compared to the baseline where no yard signs were put up.

# Answer 2d) 

# I evaluate the model fit for this regression using R-squared.
# A R-squared value of 0.094 is low and 
# suggests that the explanatory variables in the model 
# do not explain much of the variance in the outcome variable. 
# This R-squared value indicates that only 9.4% of the variance 
# in the outcome variable is explained by the explanatory variables 
# in our multiple linear regression model.  

# This is an indication that we should include other variables 
# in our model that could explain the variation in vote share 
# to a greater extent, other than whether precincts had yard signs.