# Applied Statistical Analysis I      
# Tutorial 4: Bivariate regression, inference & prediction                     

# Get working directory
getwd()

# Set working directory 
setwd("C:/Users/Lenovo/Documents/GitHub/StatsI_Fall2023/")
getwd()

#############################
### RECAP Chi-square test ###
#############################

# Research questions: Is there a relationship between
# movie genre and rating?

# Load data
df <- readRDS("datasets/movies.rds")
View(df)

# Dataframe subsetting: df[rows, columns]
df_s <- df[df$genre=="Comedy" |
             df$genre=="Drama" |
             df$genre=="Documentary", ]
df_s$genre <- droplevels(df_s$genre)
View(df_s)

# Run Chi squared test
chisq.test(df_s$genre, 
           df_s$critics_rating)

# Check p-value
sprintf("%.20f",1.097e-12) #to print out all the decimal points

# Step 1: Assumptions
# Step 2: Hypotheses
# Step 3: Test statistic
# Step 4: P-value
# Step 5: Conclusion

### Look at standardized residuals ###

# Save chi-square test in object
chi_test <- chisq.test(df_s$genre, df_s$critics_rating)

# List objects inside chi_test
ls(chi_test)
chi_test$observed # f_o (observed frequencies) --> under the alternate hypothesis(?)
chi_test$expected # f_e (expected frequencies under the assumption of H0,
# under the assumption that two variables are independent) 
## Need to this manually probably and then can use this to double check

# Pearson residuals, 
# (observed - expected) / sqrt(expected)
chi_test$residuals ##Shortcut to check residuals
##If we have large errors, we show there is a high discrepancy between expected and observed
## So high error is good. This gives us an indiciation that we are likely to reject the
## null hypothesis

##Careful in problem set. Stdized residuals are calculated differently.
## Look at Jeff's slides for how these are calculated manually
# **Standardized** residuals,
# (observed - expected) / sqrt(V), where V is the residual cell variance
chi_test$stdres  

# How can we interpret the standardized residuals? 
## High errors in both residuals test so we can reject the null hypothesis

# Agenda 
# (a.) Correlation
# (b.) Bivariate regression 

# Research questions: 
# Is there a relationship between education and income?

# (a.) Correlation -----

# Load data 
df <- read.csv("datasets/fictional_data.csv")
View(df)

# Scatter plot 
plot(df$income,df$edu)

# Calculate correlation
cor(df$income,df$edu)

# Add to scatter plot
text(1200, 7, sprintf("Correlation=%s", round(cor(df$income,df$edu),4)))

# Improve visualization and save
png(file="tutorials/04/scatter_plot.png")
plot(df$income,
     df$edu,
     xlab="Monthly net income (in Euro)",
     ylab="University level education (in years)",
     main="The Relationship between education and income") 
text(1200, 8, sprintf("Correlation=%s", round(cor(df$income,df$edu),4)))
dev.off()

# t-test for the correlation coefficient
cor.test(df$income, df$edu)

# Check p-value
sprintf("%.20f",7.52e-07)

# Step 1: Assumptions ##Continuous variables for correlation
# Step 2: Hypotheses ##Null hypothesis: the two variables are not correlated/associated
##Alternate: The two are correlated/associated
# Step 3: Test statistic
# Step 4: P-value
# Step 5: Conclusion ##P value is below 0.05 cutoff and is highly significant, so we
##reject the null and we find evidence in favor of the correlation between education and income
## Based on our plot, we see that this correlation is positive

# (b.) Bivariate regression  -----

# Fit linear regression model
summary(lm(df$income~df$edu)) 
summary(lm(income~edu, data=df)) ##this is better to enter multiple IVs in case of multivariate regression
##PR thingy is the p-value
##Intercept --> the value of y when x = 0 --> expected income in this case of smeone with 0 university years
##Slope (edu) --> On average, for every 1 unit increase in x (uni years),there is a 250 euro increase in income
## Remember to mention "on average" --> it is a model which is an average effect
## Most of the time, the intercept does not make sense depending on the variable X
## Then we dont interpret the intercept
# Save model as object
model <- lm(income~edu, data=df)

# t-test for the slope of a regression line
summary(model)
250.64/33.06 ##Slope divided by std. error to get t value

# Check p-value
sprintf("%.20f",2.17e-06)

# Step 1: Assumptions ##Both variables are continuous. In linear regression, the output is always continuous (?)
# Step 2: Hypotheses #Null hypothesis, x does not have an effect on y. Slope is 0
# Step 3: Test statistic
# Step 4: P-value
# Step 5: Conclusion #We reject the null hypothesis as p value is 
##below the cutoff, and we can assume that education has an effect on income.
##We cannot say anything about the strength of the relationship. 
##As we have not controlled for other variables, we can just say
##there is an association between X and Y

# Confidence intervals 
confint(model, level=0.95)
confint(model, level=0.99)

# Plot
plot(x=df$edu, y=df$income) # Scatter plot
abline(model) # Add regression line

# Step by step
plot(x=df$edu, y=df$income) # Scatter plot
abline(v=4)  # Either specify single value (v for vertical)
abline(976.16, 250.64) # Or intercept and slope
abline(model) # Use intercept and slope in model object
abline(model, col="red") # Change color

# What is the prediction equation?
summary(model)
# income_pred = 976.16 + 250.64 * education

# Make predictions for first observation in df
head(df)
976.16 +  250.64 * 1 # predicted outcome
model$fitted.values
1520 - (976.16 +  250.64 * 1) # error
model$residuals

# Make predictions for a range of x values
predict(model, 
        newdata=data.frame(edu = seq(min(df$edu), max(df$edu), by=1)))

# Step by step
predict(model) # Predicted outcomes
model$fitted.values # Predicted outcomes
unique(df$edu) # Unique values of x (university years)
seq(min(df$edu), max(df$edu), by=1) # Specify a sequences for which
# predictions are to be returned
predict(model, newdata=data.frame(edu = seq(min(df$edu), max(df$edu), by=1)))

# Add standard errors
predict(model, newdata=data.frame(edu = c(0,1,2,3,4,5,6,7,8)))
predict(model, newdata=data.frame(edu = c(0,1,2,3,4,5,6,7,8)), se.fit=TRUE) ##with the se.fit

# Make predictions with **confidence intervals**
# Predict an average response at any chosen value of x
predict(model, newdata=data.frame(edu = c(0,1,2,3,4,5,6,7,8)), interval="confidence", level=0.95)

# Make predictions with **prediction intervals**
# Predict an individual’s response at any chosen value of x 
predict(model, newdata=data.frame(edu = c(0,1,2,3,4,5,6,7,8)), interval="prediction", level=0.95)
# more variability in individual responses --> wider intervals

# Make predictions for x values not in data
predict(model, 
        newdata=data.frame(edu = mean(df$edu))) # Mean education
mean(df$edu)
unique(df$edu) # Unique values of x
predict(model, newdata=data.frame(edu = 9)) # **But don't extrapolate**

# Plot predictions

plot(x=df$edu, y=df$income) # Scatter plot
points(df$edu, model$fitted.values, # Add another scatter plot on top
       col="green")

# Plot, regression line with confidence intervals
# Adopted from: https://stackoverflow.com/questions/46459620/plotting-a-95-confidence-interval-for-a-lm-object

# Save confidence intervals
ci <- predict(model, newdata=data.frame(edu = seq(min(df$edu), max(df$edu),by=1)), interval="confidence", level=0.95)
plot(df$edu, df$income) # Scatter plot
abline(model) # Add regression line
# Add lower bound
lines(seq(min(df$edu), max(df$edu),by=1), ci[,2], col="gray")
# Add upper bound
lines(seq(min(df$edu), max(df$edu),by=1), ci[,3], col="gray")

# Step by step
ci <- predict(model, newdata=data.frame(edu = seq(min(df$edu), max(df$edu),by=1)), interval="confidence", level=0.95)
ci # Save confidence intervals in object
# Dataframe subsetting: df[rows, columns]
ci[,2] # second column, lower bound, lwr
ci[,3] # third column, upper bound, upr

# Improve visualization and save
png(file="tutorials/04/reg_plot.png")
plot(df$edu,
     df$incom,
     xlab="Monthly net income (in Euro)",
     ylab="University level education (in years)",
     main="The Relationship between education and income")
abline(model) # Add regression line
# Add confidence intervals
lines(seq(min(df$edu), max(df$edu),by=1), ci[,2], col="gray")
lines(seq(min(df$edu), max(df$edu),by=1), ci[,3], col="gray")
# Add legend
legend(0, 3000, # x and y position of legend
       legend=c("Predictions", "95% Confidence intervals"),
       col=c("black","gray"),
       pch=1) 
dev.off()


