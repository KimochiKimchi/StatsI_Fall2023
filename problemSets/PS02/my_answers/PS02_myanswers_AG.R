##First I set my working directory
getwd()
setwd("C:/Users/Lenovo/Documents/GitHub/StatsI_Fall2023/")

rm(list=ls())

##Importing relevant libraries
library(DescTools)
library(stargazer)
##Q1: Calculate the chi-square test statistic "by hand" in R

##First,I check the assumptions that we have two categorical variables and if
##the expected frequency is greater than/equal to 5 in all cells

##Creating a data frame for our variable
##Code from: https://sparkbyexamples.com/r-programming/select-rows-with-row-names-in-r-2/#:~:text=By%20default%2C%20row%20names%20are,names%20use%20colnames()%20function.
Not_Stopped <- c(14,7)
Bribe_Requested <- c(6,7)
Stopped_Given_Warning <- c(7,1)

PolSci <- data.frame(Not_Stopped,Bribe_Requested,Stopped_Given_Warning,
                     row.names = c('Upper Class','Lower class'))
str(PolSci)

##Next, I calculate the expected frequency for each row and column
##Formula to calculate expected frequencies:((Row total)*(Column Total))/Total sample size
(27*21)/42 # = 13.50
(15*21)/42 # = 7.50
(27*13)/42 # = 8.36
(15*13)/42 # = 4.64
(27*8)/42 # = 5.14
(15*8)/42 # = 2.86

##I have all the required values to find the chi-square test statistic value
##The formula is ((observed frequency-expected frequency)^2)/expected frequency
ChiSquareStatistic <- (((14-13.5)^2)/13.5) + (((6-8.36)^2)/8.36) + (((7-5.14)^2)/5.14) + (((7-7.5)^2)/7.5) + (((7-4.64)^2)/4.64) + (((1-2.86)^2)/2.86)
##On the basis of this, I find that my chi-square test statistic is 3.80

##For part b), I calculate the p-value from the test statistic
##degrees of freedom=(rows-1)*(columns-1)
##Hence, df=(2-1)(3-1)=1*2=2

pvalue <- pchisq(3.80,df=2, lower.tail=FALSE)
##P-value = 0.15. This value is not significant at the alpha level=0.05
## Hence, we cannot reject our null hypothesis
##Add better conclusion

##We can double check our "by hand" chi square test using the chisq.test function in R
chisq.test(PolSci)
##Saving chi-square test in an object
chi_testR <- chisq.test(PolSci) 
##From this, I get chi squared value = 3.79 and p-value = 0.15

str(chi_testR)

##I check the structure to look at the values required to calculate standardized residuals

##Finally, I calculate the standardized residuals for each cell
##The formula for each cell is (observed frequency - expected frequency)/se
##where standard error= sqrt(expected frequency(1-row proportion)(1-column proportion))

##Calculating the standardized residual of each cell by hand:
##For Cell 1:
numerator1 <- 14-13.5 #Numerator
denominator1 <- sqrt((13.5*(1-0.64)*(1-0.5)))
cell1 <- numerator1/denominator1
##For Cell2:
numerator2 <- 6-8.36
denominator2 <- sqrt((8.36*(1-0.64)*(1-0.31)))
cell2 <- numerator2/denominator2
##For Cell3: 
numerator3 <- 7-5.14
denominator3 <- sqrt((5.14*(1-0.64)*(1-0.19)))
cell3 <- numerator3/denominator3  
##For Cell4:
numerator4 <- 7-7.5
denominator4 <- sqrt((7.5*(1-0.36)*(1-0.5)))
cell4 <- numerator4/denominator4
##For Cell5:
numerator5 <- 7-4.64
denominator5 <- sqrt((4.64*(1-0.36)*(1-0.31)))
cell5 <- numerator5/denominator5
##For Cell6:
numerator6 <- 1-2.86
denominator6 <- sqrt((2.86*(1-0.36)*(1-0.19)))
cell6 <- numerator6/denominator6

##To check these values, I extract the standardized residuals that R's chisq.test function calculates:
ls(chi_testR)

StdRes <- chi_testR$stdres ##storing the standardized residuals in an object
stargazer(chi_testR$expected)
stargazer(StdRes) ##Using the stargazer function, to get code for making a table in Latex

##Interpretation of std residuals

##Q2
##First I input the dataset for this question
Economics <- read.csv("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv")

##a)Null and alternative hypothesis for bivariate regression

##Q2 b)Bivariate regression in R
##Using the lm function
reg <- lm(Economics$water ~ Economics$reserved, Economics) #the $sign is to specifically use the variables we are interested in
reg
summary(reg)

confint(reg) # using the confint function to get confidence intervals for the constant and coefficient estimate
names(reg)
reg$coefficients

stargazer(reg)
stargazer(confint(reg)) #In order to make a table in Latex

##c) Interpret the coefficient estimate for reservation policy
