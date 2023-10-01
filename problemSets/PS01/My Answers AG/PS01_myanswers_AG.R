#####################
# load libraries
# set wd
# clear global .envir
#####################
getwd()
setwd("C:/Users/Lenovo/Documents/GitHub/StatsI_Fall2023/problemSets/PS01/My Answers AG")
getwd()
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

lapply(c("stringr"),  pkgTest)

#####################
# Problem 1
#####################

y <- c(105,69,86,100,82,111,104,110,87,108,87,90,94,113,112,98,
        80,97,95,111,114,89,95,126,98)

length(y) #checking the sample size
mean(y) #central tendency, mean
sd(y) #standard deviation(the square root of the sum of squared deviations divided by n-1)
## the standard deviation helps us gauge the spread of the data

##the standard error describes how the mean varies from sample to sample
standard_error <- sd(y)/sqrt(25) 
  
##The confidence interval formula is mean(y-bar) +- t-score(se) 
##where se = standard deviation of y divided by the square root of the sample size

##using the code for confidence intervals from Tutorial 2
t_score <- qt(0.95, df=length(y)-1) 
lower_90_t <- mean(y)-(t_score)*(standard_error) 
upper_90_t <- mean(y)+(t_score) *(standard_error)
mean <- mean(y)

## Q1 Part 2
##First, I look at the assumptions. The small sample size
## means I use the t test and not the z-test
##Next, I state my null and alternate hypotheses
## Null hypothesis:  mu <= 100
## Alternative hypothesis: mu > 100

##I use a one tailed t-test as we are trying to find out if the mean IQ is greater than 100,
##a two-tailed test would be better suited if we only wanted to see if there was a difference in means

##Next, I calculate the test statistic
##This formula is from Ch.6 of Agresti and Finlay
test_statistic <- (mean(y)-100)/(sd(y)/sqrt(length(y)))

##Next, I calculate the p-value
##Code from:https://cosmosweb.champlain.edu/people/stevens/webtech/R/Chapter-9-R.pdf
P_value <- pt((test_statistic), df = 24, lower.tail = TRUE)

##The same is calculated using the t.test function from Tutorial 2 to double check
t.test(y, mu = 100, alternative = 'less')

##As the p-value is greater than the alpha level of 0.05, we
##do not have enough evidence to reject the null hypothesis
## that the mean in the school is less than/equal to 100
  
#####################
# Problem 2
#####################

expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2023/main/datasets/expenditure.txt", header=T)

##Line of best fit code  from https://www.statology.org/line-of-best-fit-in-r/
##Plot code from Tutorial 2 and 3
plot(expenditure$Y,expenditure$X1,
     xlab="Per capita expenditure on shelters/housing assistance in state",
     ylab="Per capita personal income in state",
     main="The Relationship between expenditure on shelters and per capital personal income")
abline(lm(expenditure$X1 ~ expenditure$Y),col='blue',lty='dashed')
cor(expenditure$Y,expenditure$X1)
text(50, 2500,sprintf("Correlation=%s", round(cor(expenditure$X1,expenditure$Y),4)))

plot(expenditure$Y,expenditure$X2,
     xlab="Per capita expenditure on shelters/housing assistance in state",
     ylab="”Financially insecure” residents in state per 100,000",
     main="The Relationship between expenditure 
     on shelters and No. of residents that are financially insecure")
abline(lm(expenditure$X2 ~ expenditure$Y),col='blue',lty='dashed')
cor(expenditure$Y,expenditure$X2)
text(50, 450,sprintf("Correlation=%s", round(cor(expenditure$Y,expenditure$X2),4)))

plot(expenditure$Y,expenditure$X3,
     xlab="Per capita expenditure on shelters/housing assistance in state",
     ylab="No. of people per 1000 residing in urban areas in state",
     main="Relationship between expenditure on shelters 
     and no of people per 1000 residing in urban areas")
abline(lm(expenditure$X3 ~ expenditure$Y),col='blue',lty='dashed')
cor(expenditure$Y,expenditure$X3)
text(50, 800,sprintf("Correlation=%s", round(cor(expenditure$Y,expenditure$X3),4)))


plot(expenditure$X1,expenditure$X2,
     xlab="Per Capita personal income in state",
     ylab="”Financially insecure” residents in state per 100,000",
     main="Relationship between per capital personal income
     and ”Financially insecure” residents in state per 100,000")
abline(lm(expenditure$X2 ~ expenditure$X1),col='blue',lty='dashed')
cor(expenditure$X1,expenditure$X2)
text(1200, 450,sprintf("Correlation=%s", round(cor(expenditure$X1,expenditure$X2),4)))


plot(expenditure$X1,expenditure$X3,
     xlab="Per Capita personal income in state",
     ylab="No. of people per 1000 residing in urban areas in state",
     main="Relationship between per capita personal income 
     and People per 1000 residing in urban areas")
abline(lm(expenditure$X3 ~ expenditure$X1),col='blue',lty='dashed')
cor(expenditure$X1,expenditure$X3)
text(1200, 800,sprintf("Correlation=%s", round(cor(expenditure$X1,expenditure$X3),4)))

plot(expenditure$X3,expenditure$X2,
     xlab="No. of people per 1000 residing in urban areas in state",
     ylab="”Financially insecure” residents in state per 100,000",
     main="Relationship between no. of people residing in urban areas
     and ”Financially insecure” residents per 100,000")
abline(lm(expenditure$X2 ~ expenditure$X3),col='blue',lty='dashed')
cor(expenditure$X3,expenditure$X2)
text(380,450,sprintf("Correlation=%s", round(cor(expenditure$X3,expenditure$X2),4)))


##Part2
boxplot(expenditure$Y ~ expenditure$Region,
        main="Boxplot of per capita expenditure on shelters 
        by region",
        ylab="Per capita expenditure on shelters",
        xlab="Region",
        names=c("Northeast","North Central","South","West"))
means <- tapply(expenditure$Y, expenditure$Region, mean)
points(means, pch=20) 

##The black dots in the boxplots indicate the mean of each region.
##Based on this, the West region has the highest per 
##capita expenditure on housing assistance
## The code for adding means in boxplot is from: https://www.statology.org/boxplot-with-mean-in-r/

##In addition to the boxplot, I also calculate the mean of the four regions
##To do this, I used the code from:https://www.statology.org/r-mean-by-group/
aggregate(expenditure$Y, list(expenditure$Region), FUN=mean)
Group_means <- aggregate(expenditure$Y, list(expenditure$Region), FUN=mean)
Group_means

##On the basis of this, we find that Region 4 has the highest mean of 88.3
##followed by Region 2 of 83.92

##Part 3

##I took the help of this website:https://r-coder.com/scatter-plot-r/#:~:text=The%20scatterplot%20function%20in%20R,-An%20alternative%20to&text=In%20order%20to%20customize%20the,parameters%20of%20the%20corresponding%20estimates..
##Using the car package, I made a scatterplot 
##and by using the ?scatterplot instruction to learn about the customizations possible
##Using this, I made the graph look more legible and clean
## This forum also helped: https://stackoverflow.com/questions/52876568/how-to-use-legend-argument-in-scatterplot-using-car-package-version-3

library(car)
?scatterplot
scatterplot(expenditure$X1 ~ expenditure$Y| 
    expenditure$Region,
    regLine=TRUE,smooth=FALSE, grid=FALSE,
    legend=c(title="Region",coords="topleft"),
    main="The relationship between per capita personal income
    and per capita expenditure on shelters by region",
    xlab="Per capita personal income in state",
    ylab="Per capita expenditure on shelters in state")
