# BTMA 531 Assignment 1
# Cooper Chung (Redacted Student ID)
# cooper.chung1@ucalgary.ca

# Question 1a
object000 <- seq(100, 10000, 25)  # Stores a list of numbers from 100 to 10,000 in intervals of 25 into object000
object000[29]                     # Retrieves the number from this list at index 29: 800. The answer to this command is 800


# Question 1b
ls()          # Lists out all objects in the global environment 
rm(object000) # Removes the specified object from the global environment


# Question 1c
# The first step would be to install the package "tidytext" by using install.packages("tidytext")
# The next step would be to load the tidytext package using library(tidytext)
# Once the library is loaded, you can use the functions that come with it


# Question 1d
set.seed(531) # Set seed for reproducibility

obs <- 25     # Assign parameters used in matrix here
mean <- 10
stdev <- 3
min <- 1
max <- 20

variable1 <- rnorm(obs, mean, stdev)  # Create the variables here
variable2 <- runif(obs, min, max)

matrix1 <- matrix(c(variable1, variable2), ncol = 2)  # Create a matrix with 2 columns using the variables
sd(matrix1[,1]) # First column gives us a standard deviation of 3.155124, which is expected because this is near the number that we specified it to have
sd(matrix1[,2]) # Second column gives us a standard deviation of 5.555766. This is also expected because if we calculate this by hand -
# the Variance is (20 - 1)^2 / 12 = 30.08. Square rooting this gives us a standard deviation of 5.48.


# Question 1e
# Create a scatterplot using the two columns from the matrix. Also gives the plot a title with axes labels
plot(matrix1[,2], matrix1[,1], xlab = "Variable2", ylab = "Variable1", main = "Scatterplot - Variable1 and Variable2")

cor(matrix1[,2], matrix1[,1]) # Gives us a correlation between the two columns of -0.1992592.
# This number, while negative, is close to 0. Therefore, this suggests a weak to no correlation -
# between the two columns, leaning slightly to negatively correlated but not significant.


# Question 2a
install.packages("ISLR")  # Install required package
library(ISLR)             # Load required package

ncol(Carseats)            # Using ncol tells us how many columns (variables) are in this dataset. In this case, there are 11 variables.

sapply(Carseats, class)   # Using sapply to apply the Class function on the Carseats dataset can tell us the class of each variable.
# In this case, we can see that ShelveLoc, Urban, and US are listed as "factor", meaning they are categorical.
# For the Carseats dataset, we have 8 quantitative, and 3 qualitative variables.


# Question 2b
# The variable of interest I would like to predict is Sales. One relevant question I could ask is if sales have an impact on the amount of sales generated for a specific
# product. More specifically, at what price do consumers refuse to buy a car seat, and at what price do consumers always buy a car seat. Another relevant question
# I would ask is if advertising significantly increases sales or not. More specifically, does an increase in money spent advertising correlate with an increase in sales?
# Once final question I would ask is if the community income level affects sales. More specifically, if at a certain income level they will either forego buying one,
# and if at a certain level they will always buy one. One peripheral question I would ask at first is if any of these variables are correlated to eachother to begin with.


# Question 3
data("Carseats")                                      # Load the Carseats dataset
CarMedium <- subset(Carseats, ShelveLoc == "Medium")  # Take a subset of the Carseats dataset where the ShelveLoc is "Medium"
mean(CarMedium$Advertising)                           # Take the average of the advertising column in the subset of data. This results in an average of 6.538813

CarBad <- subset(Carseats, ShelveLoc == "Bad")        # Take a subset of the Carseats dataset where the ShelveLoc is "Bad"
mean(CarBad$Advertising)                              # Take the average of the advertising column in the subset of data. This results in an average of 6.21875


# Question 4a
# This creates a scatterplot with the Advertising and Sales on the x and y axes respectively.
plot(Carseats$Advertising, Carseats$Sales, xlab = "Advertising", ylab = "Sales", main = "Scatterplot - Advertising vs Sales")


# Question 4b
abline(lm(Carseats$Sales ~ Carseats$Advertising)) # This function adds a linear estimate to the graph by fitting a linear model to
# the variables mentioned.


# Question 4c
cor(Carseats$Advertising, Carseats$Sales) # This function finds the correlation between the two variables specified. In this case, we get a correlation of 0.2695068
# While this number is positive, it is also near 0, suggesting a weak relationship. There certainly IS a relationship,
# but this relationship is weak, trending in the same general direction.


# Question 4d
# No, we cannot state that price causes sales to increase/decrease. This is because we did an analysis on advertising and sales, and the correlation alone
# is not enough to say that one causes the other. Analysis like this would require more than just one metric, especially since correlation does not necessarily
# imply causation. Based on the plot we created, we can also see that the relationship between these variables may not be linear either.


# Question 5a
shelve_loc_freq <- table(Carseats$ShelveLoc)  # This creates a frequency table from the ShelveLoc column in the Carseats dataset
shelve_loc_freq                               # Taking a look at the results, we can see that there are 96 occurrences for Bad, 85 for good, and 219 for medium

urban_freq <- table(Carseats$Urban)           # This creates a frequency table from the Urban column in the Carseats dataset
urban_freq                                    # Taking a look at the results, we can see that there are 118 occurrences for No, and 282 for Yes


# Question 5b
par(mfrow = c(1, 2))                          # This function takes parameters to assist in displaying graphs. The mfrow parameter in particular helps with
# putting 2 graphs on the same row

barplot(urban_freq, main = "Urban")           # Graph a bar chart for the Urban frequencies

barplot(shelve_loc_freq, main = "ShelveLoc")  # Graph a bar chart for the ShelvLoc frequencies


# Question 5c
par(mfrow = c(1, 1))  # Return this back to default from the previous question 
hist(Carseats$CompPrice, xlab = "CompPrice", ylab = "Frequency", main = "CompPrice Histogram")  # Creates a histogram with the CompPrices on the x axis, and
# the frequencies on the y axis


# Question 6a
# Before this question starts, make sure the required file is in your working directory :)
install.packages("data.table")  # Install the "data.table" package
library(data.table)             # Load the data.table library
setwd("D:/Desktop/A1")          # Set my working directory

AdData <- read.csv("ad.csv")    # Read the ad csv once using the default read function
AdData <- fread("ad.csv")       # Read the ad csv once using the fread function from the data.table library


# Question 6b
fit_tv_sales <- lm(AdData$sales ~ AdData$TV)  # Creates a linear regression model using the required variables. Predicting sales with TV ads
summary(fit_tv_sales)                         # Gives us a summary of the model. We can see that under the Estimates column, the coefficient is 0.047537.
# With this number, we can conclude that TV ads has little to no impact on sales.
# We can also see that the intercept is 7.032594. Therefore, to mathematically predict when TV = 145 - 
# we just do 7.032594 + 0.047537(145) = 13.92546. 13.92546 is our value for Sales if TV = 145

prediction_sales <- fit_tv_sales$coefficients[[1]] + (fit_tv_sales$coefficients[[2]] * 145) # From this, we get the same number 13.92546
prediction_sales


# Question 6c
# This creates a multiple linear regression model using the specified variables.
fit_tv_rad_news_sales <- lm(AdData$sales ~ AdData$TV + AdData$radio + AdData$newspaper)
summary(fit_tv_rad_news_sales)


# Question 6d
qqnorm(fit_tv_rad_news_sales$residuals) # Creates a normality plot of residuals from the linear regression model
qqline(fit_tv_rad_news_sales$residuals) # Adds a reference line. The closer to the line the points are, the closer the residuals follow a normal distribution
# From this graph, we can see that near 0, the residuals somewhat follow a normal distribution. However, as the quantities
# go further to the extreme positives or extreme negatives, the residuals no longer resemble a normal distribution


# Question 7a
# This function creates a multiple linear regression using the specified variables
fit_wage_yr_ag_hi_h <- lm(wage ~ year + age + health_ins + health, data = Wage)
summary(fit_wage_yr_ag_hi_h)  # Gives us a summary of the multiple linear regression


# Question 7b
# This creates a linear model with the variables specified. Since the required variables were already categorical, I did not need to modify them
predict_wage <- fit_wage_yr_ag_hi_h$coefficients[[1]] +
  (fit_wage_yr_ag_hi_h$coefficients[[2]] * 2006) +
  (fit_wage_yr_ag_hi_h$coefficients[[3]] * 40) +
  fit_wage_yr_ag_hi_h$coefficients[[4]] +
  fit_wage_yr_ag_hi_h$coefficients[[5]]

predict_wage  # From this, we can estimate that a person with the specified characteristics has a wage of 97.46517


# Question 8a
# From a business point of view, the Sales variable is the desired outcome.


# Question 8b
# This dataset excels in capturing data about the characteristics of the customers. This dataset also incorporates some aspects of geographical data which can
# assist in generating regional insights. An aspect that this dataset is missing is more store-specific data, any historical data regarding sales of the
# car seats, and product specific data that distinguish one car seat from another. Any aspects of seasonality may not be reflected in the current data.


# Question 8c
# Along with all the questions that I have mentioned in Question 2b, some more questions would be if there is a significant difference in the sales between
# urban and non-urban stores. This can also assist in determining store locations, and some potential profits to be gained/saved. As stated before,
# one general question I would ask is if there is any sort of seasonality.


# Question 9e
# This pie chart may be useful for library managers as it shows them the number of enrollment per instructor. This can be helpful as it can assist in knowing
# how much of one instructors materials to order or have on hand. It can help with discovering trends, and being prepared for the years to come.
# This can also help with keeping track of costs, and making better use of school money.