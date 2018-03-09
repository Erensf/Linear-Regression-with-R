# Assignment #5 

setwd("~/Desktop/Ancile Training/Data")

data <- read.csv("rossmann_sales.csv")
library(dplyr)

# how many unique variables 
sapply(data, function(x) length(unique(x)))
# sum of na values 
sapply(data,function(x) sum(is.na(x)))

# if NA values, put mean 
# data$x[is.na(data$x)] <- mean(data$x,na.rm=TRUE)

# convert the date format so that R can understand 
data$date <- as.Date(data$date, format='%m/%d/%Y')
library(lubridate)
# create new columns to capture more information on sales 
data$month_year <- month(data$date)
data$week_of_month <- ceiling(day(data$date)/7)



#Level 1
# ●	Build multiple linear regression model to predict sales of
# a store given number of customers, open, promo, state holiday, school holiday, day of week
# week of month and month of year

# select only store==1 to perform analysis 
model.data <- filter(data, store==1)
train <- select(model.data,"sales",
                "customers",
                "day_of_week",
                "month_year",
                "week_of_month",
                "promo",
                "open",
                "school_holiday",
                "state_holiday"
                )
                

# names(train)[3:ncol(train)]

cols <- c("day_of_week",
            "month_year",
            "week_of_month",
            "promo",
            "open",
            "school_holiday",
            "state_holiday")

train[cols] <- lapply(train[cols],factor)
str(train)


model <- lm(sales ~. , data=train)
summary(model)

plot(model)

#●	What are the variables that are statistically significant in the model ?
# ANSWER: customers and promo are statistically significant

#●	What is R Square and Adjusted R square of the model ? 
# ANSWER: They are same, 0.9805 

#Level 2
#●	What happens to R Square as you remove outliers ? Why ?
#●	What happens to R Square as you increase variables ? Why ?
# Answer: R square tells how well a regression model predicts responses for new observations. 
# R square has increased when I increase explanatory variables. 
#●	What happens to Adjusted R Square as you increase the number of variables ?
# Answer: The adjusted R-squared compares the explanatory power of regression models that contain different numbers of predictors.
# The adjusted R-squared is a modified version of R-squared that has been adjusted for the number of predictors in the model
# Adjusted R square has also increased when I increase explanatory variables. 
# A model with more predictors(explainatory variables) may appear to have a better fit simply because it has more predictors 

#Level 3
#●	What is predicted sale of a store when 1000 customers visit when store is open, when there is promotion, when it is a state holiday and school holiday on Aug5,2015. Use the model constructed above.
# Answer: 8808.728 

#●	Do feature selection/ggvariable selection using stepwise selection algorithm

test <- data.frame( "sales"=0,
                    "customers"=1000,
                    "day_of_week"=1,
                    "month_year"=1,
                    "week_of_month"=1,
                    "promo"=1,
                    "open"=1,
                    "school_holiday"=1,
                    "state_holiday"="a")

col <- c(  "day_of_week",
           "month_year",
           "week_of_month",
           "promo",
           "open",
           "school_holiday",
           "state_holiday")
test[col] <- lapply(test[col],factor)

str(test)

temp1 <-rbind(train,test)
test <- temp1[943,]
predict(model, test)

