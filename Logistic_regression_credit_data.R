#Sergio L. Marrugo
#Logistic regression with Credit_card data, June 2024

setwd("C:/Users/EQUIPO/Desktop/py4e")
library(dplyr)

credit_data <- read.csv("dataset.csv", header = T, strip.white = T)
#Gender data needs to be F or M 
credit_data[credit_data$Gender == 0,]$Gender <- "M"
credit_data[credit_data$Gender == 1,]$Gender <- "F"
credit_data$Gender <- as.factor(credit_data$Gender)

credit_data$Family_status <- as.character(credit_data$Family_status)
group_by(credit_data, Family_status) %>% count()
# replace civil marriage to Marriage, turn to factors
credit_data[credit_data$Family_status == "Civil marriage",]$Family_status <- "Married"
credit_data$Family_status <- as.factor(credit_data$Family_status)

#Turn the binomial variables used in the logistic regression by SKlearn into
#Factors

credit_data$Target <- as.factor(credit_data$Target)
credit_data$Own_car <- as.factor(credit_data$Own_car)
credit_data$Unemployed <- as.factor(credit_data$Unemployed)
credit_data$Own_property <- as.factor(credit_data$Own_property)

#useful code for checking NAs and position
nrow(credit_data[is.na(credit_data$Target)|is.na(credit_data$Total_income),])
credit_data[is.na(credit_data$Target)| is.na(credit_data$Total_income),]
#data balance example
xtabs(~Target + Family_status, credit_data)

logistic <- glm(Target ~ Gender + Family_status + Own_car + Own_property + Unemployed +
                  Total_income, Age, Years_employed, data = credit_data, family="binomial")
summary(logistic)

#Lets make a new model using ONLY the significant variables from the previous model

logistic2 <- glm(Target ~ Gender  + Total_income + Own_car,
                 data = credit_data, family="binomial")
summary(logistic2)
# The AIC diminishes considerably, however, none of the variables are significant