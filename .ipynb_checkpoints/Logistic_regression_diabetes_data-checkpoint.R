#Logistic regression prevalence of diabetes
#Sergio L. Marrugo
#June 2024

setwd("C:/Users/EQUIPO/Desktop/py4e")
library(dplyr)
library(flexplot)
library(ggplot2)

#upload the imputed diabetes table that we created in the jupyter notebook
diabetes_data <- read.csv("Diabetes_imputed.csv", header = T, strip.white = T)
diabetes_data <- diabetes_data %>% select(!BloodPressure & !SkinThickness & !BMI)

#Transform binary data into factors
diabetes_data$Outcome <- as.factor(diabetes_data$Outcome)
#Run the regression with all the variables
all_vars <- glm(Outcome ~ ., data = diabetes_data, family="binomial")
summary(all_vars)

#Residuals have a binomial distribution
residuales <- as.data.frame(all_vars$residuals)

ggplot(data = residuales, aes(x = `all_vars$residuals`))+
  geom_histogram(bins = 200)

#variables: Pregnacies, BMI, Glucose, DiabetesPedigreeFunction.
#Are statistically significant (in the all vars models) AIC 737.52, Insulin is marginally significant

#Let's calculate a pseudo R-squared to have an idea of the effect size
all_vars_ll.null <- all_vars$null.deviance/-2
all_vars_ll.proposed <- all_vars$deviance/-2
R_sq_all <- (all_vars_ll.null - all_vars_ll.proposed)/ all_vars_ll.null # about 0.26 which is considered a large effect size
#p-value for R-squared in all_vars model (p-value is expremely small!)
pvall<- 1-pchisq(2*(all_vars_ll.proposed-all_vars_ll.null), df =(length(all_vars$coefficients)-1))

#Let's try a new model with the significant variables
s_vars <- glm(Outcome ~ Pregnancies + BMI_na + Glucose + DiabetesPedigreeFunction,
              data = diabetes_data, family = "binomial")
summary(s_vars)

#All variables are still significant! AIC slightly better 735.11
#R-squared
s_vars.null <- s_vars$null.deviance/-2
s_vars.proposed <- s_vars$deviance/-2
R_sq_svars <- (s_vars.null - s_vars.proposed)/ s_vars.null

pvall_svars <- 1-pchisq(2*(s_vars.proposed-s_vars.null), df =(length(s_vars$coefficients)-1))
visualize(s_vars)

#Let's go back to python and build predictive logistic models with sklearn using the significant variables
