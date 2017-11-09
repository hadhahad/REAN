################################## 
##### 01REAN Cviceni 6 ##########
#################################
#
# Todays exercise
# Model Selection
# Post-Hoc Analysis
# Residual Analysis and Diagnostic Tools
# Cook’s Distance
library(lattice)
library(MASS)
library(car)
library(ggplot2)
library(leaps)
library(ISLR)
library(car)

setwd("D:/Vyuka/REGA/2017/REAN2017_Ex06/")  

Advert <- read.table("Advert.csv", sep =",", header = T )
head(Advert)
summary(Advert)

model1 <- lm(sales ~ TV*radio*newspaper, data=Advert)
summary(model1)

model2 <- lm(sales ~ TV*radio, data = Advert)
summary(model2)

pairs(Advert)

model_step1 <- step(model1)
summary(model_step1)

model_step2 <- stepAIC(model1)
model_step2 <- stepAIC(model1, direction = "both")
summary(model_step2)
model_step2$anova

anova(model_step1,model2)

model_step3 <- leaps(model1)

leapsstatCp <- leaps(x=Advert[,2:4], y=Advert[,5], names=names(Advert)[2:4], method="Cp")
CpParams <- leapsstatCp$size
CpValues <- leapsstatCp$Cp
plot(CpValues ~ CpParams)

leapsstatr2 <- leaps(x=Advert[,2:4], y=Advert[,5], names=names(Advert)[2:4], method="r2")
r2Params <- leapsstatr2$size
r2Values <- leapsstatr2$r2
plot(r2Values ~ r2Params)

leapsstatadjr2 <- leaps(x=Advert[,2:4], y=Advert[,5], names=names(Advert)[2:4], method="adjr2")
adjr2Params <- leapsstatadjr2$size
adjr2Values <- leapsstatadjr2$adjr2
plot(adjr2Values ~ adjr2Params)

leaps<-regsubsets(sales~TV*radio*newspaper,data=Advert,nbest=10)
summary(leaps)
plot(leaps,scale="Cp")
# plot statistic by subset size
library(car)
subsets(leaps, statistic="cp") 

model <- lm(sales ~ TV, data=Advert)
summary(model)

plot(sales~TV*radio, data=Advert, xlab="TV", ylab="Sales", main="Dependance of Sales on TV&Radio advertising")
sales_new <- data.frame(TV = seq(0,300,1))
conf_new  <- predict(model, newdata = sales_new, interval = "confidence")
pred_new  <- predict(model, newdata = sales_new, interval = "prediction")
lines(seq(0,300,1), pred_new[,1], col='black')
lines(seq(0,300,1), pred_new[,2], col='red')
lines(seq(0,300,1), pred_new[,3], col='red')
lines(seq(0,300,1), conf_new[,2], col='blue')
lines(seq(0,300,1), conf_new[,3], col='blue')

# Advertising data displays sales (in thousands of units) for a particular product
# as a function of advertising budgets (in thousands of dollars) for TV, radio, and newspaper media.
# Solve following problems and questions 

# Display Advertising data - make several plots

# Try to find "best" model to explain how can advertising influence sales

# Use AIC, BIC, C_2, R^2, Adj-R^2, F-test to choose your model.
# Start with full model with all interactions, or with null model with intercept only
# Display AIC, BIC, C_2 statistics in dependence on number of parameters

# Validate you final model - normality, homoscedasticity, outliers


# With final model answer following questions:
#  - Is there a relationship between advertising budget and sales?
#  – Which media contribute to sales, i.e. do all three media—TV, radio, and newspaper—contribute to sales?
#  – Which media generate the biggest boost in sales?
#  - How strong is the relationship between advertising budget and sales?
#  – How much increase in sales is associated with a given increase in TV advertising?
#  – How much increase in sales is associated with a given increase in Radio advertising?
#  - How accurately can we estimate the effect of each medium on sales?
#  - How accurately can we predict future sales?
#  - Is there synergy among the advertising media?
#  - Imagine you have 100k $, what is the best strategy how to spend it in advertising?
#  - How much more pruduct will we sell, if we spend 10k$ in TV and 20k$ in radio advertising?
#  - What is the 95% confidence interval of previous question?








