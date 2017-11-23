################################## 
##### 01REAN Cviceni 8 ##########
#################################
#
# Todays exercise
# Hypothesis Testing
#  - Homoscedasticity / Heteroscedasticity
#  - Normality
#  - Linearity
# Transformations
#  - Polyinomial
#  - Logaritmic
#  - Box - Cox
# Generalized Linear Models

library(car)
library(lattice)
library(MASS)
library(ggplot2)
library(leaps)
library(ISLR)
library(ISLR)
library(graphics)
library(effects)

setwd("~/Studies/REAN/tutorials/08")  

# Back to simple car break to stop data used in Ex4
# Before transforming data - we need some suggestin from Testing or Residual plots
### Testing ### 

# Shape of dependence - linearity, polynomial regression
summary(lm(dist~speed,data=cars))             # intercept + linear
summary(lm(dist~speed^2 ,data=cars))          # intercept + quadrature
summary(lm(dist~speed+I(speed^2),data=cars))  # intercept + linear + quadrature
summary(lm(dist~poly(speed, degree=3, raw=TRUE),data=cars)) # polynomial regression till d. 3


# Repeating measurement - compare factor model 
# Test submodel with factor variable with less DF
with(cars,table(speed))
anova(m1_f<-lm(dist/speed~factor(speed),data=cars))
anova(m1_c<-lm(dist/speed~speed,data=cars))
anova(m1_c,m1_f)

# Homoscedasticity 
# for groups leveneTest, bartlett.test and fligner.test

# for dependence on mean value
# Breusch-Pagan test statistic
bptest(m1_c)
# Score Test for Non-Constant Error Variance
ncvTest(m1_c)

# Normality
library(nortest)
lillie.test(residuals(m1_c))   # Lilliefors test
shapiro.test(resid(m1_c))      # Shapiro-Wilk test
shapiro.test(residuals(m1_c))  # same

# Independence 
# Assume that data are measured from one car - step-by-step
# check if there is any independence (driver is tired, breaks abraded, ...)
durbinWatsonTest(m1_c)
plot(resid(m1_c)[-length(resid(m1_c))],resid(m1_c)[-1]) # no autocorelation


# OLS regression models assume the response variable to be (approximately) normal distributed.
# If non-normality of residuals appears, we can
#  -> transform data to obtain normality and use OLS
#  -> use GLM



###### Transformations ####################

# transforamtion in responses - no change in analysis

# Use SLID data from car package
? SLID
summary(SLID)
SLID <- na.omit(SLID) # remove NA's
summary(SLID)


mod0 <- lm(wages ~ sex + age + education, data=SLID)
summary(mod0)

# checking normality
op <- par(mfrow=c(1,2))
qq.plot(mod0, simulate=TRUE, line="none")
plot(density(rstudent(mod0)))
par(op)
# we can see heavy tails and problems with larger events
# try log transform

mod1 <- lm(log(wages) ~ sex + age + education, data=SLID)
# +(factor(language) neni signifikantni)
summary(mod1)


# compare models with and without log transform
op <- par(mfrow=c(2,2))
qq.plot(mod0, simulate=TRUE, line="none")
plot(density(rstudent(mod0)))
qq.plot(mod1, simulate=TRUE, line="none")
plot(density(rstudent(mod1)))
par(op)

#dev.off()
#graphics.off()


# checking constant error variance
op <- par(mfrow=c(2,2))
plot(fitted(mod0), rstudent(mod0), col="black",pch=".")
abline(h=0, lty=2)
lines(lowess(fitted(mod0), rstudent(mod0)))
spreadLevelPlot(mod0, col="black",pch=".")

plot(fitted(mod1), rstudent(mod1), col="blue",pch=".")
abline(h=0, lty=2)
lines(lowess(fitted(mod1), rstudent(mod1)))
spreadLevelPlot(mod1,col="blue",pch=".")
par(op)


# checking linearity
crPlots(mod1)


mod2 <- lm(log(wages) ~ sex + poly(age, degree=2, raw=TRUE) + I(education^2), data=SLID)
mod2 <- lm(log(wages) ~ sex + age + I(age^2) + I(education^2),    data=SLID) # same
summary(mod2)
crPlots(mod2)



library(effects)
plot(allEffects(mod1))
plot(allEffects(mod2))


dev.off()

# Box-Cox transformation

boxcox(mod0)
mod_bc <- update(mod0, . ~ . + boxCoxVariable(wages))
summary(mod_bc)
avPlots(mod_bc)  # constructed-variable plot


# Box-Tidwell regression
boxTidwell(log(wages) ~ I(age - 15) + I(education + 1), other.x=~sex, data=SLID)

mod_bt <- lm(log(wages) ~ I(age - 15) + I(education + 1) + sex
             + I((age - 15)*log(age - 15)) + I((education + 1)*log(education + 1)),
             data=SLID)
avPlots(mod_bt)  # constructed-variable plots
summary(mod_bt)


# non-constant error variance
ncvTest(mod0)
ncvTest(mod0, ~ age + education + sex, data=SLID)


##### Very short introduction to GLM ######
# more in 01ZLIM !!!

# Generalised linear models (GLM) provide a framework for the estimation of
# regression models with response variables from the exponential family.
# The GLM generalizes linear regression by allowing the linear model
# to be related to the response variable via a link function 


#### Logistic regression ####
#The response variable is binary (0x1, Yes x NO, ...)

# Use transformation of expected value of the response,
# to be able to model it by linear function of explanatory variables.

# Exponential distribution
mod_g1 <- glm(wages ~ sex + age + I(age^2) + I(education^2), family=Gamma(link = "inverse"),   data=SLID) # same
summary(mod_g1)
residP_mode_g1 <- residuals(mod_g1, type = "pearson")
residD_mode_g1 <- residuals(mod_g1, type = "devi")
plot(residP_mode_g1,residD_mode_g1) # tato rezidua nejsou stejne

opar<-par(mfrow = c(2,2))
plot(mod_g1)
par(opar)

#"response", "pearson", anscombe, deviance
library(surveillance)  # for Anscombe residuals
resid_mode_g1 <- anscombe.residuals(mod_g1, 0.1679705)

qqnorm(resid_mode_g1,pch=".")
qqline(resid_mode_g1)


options(digits=2)
confint(mod_g1, level = 0.95)
confint(mod_g1, level = 0.99)



#  logistic or logit transformation (one from many)
#logit(y) = log((y\(1-y))) = beta_0 + beta_1*x_1 + ... + beta_p*x_p.
#The logit of a probability is simply the log of the odds of the response taking the value one.

#generate Bernoulli probabilities from true model
x <-rnorm(100)
p<-exp(x)/(1+exp(x))

#one replication per predictor value
n <- rep(1,100)
#simulate response
y1 <- rbinom(100,n,p)
#fit model
mod_g21 <- glm(y1~x,family=binomial(link = "logit"))
#make quantile-quantile plot of residuals
qqnorm(residuals(mod_g21, type="deviance"))
abline(a=0,b=1)


#many replications per predictor value
n <- rep(30,100)
#simulate response
y2<-rbinom(100,n,p)   # popisuje uspechy a neuspechy
#fit model
mod_g22 <- glm(cbind(y2,n-y2)~x,family="binomial")
#make quantile-quantile plot of residuals
qqnorm(residuals(mod_g22, type="deviance"))
abline(a=0,b=1)

# insted of number of successes and number of faulures, we can use rates or weights





######## TODAY INDIVIDUAL EXERCISE #########

# Get Boston data from car package
head(Boston)
? Boston
summary(Boston)

# Model medv response as a function of lstat 
# 1) Try nonlinear transformations of the predictors  
#    - kvadratic
#    - polynomial 
#    - log 
#    - transform response variable too

# 2) Try to predict response chas as a function of other variables (crim, indus, medv, rad)
 # by GLM and logistic regression
dev.off()

opar<-par(mfrow = c(2,2))
model1 <- lm(medv ~ poly(lstat, degree=3, raw=T), data = Boston)
plot(model1)

model2 <- lm(medv ~ log(lstat), data = Boston)
plot(model2)

model3 <- lm(log(medv) ~ poly(lstat, degree=2, raw=TRUE), data = Boston)
crPlots(model3)
