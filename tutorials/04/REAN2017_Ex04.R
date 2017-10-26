################################## 
##### 01REAN Cviceni 4 ##########
#################################
#
# Todays exercise
# Explanation of summary in lm.models 
# Finish simple linear regression in R
# Start with multivariable linear regression in R


library(car)
library(ggplot2)

setwd("~/Studies/REAN/tutorials/04")   

# Task from the last exercise:
# Investigate a relationship between speed and stopping distance for cars
summary(cars)
View(cars)

# Show again what R can do in this simple regression problem and how to interpret results.

#plot data
plot(cars$speed,cars$dist)
#same plot
plot(cars$dist~cars$speed)
# tuned plot saved as plot_car.png into the working directory
png('plot_car.png')
plot(dist ~ speed, data = cars, xlim = c(0,30), ylim = c(0,130),pch=20, col = "red3", xaxs="i",yaxs="i",
main="Speed and Stopping Distances of Car (1920s)",xlab="Speed", ylab="Stopping Distance")
dev.off()
# tuned plot by ggplot 
plot_car2 <- ggplot(cars, aes(x=speed, y=dist)) + 
            geom_point(size=2, alpha=0.8) + 
            theme_bw() +
            xlab("Speed of car") + 
            ylab("Stopping distance") + 
            ggtitle("Speed and Stopping Distances of Car (1920s)")
ggsave('plot_car_2.png', plot_car2)


# linear models - simple regression


cars_lm1 <- lm(dist ~ -1 +speed , data = cars)
cars_lm2 <- lm(dist ~  speed , data = cars)
cars_lm3 <- lm(dist ~  -1 +I(speed^2) , data = cars)
cars_lm4 <- lm(dist ~  speed + I(speed^2) , data = cars)

summary(cars_lm1)
summary(cars_lm2)
summary(cars_lm3)
summary(cars_lm4)


plot(dist ~ speed, data = cars, xlim = c(0,30), ylim = c(0,130),pch=20, col = "black", xaxs="i",yaxs="i",
     main="Speed and Stopping Distances of Car (1920s)",xlab="Speed", ylab="Stopping Distance")
abline(cars_lm1, col ="blue4")
abline(cars_lm2, col ="red4")
#lines(sort(cars$speed), fitted(cars_lm3)[order(cars$speed)], col='green')
lines(seq(0, 30, 0.5), predict(cars_lm3,data.frame(speed = seq(0, 30, 0.5))), col='blue')
lines(seq(0, 30, 0.5), predict(cars_lm4,data.frame(speed = seq(0, 30, 0.5))), col='red')
legend("topleft",legend = c("linear","intercept + linear",
  "quadratic","intercept + linear + quadratic"),lty = c(1,1,1,1),col = c("blue4","red4","blue","red"))


# what can we obtain from lm object (almost all)

coef(cars_lm2) # estimated coefficenta
              # \hat{\mu}(dist) = -17.58 + 3.93(speed).
              # We estimated average stopping distance (in feet) for a car going (speed) mph.
# interpretation of the slope
#    Estimated slope \hat{\beta}_2 = 3.93 represents the increase in average stopping
#    distance for each mile per hour faster that the car drives.
# interpretation of the intercept
#     Estimated intercept \hat{\beta}_1 = -17.58 represents the mean stopping distance 
#     for a car traveling 0 mph (make no sence in our example !!!)
#     Extrapolating can be dangerous and can lead to  nonsensical results.

dim(cars) # 50 pozorovani
fitted(cars_lm2) # all fitted response variables, regressors are the same we used in the estimation
cars[1:5,]
hat_dist2a = coef(cars_lm2)[1] + coef(cars_lm2)[2]*cars[1:5,"speed"]   # manually from the definition
hat_dist2b = fitted(cars_lm2)[1:5]                                     # by R function
cbind(hat_dist2a, hat_dist2b)

# if we want to predict response with new data (first 5 are same as in X)
predict(cars_lm2, newdata = data.frame(speed = c(4, 4, 7, 7, 8, 1, 30)))

# residuals
residuals(cars_lm2)
resid_2a =cars[1:5,"dist"] - hat_dist2a   # manually from the definition
resid_2b = residuals(cars_lm2)[1:5]       # by R function
cbind(resid_2a, resid_2b)                  

# Mean Square Error and Standard Error
n = dim(cars)[1]
p = dim(cars)[2] - 1
MSE_2a = sum(residuals(cars_lm2)^2)/(n-p-1) # Mean square error  - manually from the definition
sigma_hat2a = sqrt(MSE_2a)                  # Residual standard error (estimation of standard deviation)
sigma_hat2b = summary(cars_lm2)$sigma       # by R function
cbind(sigma_hat2a, sigma_hat2b)   
sigma_hat = sigma_hat2a
# compare to summary(cars_lm2)


# Interval Estimates of the Parameters
coef(cars_lm2)
confint(cars_lm2)
conf_int = as.data.frame(cbind(confint(cars_lm2)[,1], coef(cars_lm2), confint(cars_lm2)[,2]))
names(conf_int) = c("lower CI 2.5 %", "estimate", "upper CI 97.5 %"  )
conf_int
summary(cars_lm2)$coef
#summary(cars_lm2)$coefficients

# another confidence interval - 99%
confint(cars_lm2, level = 0.99)


# t-test of significance of parameters
Sb_1 = summary(cars_lm2)$coef[1,2]
LCI_1 = coef(cars_lm2)[1]+qt(.975, df = n-1)*Sb_1
UCI_1 = coef(cars_lm2)[1]+qt(.025, df = n-1)*Sb_1

Sb_2 = summary(cars_lm2)$coef[2,2]
UCI_2 = coef(cars_lm2)[2]+qt(.975, df = n-1)*Sb_2
LCI_2 = coef(cars_lm2)[2]+qt(.025, df = n-1)*Sb_2

conf_int_manually = rbind( c(LCI_1,coef(cars_lm2)[1],UCI_1), c(LCI_2,coef(cars_lm2)[2],UCI_2))
conf_int_manually = as.data.frame(conf_int_manually)
names(conf_int_manually) = c("lower CI 2.5 %", "estimateed val.", "upper CI 97.5 %"  )
row.names(conf_int_manually) = c("Intercept","Speed")
conf_int_manually



# t-test of significance of parameters
# H0 hypothesis is beta_i = 0
beta_H0 = 0
tval_1 = (summary(cars_lm2)$coef[1,1]-beta_H0)/summary(cars_lm2)$coef[1,2]
tval_2 = (summary(cars_lm2)$coef[2,1]-beta_H0)/summary(cars_lm2)$coef[2,2]

pval_1 = 2*pt(abs(tval_1), cars_lm2$df.residual, lower.tail = FALSE)
pval_2 = 2*pt(abs(tval_2), cars_lm2$df.residual, lower.tail = FALSE)
coef_manually = rbind( c(coef(cars_lm2)[1], Sb_1, tval_1,pval_1),
                       c(coef(cars_lm2)[2], Sb_2, tval_2,pval_2))
coef_manually
#comapre with 
summary(cars_lm2)$coef


#  Confidence intervals vs. Prediction intervals 

# Confidence intervals tell us about how well we have determined the mean. 
# Prediction intervals tell us  where we can expect to see the next data point sampled.

new_speed =data.frame(speed = seq(0,30,0.1))
new_conf = predict(cars_lm2, newdata = new_speed, interval = "confidence")
new_pred = predict(cars_lm2, newdata = new_speed, interval = "prediction")
plot(dist ~ speed, data = cars, xlim = c(0,30), ylim = c(0,130),pch=20, col = "black", xaxs="i",yaxs="i",
     main="Speed and Stopping Distances of Car (1920s)",xlab="Speed", ylab="Stopping Distance")
lines(seq(0, 30, 0.1), new_pred[,1], col='black')
lines(seq(0, 30, 0.1), new_pred[,2], col='red')
lines(seq(0, 30, 0.1), new_pred[,3], col='red')
lines(seq(0, 30, 0.1), new_conf[,2], col='blue')
lines(seq(0, 30, 0.1), new_conf[,3], col='blue')
legend("topleft",legend=c("observed","fit","Confidence int","Prediction int"),
       pch=c(20,NA,NA,NA),lty = c(NA,1,1,1),col = c("black","black","blue","red"))

# faster - works only for simple linear regression (lm(y ~ x))
library(HH)
ci.plot(cars_lm2)


######
## Residual analysis############
#####

#####
# Normality of residuls 

# Shapiro-Wilk test
shapiro.test(residuals(cars_lm2))
# Kolmogorov-Smirnov test
ks.test(residuals(cars_lm2),"pnorm", 0, sigma_hat2a)             # problem
ks.test(unique(residuals(cars_lm2)),"pnorm", 0, sigma_hat2a)     # problem solved, but bad approach, don't do it !!!!

library(nortest)
# Lilliefors test
lillie.test(residuals(cars_lm2))
# Anderson-Darling test 
ad.test(residuals(cars_lm2))

######
# Homoscedasticity -  Constant Variance Assumption
library(lmtest)

# Breusch-Pagan test statistic
bptest(cars_lm2)

# Score Test for Non-Constant Error Variance
ncvTest(cars_lm2)
# spreadLevelPlot(cars_lm2) 



#dev.off()
#graphics.off()

opar <- par(mfrow=c(2,2))
plot(cars_lm2)
par(opar)

# QQplot
qqnorm(residuals(cars_lm2))
qqline(residuals(cars_lm2), col = 2)

#residuals vs. fitted
plot(residuals(cars_lm2) ~ fitted(cars_lm2))

# standartized residuals vs. fitted
plot(rstandard(cars_lm2) ~ fitted(cars_lm2))

# Cook distance 
cooks.distance(cars_lm2)


# we need hat matrix - the rest will be covered next time.






# 1) ###############
# Go through whole exercise - use quadratic model instead of linear.
# What is different? 


# 2) ################
# Use again trees data
View(trees)
summary(trees)


#Questions:
#
# Try to find best linear model that describe how the Volume depends on Girth 
# plot data with estimated regression curves
# compute adn plot confidence and prediction intervals
# validate model - investigate residuals

# 3) ################
# add second regressor Height
# find again the best linear model fit Girth
# validate this model
# add interaction + BMI information computed during the first lesson

## Question 3 will be discussed next lesson in details



trees_lm1 <- lm(Volume ~ -1 +Girth , data = trees)
trees_lm2 <- lm(Volume ~  Girth , data = trees)
trees_lm3 <- lm(Volume ~  I((Girth)^2) , data = trees)
trees_lm4 <- lm(Volume ~  Girth + I(Girth^2) , data = trees)

summary(trees_lm3)

opar <- par(mfrow=c(2,2))
plot(trees_lm3)
par(opar)

opar <- par(mfrow=c(1,1))
par(opar)
plot(Volume ~ Girth, data = trees, xlim = c(5,25), ylim = c(0,100),pch=20, col = "black", xaxs="i",yaxs="i",
     main="Trees",xlab="Girth", ylab="Volume")
abline(trees_lm1, col ="blue4")
abline(trees_lm2, col ="red4")
lines(seq(5, 25, 0.5), predict(cars_lm3,data.frame(speed = seq(5, 25, 0.5))), col='blue')
lines(seq(5, 25, 0.5), predict(cars_lm4,data.frame(speed = seq(5, 25, 0.5))), col='red')


new_Girth =data.frame(Girth = seq(5,25,0.1))
new_conf = predict(trees_lm3, newdata = new_Girth, interval = "confidence")
new_pred = predict(trees_lm3, newdata = new_Girth, interval = "prediction")
plot(Volume ~ Girth, data = trees, xlim = c(5,25), ylim = c(0,100),pch=20, col = "black", xaxs="i",yaxs="i",
     main="Trees",xlab="Girth", ylab="Volume")
lines(seq(5, 25, 0.1), new_pred[,1], col='black')
lines(seq(5, 25, 0.1), new_pred[,2], col='red')
lines(seq(5, 25, 0.1), new_pred[,3], col='red')
lines(seq(5, 25, 0.1), new_conf[,2], col='blue')
lines(seq(5, 25, 0.1), new_conf[,3], col='blue')
legend("topleft",legend=c("observed","fit","Confidence int","Prediction int"),
       pch=c(20,NA,NA,NA),lty = c(NA,1,1,1),col = c("black","black","blue","red"))













#cov(fsdata$father,fsdata$son)/(sd(fsdata$father))^2


