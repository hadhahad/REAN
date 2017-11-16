################################## 
##### 01REAN Cviceni 7 ##########
#################################
#
# Outliers
# Multicolinearity

library(lattice)
library(MASS)
library(car)
library(leaps)
require(ISLR)
require(graphics)

setwd("~/Studies/REAN/tutorials/07") 
set.seed(4242)


#Simulate data Y = beta1 + X2*beta2  + X3*beta3  + X4*beta4 + e
# e ~ N(0,4)

n=100
p=4
X1    = rep(1, times=n)
X2    = rnorm(n,20,3)
X3    = 10+rexp(n,0.1)
X4    = 5+rbinom(n,15,0.2)
beta0 = matrix(c(5,3,2,-5),nrow =4 , ncol = 1)
e     = rnorm(n,0,4)
X0    = cbind(X1,X2,X3,X4)
Y     = X0%*%beta0 + e
X     = cbind(X2,X3,X4)

# Create data.frame
data0 = data.frame(cbind(Y,X))
names(data0) = c("Y","X2","X3","X4")
summary(data0)
pairs(data0)

# LM model 0
m0 = lm(Y ~ X)
summary(m0)
op <- par(mfrow=c(2,2))
plot(m0)
par(op)





# Add good outlying point
X_0 = rbind(X,c(max(X[,1])+20,max(X[,2])+30,max(X[,3])+20))
Y_0 = cbind(rep(1, times=(n+1)),X_0)%*%beta0
plot(Y_0~X_0[,2],xlab = "X",ylab = "Y", main = "Simple Regression with at least one influential point")

m_0 = lm(Y_0 ~ X_0[,2])
summary(m_0)
op <- par(mfrow=c(2,2))
plot(m_0)
par(op)  

plot(Y_0~X_0[,2],xlab = "X",ylab = "Y", main = "Simple Regression with at least one influential point")
abline(m_0)

# Let's play with this point
# Change its Y value
# Change its X value
#.....

X_0 = rbind(X,c(max(X[,1])+20,max(X[,2])+30,max(X[,3])+20))
Y_0 = rbind(Y,100)
plot(Y_0~X_0[,2],xlab = "X",ylab = "Y", main = "Simple Regression with at least one influential point")

m_0 = lm(Y_0 ~ X_0[,2])
summary(m_0)
op <- par(mfrow=c(2,2))
plot(m_0)
par(op)  

plot(Y_0~X_0[,2],xlab = "X",ylab = "Y", main = "Simple Regression with at least one influential point")
abline(m_0)



# Add more leverages in more regressors         
X_1 = X
X_1[c(11,21,31),]
X_1[11,1] = 3*X_1[11,1]
X_1[21,2] = 3*X_1[21,2]
X_1[31,3] = 5*X_1[31,3]

m1 = lm(Y ~ X_1)
summary(m1)
op <- par(mfrow=c(2,2))
plot(m1)
par(op)


# Add more outliers in response 

Y_2 = Y
Y_2[c(12,22,32)]
Y_2[12] = 3*Y_2[12]
Y_2[22] = -3*Y_2[22]
Y_2[32] = 5*Y_2[32]

m2 = lm(Y_2 ~ X)
summary(m2)
op <- par(mfrow=c(2,2))
plot(m2)
par(op)



# Model with 3 leverages and 3 outliers

m3 = lm(Y_2 ~ X_1)
summary(m3)
op <- par(mfrow=c(2,2))
plot(m3)
par(op)

#plot(Y_2~X_1[,2],xlab = "X",ylab = "Y", main = "Simple Regression with at least one influential point")
#dX_1 = cbind( seq(min(X_1[,1]), max(X_1[,1]),length=100),
#             seq(min(X_1[,2]), max(X_1[,2]),length=100),
#              seq(min(X_1[,3]), max(X_1[,3]),length=100))
#dX_1  = as.data.frame(dX_1)
#names(dX_1) = c("X1","X2","X3")
#dY_1 = predict(m3,dX_1)
#lines(dX_1[,2],dY_1)




# influence.measures
# produces a class "infl" object tabular display showing 
# the DFBETAS for each model variable, DFFITS, covariance ratios, Cook's distances
# and the diagonal elements of the hat matrix.
summary(influence.measures(m1))
summary(influence.measures(m2))
summary(influence.measures(m3))

#dfbeta displays effect on coefficients of deleting each observation in turn
dfbeta(m3)
dfbetaPlots(m3)

#dfbetas displayseffect on coefficients of deleting each observation in turn,
#  standardized by a deleted estimate of the coefficient standard error
dfbetas(m3)
dfbetasPlots(m3)

# dffits statistics
dffits(m3)
sort(dffits(m3))

# covratio statistics
covratio(m3)
sort(covratio(m3))

#standardized residuals (Normovana rezidua)
rstandard(m3)
sort(rstandard(m3))

# Studentized residuals (studentizovana rezidua)
rstudent(m3)
sort(rstudent(m3))

# cooks distance
cooks.distance(m3)
sort(cooks.distance(m3),decreasing = T)






# Collinearity - Multicollinearity

# RIDGE REGRESSION
# We'll use Ridge regression function - ridge.lm() from MASS package



# Generating the data
set.seed(558562316)
n    = 50      # Sample size
X1   = rnorm(n,10,1)
X2   = rnorm(n,20,2)
X3   = runif(n)-1
X3c  = X1 + X3           # New variable - collinearity
X3cc = X1 + 0.5*X2 +X3   # New variable - multicollinearity
e    =rnorm(n,0,4)
Y    = X1 + X2 + e 


#A simple way to detect collinearity is to look at the correlation matrix of the predictors.
cor(cbind(X1,X2,X3,X3c,X3cc))
# plot pairs
pairs(as.data.frame(cbind(Y,X1,X2,X3,X3c,X3cc)))


# OLS fit of 3-variable model using independent x3
Mc1  <- lm(Y~ X1 + X2 + X3)
summary(Mc1)


# OLS fit of 3-variable model using correlated x3i.
Mc2  <- lm(Y~ X1 + X2 + X3c)
summary(Mc2)
# compare with scaled model
scaleMc2  <- lm(scale(Y)~ scale(X1) + scale(X2) + scale(X3c))
summary(scaleMc2)

# Compute variance inflation factor
vif(lm(Y~ X1 + X2 + X3c))
vif(lm(Y~ X1 + X2))
vif(lm(Y~ X2 + X3c))
summary(lm(Y~ X1 + X2 + X3c))
summary(lm(Y~ X1 + X2 ))
summary(lm(Y~  X2 + X3c))

# compute Condition number ( index podmíněnosti )
# from lm object - not recommended
kappa(lm(scale(Y)~ scale(X1) + scale(X2) + scale(X3c)))^2
kappa(lm(scale(Y)~ scale(X1) + scale(X2)))^2
# from regression matarix
kappa(scale(cbind(X1,X2,X3c)),exact=T)^2
kappa(cor(cbind(X1,X2,X3c)),exact=T)
kappa(cor(cbind(X1,X2)),exact=T)


piMulticol(lm(Y~ X1 + X2 + X3c))


# Ridge regression using independent variables
ridge <- lm.ridge(Y ~ X1+X2+X3, lambda = seq(0, .1, .001))
summary(ridge)
select(ridge)
plot(ridge)


# Ridge regression using correlated variables
ridgec <- lm.ridge(Y ~ X1+X2+X3c, lambda = seq(0, .1, .001))
plot(ridgec)
select(ridgec)



# Selection of constant is at endpoint.  Extend endpoint and try again
ridgec <- lm.ridge (Y ~ X1+X2+X3c, lambda = seq(0, 1, .1))
plot(ridgec)
select(ridgec)
ridgec <- lm.ridge (Y ~ X1+X2+X3c, lambda = seq(0, 10, .1))
plot(ridgec)
select(ridgec)


# Final model uses lambda=0.4
ridge.final <- lm.ridge (Y ~ -1 + X1+X2+X3c, lambda = 0.4)
ridge.final
summary(ridge.final)

#  There's no predict() method for "ridgelm" objects




# Multicollinearity and ridge regression again ()

X1 <- rnorm(20,10,1)
X2 <- rnorm(20,mean=X1,sd=.01)
Y  <- rnorm(20,mean=5+X1+X2)
lm(Y~X1+X2)$coef
lm.ridge(Y~X1+X2,lambda=1)





# PCA - simple ilustration
?USArrests
head(USArrests)
dim(USArrests)

pairs(USArrests, panel = panel.smooth, main = "USArrests data")

?prcomp

prcomp(USArrests)                   # inappropriate
prcomp(USArrests, scale = TRUE)     # appropriate, due to vary by orders of magnitude
prcomp(~ Murder + Assault + Rape, data = USArrests, scale = TRUE)
plot(prcomp(USArrests))
summary(prcomp(USArrests, scale = TRUE))
biplot(prcomp(USArrests, scale = TRUE))



# Today: Exercise


Credit <- read.table("Credit.csv", sep =",", header = T )
Credit_out <- read.table("Credit_out.csv", sep =",", header = T )

? Credit
#A simulated data set containing information on ten thousand customers. The aim here is to predict
#which customers will default on their credit card debt.

#A data frame with 10000 observations on the following 4 variables.
#ID        Identification
#Income    Income in $10,000’s
#Limit     Credit limit
#Rating    Credit rating
#Cards     Number of credit cards
#Age       Age in years
#Education Number of years of education
#Gender    A factor with levels Male and Female
#Student   A factor with levels No and Yes indicating whether the individual was a student
#Married   A factor with levels No and Yes indicating whether the individual was married
#Ethnicity A factor with levels African American, Asian, and Caucasian
#Balance   Average credit card balance in $

summary(Credit)
dim(Credit)

summary(Credit_out)
dim(Credit_out)

# A) zpracujte data bez "umelych outlieru"
# 
# 1) Zobrazte data a diskutujte mozne vlivne promenne na odezvu "balance"
# 2) Naleznete vhodny model
# 3) Diskutujte vliv promennych Limit a Rating
# 4) Lze Limit nahradit pomoci Raing, nebo mit obe vlivne promenne v modelu zaroven?
# 5) Spoctete index podminenosti vysledne regresni matice
#    Spoctete VIF pro dany model obsahujici obe zmine promene, nebo jen jednu znich
# 6) Vyzkousejte hrebenovou regresi
# 7) Overte predpoklady finalniho modelu


# A) zpracujte data Credit_out s "outliery"
# 1) vyskytuji se v modelu nejaka rezidua?
 #  - pouzijete vsechny mozne zpusoby analyzy - leverage, cook, dfbeta, dffit, rstudent, ...

# for example ...
pairs(Credit)
pairs(Credit[,c(2:7,12)])
step(lm(Balance ~ (.), data=Credit[,c(2:12)]))
summary(lm(Balance ~ Student + Limit, data=Credit))

model1 <- lm(Balance ~ Income + Limit, data = Credit)
model2 <- lm(Balance ~ Limit, data = Credit)
model1_out <- lm(Balance ~ Income + Limit, data = Credit_out)
model2_out <- lm(Balance ~ Income, data = Credit_out)
op <- par(mfrow=c(2,2))
plot(model1)
plot(model2)
plot(model1_out)
plot(model2_out)
par(op)

s <- step(lm(Balance ~ -1 + Income + Limit + Rating, data = Credit), direction="both")

summary(s)
s$anova

library(leaps)
summary(Credit)
dim(Credit)
leapsstatCp <- leaps(x=Credit[,2:4], y=Credit[,12], names=names(Credit)[2:4], method="Cp")
CpParams <- leapsstatCp$size
CpValues <- leapsstatCp$Cp
plot(CpValues ~ CpParams)

leaps<-regsubsets(Balance~Income + Rating + Limit,data=Credit,nbest=10)
summary(leaps)
plot(leaps,scale="Cp")
subsets(leaps, statistic="bic") 
dev.off()

model_final <- lm(Balance ~ Income + Rating, data = Credit)
op <- par(mfrow=c(2,2))
plot(model_final)

