################################## 
##### 01REAN Cviceni 9 ##########
#################################
#
# Todays exercise
# Robust regression - M, MM, LTS
#
setwd("~/Studies/REAN/tutorials/09")  

# Load data used in this exercise
library(robustbase)
library(MASS)
library(car)


#Use Hertzsprung-Russell Diagram Data of Star Cluster CYG OB1
? starsCYG
summary(starsCYG)
attach(starsCYG)
#png('starsCYG.png')
plot(log.light ~ log.Te,xlim=rev(range(log.Te)+c(-0.1,0.1)),ylim = c(3.5,6.5), pch=20, col = "red3", xaxs="i",yaxs="i",
     main="Hertzsprung-Russell Diagram Data",xlab="SURFICE TEMPERATURE", ylab="LUMINIOUS OUTPUT")
#dev.off()


# Simple linear regression by OLS

#cst <- covMcd(starsCYG)
OLS_stars <- lm(log.light ~ log.Te)
opar<-par(mfrow = c(2,2))
plot(OLS_stars)
par(opar)
summary(OLS_stars)


plot(log.light ~ log.Te,xlim=rev(range(log.Te)+c(-0.1,0.1)),ylim = c(3.5,6.5), pch=20, col = "red3", xaxs="i",yaxs="i",
     main="Hertzsprung-Russell Diagram Data",xlab="SURFICE TEMPERATURE", ylab="LUMINIOUS OUTPUT")
abline(OLS_stars)
points(log.Te[log.Te<3.7],log.light[log.Te<3.7],pch = 22, col = "blue", lwd =2)


c(1:length(log.Te))[log.Te<3.7]
sort(OLS_stars$residuals)
outl <- c(1:length(log.Te))[log.Te<3.7]

OLS_stars2 <- lm(log.light[-outl] ~ log.Te[-outl])
opar<-par(mfrow = c(2,2))
plot(OLS_stars2)
par(opar)
summary(OLS_stars2)

plot(log.light ~ log.Te,xlim=rev(range(log.Te)+c(-0.1,0.1)),ylim = c(3.5,6.5), pch=20, col = "red3", xaxs="i",yaxs="i",
     main="Hertzsprung-Russell Diagram Data",xlab="SURFICE TEMPERATURE", ylab="LUMINIOUS OUTPUT")
points(log.Te[log.Te<3.7],log.light[log.Te<3.7],pch = 22, col = "blue", lwd =2)
points(log.Te[7],log.light[7],pch = 22, col = "cyan", lwd =2)



# Robust regression by M-estimator
# rlm(formula, data,..., method = c("M","MM")) 
# M estimate with diferent types of weight functions psi (derivative of rho)
# MM estimate - S estimate + M estimate 

# Summary plot for starsCYG
M_huber_stars    <- rlm(log.light ~ log.Te, method="M", psi = psi.huber)
M_hampel_stars   <- rlm(log.light ~ log.Te, method="M", psi = psi.hampel)
M_bisquare_stars <- rlm(log.light ~ log.Te, method="M", psi = psi.bisquare) # Tukey
MM_bisquare_stars<- rlm(log.light ~ log.Te, method="MM",psi = psi.bisquare) # Tukey
plot(starsCYG,main="Hertzsprung-Russell Diagram Data",xlab="SURFICE TEMPERATURE", ylab="LUMINIOUS OUTPUT")
abline(M_huber_stars, col="blueviolet")
abline(M_hampel_stars, col="blue")
abline(M_bisquare_stars, col="cyan")
abline(MM_bisquare_stars, col="firebrick", lwd = 2)
legend("bottomleft",cex=0.7,legend = c("M Huber","M Hampel","M Tukey","MM Tukey"),
       lty = c(1,1,1,1,1),col = c("blueviolet","blue","cyan","firebrick"))

coef(summary(M_huber_stars))
coef(summary(M_hampel_stars))
coef(summary(M_bisquare_stars))
coef(summary(MM_bisquare_stars))

# alternative to MM from rlm is lmrob from robustbase package
# lmrob computes fast MM-type estimator

MM_stars2 <- lmrob(log.light ~ log.Te, init = "S")
MM_stars2$coefficients
summary(MM_stars2)



# Robust regression by LMS-estimator

LMS_stars  <- lmsreg(log.light ~ log.Te)
LMS_stars

# Robust regression by LTS-estimator

# two possibilities - ltsreg from MASS (lqs(formula, data, ..., method = c("lts")))
#                   - ltsReg from robustbase
LTS_stars  <- ltsreg(log.light ~ log.Te)

LTS_stars  <- ltsreg(log.light ~ log.Te, alpha=0.9)
LTS_stars  <- ltsreg(log.light ~ log.Te, alpha=0.5, nsamp="exact")


# Summary plot for starsCYG
OLS_stars  <- lm(log.light ~ log.Te)
MM_stars   <- rlm(log.light ~ log.Te,method="MM")
LTS_stars  <- ltsreg(log.light ~ log.Te)
LTS2_stars <- ltsReg(log.light ~ log.Te, alpha=0.9)
LMS_stars  <- lmsreg(log.light ~ log.Te)
plot(starsCYG,main="Hertzsprung-Russell Diagram Data",xlab="SURFICE TEMPERATURE", ylab="LUMINIOUS OUTPUT")
abline(OLS_stars, col="black")

abline(MM_stars, col="blue")
abline(LTS_stars, col="red")
abline(LTS2_stars, col="darkred")
abline(LMS_stars, col="green")
legend("bottomleft",cex=0.7,legend = c("OLS","MM","LTS, h=0.5","LTS h=0.9","LMS"),
         lty = c(1,1,1,1,1),col = c("black","blue","red","darkred","green"))



# Problems with general position
data(exAM)
? exAM
summary(exAM)

#Because the points are not in general position, both LMS and LTS typically fail
summary(lm1   <- lm(y ~ x, data=exAM))
summary(rlm0  <- rlm(y ~ x, data=exAM,method="MM"))
summary(rlm1  <- ltsReg(y ~ x, data=exAM))
summary(rlm1b <- ltsReg(y ~ x, alpha = 0.9,data=exAM))
summary(rlm2  <- lmsreg(y ~ x, data=exAM))


plot(exAM,main="Antille and May artificial data set")
abline(lm1, col="black")
abline(rlm0, col="blue")
abline(rlm1, col="red")
abline(rlm1b, col="darkred")
abline(rlm2, col="green")
legend("topright",cex=0.7,legend = c("OLS","MM","LTS, h=0.5","LTS h=0.9","LMS"),
       lty = c(1,1,1,1,1),col = c("black","blue","red","darkred","green"))


# THE PROBLEM IS HOW LARGE h (alpha) WE SHOULD SELECT FOR LTS
# for example in Stars data - nearly now change
# How trimmed parameter h (alpha) change the estimation?
LTS_stars09 <- ltsReg(log.light ~ log.Te, alpha=0.9)
LTS_stars08 <- ltsReg(log.light ~ log.Te, alpha=0.8)
LTS_stars07 <- ltsReg(log.light ~ log.Te, alpha=0.7)
LTS_stars06 <- ltsReg(log.light ~ log.Te, alpha=0.6)
LTS_stars05 <- ltsReg(log.light ~ log.Te, alpha=0.5)
plot(starsCYG,main="Hertzsprung-Russell Diagram Data",xlab="SURFICE TEMPERATURE", ylab="LUMINIOUS OUTPUT")
abline(LTS_stars09, col="black", lwd = 2)
abline(LTS_stars08, col="blue", lwd = 7)
abline(LTS_stars07, col="red", lwd = 5)
abline(LTS_stars06, col="darkred", lwd = 3)
abline(LTS_stars05, col="green", lwd = 1)
legend("bottomleft",cex=0.7,legend = c("LTS h=0.9","LTS h=0.8","LTS h=0.7","LTS h=0.6","LTS h=0.5"),
       lty = c(1,1,1,1,1),col = c("black","blue","red","darkred","green"))








######## TODAY INDIVIDUAL EXERCISE #########

# Investiagate following datasets
# Try to find influence points (leverages, outliers)
# Try M, MM, LTS estiamtors with different input parameters and compare results
# plot regression lines of more estimators together and discuss resluts


#Hawkins, Bradu, Kass’s Artificial Data
data(hbk)
? hbk
summary(hbk)

pairs(hbk)
plot(hbk)

covMcd(hbk, alpha = 0.7)
attach(hbk)
M_huber_hbk    <- rlm(Y ~ X1, method="M", psi = psi.huber)
M_hampel_hbk   <- rlm(Y ~ X1, method="M", psi = psi.hampel)
M_bisquare_hbk <- rlm(Y ~ X1, method="M", psi = psi.bisquare) # Tukey
MM_bisquare_hbk<- rlm(Y ~ X1, method="MM",psi = psi.bisquare) # Tukey
plot(Y~X1,main="Hawkins, Bradu, Kass's Artificial Data")
abline(M_huber_hbk, col="blueviolet")
abline(M_hampel_hbk, col="blue")
abline(M_bisquare_hbk, col="cyan")
abline(MM_bisquare_hbk, col="firebrick", lwd = 2)
legend("bottomleft",cex=0.7,legend = c("M Huber","M Hampel","M Tukey","MM Tukey"),
       lty = c(1,1,1,1,1),col = c("blueviolet","blue","cyan","firebrick"))


?wood
# Outliers: 4,6,8,19
summary(wood)
plot(wood)

# Education Expenditure Data
data(education)
education <- within(education, Region <- factor(Region))
? education
summary(education)


# Delivery Time Data
data(delivery)
? delivery
summary(delivery)



