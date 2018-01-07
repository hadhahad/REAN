################################## 
##### 01REAN Cviceni 10 ##########
#################################
#
# Todays exercise
# Robust regression - LWS
#
setwd("~/Studies/REAN/tutorials/10")  

# Load data used in this exercise
library(robustbase)
library(MASS)
library(car)

# Nacteme si funkce potrebne pro vypocet LWS
source("LWS.R")


# Nagenerujeme si data 
n=100
p=3
beta0 <- matrix(c(2,3,-4),p,1)

set.seed(123)
X1 <- rep(1,times=n)
X2 <- rnorm(n,10,2)
X3 <- rexp(n,1/5)
X  <- cbind(X1, X2, X3)
e  <- rnorm(n,0,1)
Y  <- X%*%beta0 + e

# Vytvorime 5% outlieru
Yoi    = sample(n)[1:ceiling(n/20)]
Y[Yoi] = 5*Y[Yoi]

# Vytvorime 5% Leverage pointu
Xoi     = sample(n)[1:ceiling(n/20)]
X[Xoi,2:p]  = 0.2*X[Xoi,2:p]  



w = Weights1(n)
OLS_qr(Y,X)
LWS(Y,X,w,100)
summary(ltsReg(Y ~ X2+X3, alpha=0.9))


#Use Hertzsprung-Russell Diagram Data of Star Cluster CYG OB1
? starsCYG
summary(starsCYG)
attach(starsCYG)

w_star       <- Weights1(nrow(starsCYG))
Y = as.matrix(log.light)
X = cbind(rep(1,times=nrow(Y)), log.Te)
LWS_stars_lm <- LWS(Y, X, w_star, 100)
LTS_stars    <- ltsReg(log.light ~ log.Te, alpha=0.9)
OLS_stars    <- lm(log.light ~ log.Te)

plot(starsCYG,main="Hertzsprung-Russell Diagram Data",xlab="SURFICE TEMPERATURE", ylab="LUMINIOUS OUTPUT")
abline(OLS_stars, col="black")
abline(LWS_stars_lm$beta, col="blueviolet")
abline(LTS_stars, col="red")
legend("bottomleft",cex=0.7,legend = c("OLS","LTS, h=0.5","LWS h=0.9"),
       lty = c(1,1,1),col = c("black","blueviolet","red"))


#POZN: Pokud muzete vyvarujte se pocitani LS pomoci primeho vypoctu inverzi matice z X'T !!!
#      Pouzivejte NUMERICKY robustnejsi pristupy.

# Ukol:

#1)
# Vyzkousejte si LWS na datech z minule hodiny, 
# porovnejte vysledky s dalsimi metodami, zkuste zmenit vstupni parametry


#2)
# Vytvorte funkci generujici vahy pro LWS Weights2  tak, 
#  aby pro prvnich g pozorovani klesala konstannte tak, ze na celkovem intervalu (0,g) klesne o 0.1 
#  aby na inervalu (g,h) klesla z 0.9 na 0
#  aby pro (n-h) pozorovani byla rovna 0 (nulova na intervalu (h,n))
#  g a h byly vstupni parametry

# Provedte experiment, kde otestujete konzistenci odhadu regresnich parametru pomoci metody LWS
# Pro pocet pozorovani 50,100,200,300,400,500 nagenerujte data, kde regresni matice X bude mit
#  1. sloupec intercept
#  2. sloupec pozorovani z binomickeho rozdeleni s parametry k=10, p = 0.3 posunute o +1
#  3. sloupec pozorovani z normalniho rozdeleni N(10,2)
#  4. sloupec pozorovani z normalniho rozdeleni N(20,3)
#  5. sloupec pozorovani z exponencialniho rozdeleni se stredni hodnotou 8 
# vektor beta0 volte napriklad beta0=c(2,3,-2,2,3)

# Pro dany pocet pozorovani si 1000 vygenerujte nova data, pro ne spoctete odhad pomoci OLS, LWS a LTS
# Pro kazdy odhad parametru spoctete empiricky median z SE (popripade MSE)
# vykreslete zavislost medianu (popripade eMSE) na poctu pozorovani pro vsechny 3 odhady

#  Experiment opakujte s 0%, 5% a 10% odlehlych pozorovani, ktera muzete vytvorit podle sveho uvazeni 





