#############################################################
###### Analýza odezvy 'medv' vzhledem k proměnné 'nox' ######
#############################################################

### Načtení potřebných knihoven ###
needed.libraries <- c('data.table','car','MASS','ggplot2',
                      'ISLR','graphics','effects','lattice',
                      'segmented','leaps','splines','psych',
                      'corrgram','forecast','lmtest')
for(libs in needed.libraries) require(libs, character.only = TRUE)

# Odstraníme outliery
with(Boston, table(medv))
newBoston <- Boston[which(Boston$medv != 50),]

# Scatter plot
p <- ggplot(newBoston, aes(x=nox, y=medv)) +
  geom_point(size=1, alpha=0.7) +
  geom_smooth(method = 'lm', formula = y ~ x, aes(colour="Linear"))  +
  geom_smooth(method = 'loess', formula = y ~ x, aes(colour="Loess")) +
  scale_colour_manual(name="Smoothers", values=c("blue", "red")) + 
  theme_bw() +
  xlab("Nitrogen Oxides Concentration (parts per 10 million)") +
  ylab("Mean Value of Owner-Occupied Homes") +
  ggtitle("Mean Value of Owner-Occupied Homes and Nitrogen Oxides Concentration") +
  coord_cartesian(xlim=c(0.385, 0.8710), ylim=c(0, 50)) 
p

# Podívejme se na jednoduchý lineární model:
boston_lm <- lm(medv ~ nox, data = newBoston)
summary(boston_lm)
opar <- par(mfrow=c(2,2))
plot(boston_lm)
par(opar)
# Rezidua jsou relativně symetrická, ale podle Q-Q plotu hypotézu o jejich normalitě zamítneme.

############
### Q07: ###
############

### (1) Uděláme logaritmickou transformaci odezvy 'medv'. ###
logboston_lm <- lm(log(medv) ~ nox, newBoston)
summary(logboston_lm)
opar <- par(mfrow=c(2,2))
plot(logboston_lm)
par(opar)
# Rezidua jsou relativně symetrické vzhledem k 0, Q-Q plot také vypadí docela rozumně.

### (2) Graf s regresní křivkou pro nový model a konfidenčními intervaly na hladině významnosti 5%: ###
new_nox <- data.frame(nox = seq(0.3850,0.8710,0.0009925))
new_conf = predict(logboston_lm, newdata = new_nox, interval = "confidence", level=0.95)
new_pred = predict(logboston_lm, newdata = new_nox, interval = "prediction", level=0.95)
logtrans_p1<- ggplot(newBoston, aes(x=nox, y=log(medv))) +
  geom_point(size=1, alpha=0.7) +
  labs(color="Lines:") + 
  geom_line(aes(x=seq(0.3850,0.8710,0.0009925), y=new_conf[,1], colour = "Fit")) + 
  geom_line(aes(x=seq(0.3850,0.8710,0.0009925), y=new_conf[,2], colour = "Confidence Interval")) + 
  geom_line(aes(x=seq(0.3850,0.8710,0.0009925), y=new_conf[,3], colour = "Confidence Interval")) + 
  geom_line(aes(x=seq(0.3850,0.8710,0.0009925), y=new_pred[,2], colour = "Prediction Interval"), linetype=2) + 
  geom_line(aes(x=seq(0.3850,0.8710,0.0009925), y=new_pred[,3], colour = "Prediction Interval"), linetype=2) + 
  theme_bw() +
  xlab("Nitrogen Oxides Concentration (parts per 10 million)") +
  ylab("Log - Mean Value of Owner-Occupied Homes") +
  ggtitle("Linear Model with Log-Transformation of the Dependant Variable (Tolerance - 5%)") +
  coord_cartesian(xlim=c(0.385, 0.8710), ylim=c(1.5, 4))
logtrans_p1

### (3) Porovnáme naši transformaci s transformací navrženou Box-Coxem: ###
dev.off()

# Log-věrohodnostní profil Box-Coxovy transformace:
boxcox(boston_lm)
# Nový model s použitím Box-Coxovy transformace.
lambda <- BoxCox.lambda(Boston$medv)
medv_bc <- BoxCox(Boston$medv, lambda)
boston_lm_bc <- lm(medv_bc ~ nox, Boston)
summary(boston_lm_bc)
summary(logboston_lm)
# Koeficient determinace pro model s Box-Coxovou transformací není lepší
# než u modelu s logaritmickou transformací odezvy. 
# Vzhledem k Log-věrohodnostnímu profilu si dovolíme zvolit pro Box-Coxovou 
# transformaci \lambda=0, čímž obdržíme logaritmickou.

# Nakonec se podíváme na chování reziduí obou modelů:
opar <- par(mfrow=c(2,2))
plot(boston_lm_bc)  # Box-Coxova transformace pro \lambda=-0.2380317
plot(logboston_lm)  # Logaritmická transformace odezvy
par(opar)
# Je vidět, Q-Q plot pro obyčejnou logaritmickou transformaci vypadá lépe.


############
### Q08: ###
############

# Ještě jednou se podívejme na logaritmickou transformaci odezvy 'medv':
logtrans_p1

exp_beta1 <- exp(coef(logboston_lm)[2]*0.1)  
# Je to kladné číslo menší než jedna vyjádřující poměr E[Y|X=x+0.1]/E[Y|X=x].
# Tedy (1-exp_beta1)*100 % vyjadřuje o kolik procent klesne cena nemovitosti 
# při nárustu 'nox' o 0.1.
(1-exp_beta1)*100
# Což je přibližně 16.97 %.

############
### Q09: ###
############ 

dev.off()

### (1) Lineární transformace. ###
lin_boston_lm <- lm(medv ~ poly(nox, degree=1, raw=TRUE), data = newBoston)
summary(lin_boston_lm)
new_boston_linear <- predict(lin_boston_lm, newdata = new_nox)
crPlots(lin_boston_lm)

### (2) Kvadratická polynomiální transformace. ###
quadratic_boston_lm <- lm(medv ~ poly(nox, degree=2, raw=TRUE), data = newBoston)
summary(quadratic_boston_lm)
new_boston_quadratic <- predict(quadratic_boston_lm, newdata = new_nox)
crPlots(quadratic_boston_lm)

### (3) Kubická polynomiální transformace. ###
cubic_boston_lm <- lm(medv ~ poly(nox, degree=3, raw=TRUE), data = newBoston)
summary(cubic_boston_lm)
new_boston_cubic <- predict(cubic_boston_lm, newdata = new_nox)
crPlots(cubic_boston_lm)

### (4) Splines. ###
splines_boston_lm <- lm(medv ~ bs(nox,knots=c(0.695),degree=1,Boundary.knots=c(0.385, 0.8710)), data = newBoston)
summary(splines_boston_lm)
new_boston_splines <- predict(splines_boston_lm, newdata = new_nox)

# Zobrazíme si tyto transformace:
transf_plot <- ggplot(newBoston, aes(x=nox, y=medv)) +
  geom_point(size=1, alpha=0.7) +
  geom_line(aes(x=new_nox, y=new_boston_linear, colour="Linear Transformation")) +
  geom_line(aes(x=new_nox, y=new_boston_quadratic, colour="Quadratic Transformation")) +
  geom_line(aes(x=new_nox, y=new_boston_cubic, colour="Cubic Transformation")) +
  geom_line(aes(x=new_nox, y=new_boston_splines, colour="Splines Transformation")) +
  labs(color="Lines:") + 
  theme_bw() +
  xlab("Nitrogen Oxides Concentration (parts per 10 million)") +
  ylab("Mean Value of Owner-Occupied Homes") +
  ggtitle("Transformations of the Dependant Variable - Boston Dataset") +
  coord_cartesian(xlim=c(0.385, 0.8710), ylim=c(0, 50))
transf_plot

### Podívejme se na rezidua modelů. ###
opar <- par(mfrow=c(2,2))
plot(lin_boston_lm) # Tento model nesplňuje symetrii reziduí (je přítomná nelineární závislost) ani jejich normalitu.
plot(quadratic_boston_lm)
plot(cubic_boston_lm)
plot(splines_boston_lm)
par(opar)
# Ostatní modely jsou vzhledem symetrii o něco lepší, ale je problém s normalitou reziduí (pokračování v Q10).



############
### Q10: ###
############

# Z nějakého důvodu 'medv' počinaje hodnotou 'nox'~0.8 začíná 
# růst (průmyslová zóna se silným zněčištěním?).
# Necháme to tak, jak je, i když by asi stalo za to tyto hodnoty odstranit. 

### Vybereme model 'splines_boston_lm', ale provedeme pro něj navíc logaritmickou transformaci odezvy. ###
opar <- par(mfrow=c(2,2))
logsplines_boston_lm <- lm(log(medv) ~ bs(nox,knots=c(0.695),degree=1,Boundary.knots=c(0.385, 0.8710)), data = newBoston)
summary(logsplines_boston_lm)
# Logaritmizací odezvy jsme se docíleli zvětšení koeficientu determinace a zlepšenít tvaru Q-Q plotu.
plot(logsplines_boston_lm)
par(opar)
# Nyní Q-Q Plot vypadá o něco lepší, stále ale jsou přítomny outliery a leverage pointy.
# Navíc se zvětšila signifikance poslední proměnné.

### Nakreslíme si Scatter Plot s konfidenčními a prediction intervaly na hladine významnosti 5 %: ###
new_conf = predict(logsplines_boston_lm, newdata = new_nox, interval = "confidence", level=0.95)
new_pred = predict(logsplines_boston_lm, newdata = new_nox, interval = "prediction", level=0.95)
boston_crime_final_p1<- ggplot(newBoston, aes(x=nox, y=log(medv))) +
  geom_point(size=1, alpha=0.7) +
  labs(color="Lines:") + 
  geom_line(aes(x=seq(0.3850,0.8710,0.0009925), y=new_conf[,1], colour = "Fit")) + 
  geom_line(aes(x=seq(0.3850,0.8710,0.0009925), y=new_conf[,2], colour = "Confidence Interval")) + 
  geom_line(aes(x=seq(0.3850,0.8710,0.0009925), y=new_conf[,3], colour = "Confidence Interval")) + 
  geom_line(aes(x=seq(0.3850,0.8710,0.0009925), y=new_pred[,2], colour = "Prediction Interval"), linetype=2) + 
  geom_line(aes(x=seq(0.3850,0.8710,0.0009925), y=new_pred[,3], colour = "Prediction Interval"), linetype=2) + 
  theme_bw() +
  xlab("Per Capita Crime Rate by Town") +
  ylab("Log - Mean Value of Owner-Occupied Homes") +
  ggtitle("Mean Value of Owner-Occupied Homes vs. Criminality") +
  coord_cartesian(xlim=c(0.3850, 0.8710), ylim=c(1.5, 4))
boston_crime_final_p1

### Plot efektů: ###
plot(allEffects(logsplines_boston_lm))

### Shapiro-Wilk Normality Test: ###
shapiro.test(residuals(logsplines_boston_lm)) 
# P-value je nízka, tudíž musíme hypotézu o normalitě reziduí zamítnout.

### Lilliefors (Kolmogorov-Smirnov) test for normality: ###
library(nortest)
lillie.test(residuals(logsplines_boston_lm)) # Analogicky, ale na hladině 1 % by to šlo příjmout.


