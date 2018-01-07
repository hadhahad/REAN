###############################
####### Zápočtová úloha #######
####### Vladislav Belov #######
###############################


### Načtení potřebných knihoven ###
needed.libraries <- c('data.table','car','MASS','ggplot2',
                      'ISLR','graphics','effects','lattice',
                      'segmented','leaps','splines','psych',
                      'corrgram','forecast','lmtest')
for(libs in needed.libraries) require(libs, character.only = TRUE)

? Boston
head(Boston)

############
### Q01: ###
############

# Z použití funkce 'summary' vyplývá, že v datovém souboru nejsou chybějicí hodnoty (NA), navíc
# lze také vidět základní charakteristiky proměnných.
describe(Boston)

# Rozměry datového souboru jsou 506x14, což odpovídá rozměrům uvedeným v zadání zápočtové úlohy.
dim(Boston)


############
### Q02: ###
############

# Histogram a odhad hustoty pro odezvu 'medv'.
hist(Boston$medv, freq=F, main="Density Estimation", 
     xlab="Median Value of Owner-Occupied Homes in $1000s.",
     xlim=c(0, 55), ylim=c(0, 0.07))
lines(density(Boston$medv), col="red", lwd=2)


############
### Q03: ###
############

### (a) Scatter plot závislosti cen nemovitostí na kriminalitě (knihovna 'car'). ###
# + smoothery
scatterplot(medv ~ crim, Boston, grid = T, smoother = NULL, lwd = 2,
            xlab = "Per Capita Crime Rate by Town", 
            ylab = "Mean Value of Owner-Occupied Homes",
            xlim = c(0, 90), 
            ylim = c(0, 50),
            pch = 20)
lines(lowess(x = Boston$crim, y = Boston$medv), col = "red", lwd = 2)
legend("topright", 
       legend = c("Linear Smoother", "LOWESS Smoother"), 
       col = c("green", "red"), 
       pch = c(19,19), 
       bty = "n", 
       pt.cex = 1.5, 
       cex = 1, 
       text.col = "black", 
       horiz = F , 
       inset = c(0, 0.1),)

### (b) Scatter plot závislosti cen nemovitostí na kriminalitě (knihovna 'ggplot2'). ###
# + smoothery s konfidenčními intervaly
# ('loess smoother' je obecnější než 'lowess') 
p1 <- ggplot(Boston, aes(x=crim, y=medv)) +
  geom_point(size=1, alpha=0.7) +
  geom_smooth(method = 'lm', formula = y ~ x, aes(colour="Linear"))  +
  geom_smooth(method = 'loess', formula = y ~ x, aes(colour="Loess")) +
  scale_colour_manual(name="Smoothers", values=c("blue", "red")) + 
  theme_bw() +
  xlab("Per Capita Crime Rate by Town") +
  ylab("Mean Value of Owner-Occupied Homes") +
  ggtitle("Mean Value of Owner-Occupied Homes and Crime Rate") +
  coord_cartesian(xlim=c(0, 90), ylim=c(0, 50))
p1

### Vybereme zbůsob (b) pro zobrazení závislosti 'medv' na ostatních parametrech: ###
p2 <- ggplot(Boston, aes(x=nox, y=medv)) +
  geom_point(size=1, alpha=0.7) +
  geom_smooth(method = 'lm', formula = y ~ x, aes(colour="Linear"))  +
  geom_smooth(method = 'loess', formula = y ~ x, aes(colour="Loess")) +
  scale_colour_manual(name="Smoothers", values=c("blue", "red")) + 
  theme_bw() +
  xlab("Nitrogen Oxides Concentration (parts per 10 million)") +
  ylab("Mean Value of Owner-Occupied Homes") +
  ggtitle("Mean Value of Owner-Occupied Homes and Nitrogen Oxides Concentration") +
  coord_cartesian(xlim=c(0.385, 0.8710), ylim=c(0, 50)) 
p3 <- ggplot(Boston, aes(x=rm, y=medv)) +
  geom_point(size=1, alpha=0.7) +
  geom_smooth(method = 'lm', formula = y ~ x, aes(colour="Linear"))  +
  geom_smooth(method = 'loess', formula = y ~ x, aes(colour="Loess")) +
  scale_colour_manual(name="Smoothers", values=c("blue", "red")) + 
  theme_bw() +
  xlab("Average Number of Rooms per Dwelling") +
  ylab("Mean Value of Owner-Occupied Homes") +
  ggtitle("Mean Value of Owner-Occupied Homes and Average Number of Rooms") +
  coord_cartesian(xlim=c(3.561, 8.780), ylim=c(0, 50))
p4 <- ggplot(Boston, aes(x=lstat, y=medv)) +
  geom_point(size=1, alpha=0.7) +
  geom_smooth(method = 'lm', formula = y ~ x, aes(colour="Linear"))  +
  geom_smooth(method = 'loess', formula = y ~ x, aes(colour="Loess")) +
  scale_colour_manual(name="Smoothers", values=c("blue", "red")) + 
  theme_bw() +
  xlab("Lower Status of the Population (percent)") +
  ylab("Mean Value of Owner-Occupied Homes") +
  ggtitle("Mean Value of Owner-Occupied Homes and Lower Status of the Population") +
  coord_cartesian(xlim=c(1.7, 38), ylim=c(0, 50))
p5 <- ggplot(Boston, aes(x=ptratio, y=medv)) +
  geom_point(size=1, alpha=0.7) +
  geom_smooth(method = 'lm', formula = y ~ x, aes(colour="Linear"))  +
  geom_smooth(method = 'loess', formula = y ~ x, aes(colour="Loess")) +
  scale_colour_manual(name="Smoothers", values=c("blue", "red")) + 
  theme_bw() +
  xlab("Pupil-Teacher Ratio by Town") +
  ylab("Mean Value of Owner-Occupied Homes") +
  ggtitle("Mean Value of Owner-Occupied Homes and Pupil-Teacher Ratio") +
  coord_cartesian(xlim=c(12.6, 22), ylim=c(0, 50))
p6 <- ggplot(Boston, aes(x=dis, y=medv)) +
  geom_point(size=1, alpha=0.7) +
  geom_smooth(method = 'lm', formula = y ~ x, aes(colour="Linear"))  +
  geom_smooth(method = 'loess', formula = y ~ x, aes(colour="Loess")) +
  scale_colour_manual(name="Smoothers", values=c("blue", "red")) + 
  theme_bw() +
  xlab("Weighted Mean of Distances to Five Boston Employment Centres") +
  ylab("Mean Value of Owner-Occupied Homes") +
  ggtitle("Mean Value of Owner-Occupied Homes and Mean of Distances to Centres") +
  coord_cartesian(xlim=c(1, 12.5), ylim=c(0, 50))

### Grafy: ###
# Mean Value of Owner-Occupied Homes and Crime Rate
p1
# Mean Value of Owner-Occupied Homes and Nitrogen Oxides Concentration
p2
# Mean Value of Owner-Occupied Homes and Average Number of Rooms
p3
# Mean Value of Owner-Occupied Homes and Lower Status of the Population
p4
# Mean Value of Owner-Occupied Homes and Pupil-Teacher Ratio
p5
# Mean Value of Owner-Occupied Homes and Mean of Distances to Centres
p6


############
### Q04: ###
############

### Boxplot pro proměnnou 'rad' ###
radbp <- ggplot(Boston, aes(x=rad, y=medv, group=rad)) + 
  geom_boxplot(fill="white", colour="darkblue", notch=F) +
  geom_jitter(width = 0.2) +
  xlab("Index of Accessibility to Radial Highways") +
  ylab("Mean Value of Owner-Occupied Homes") + 
  ggtitle("Mean Value of Owner-Occupied Homes and Index of Accessibility to Radial Highways")
radbp

### Boxplot pro proměnnou 'chas' ###
is.factor(Boston$chas)
Boston$chas <- as.factor(Boston$chas)
chasbp <- ggplot(Boston, aes(x=chas, y=medv, group=chas)) + 
  geom_boxplot(fill="white", colour="darkblue", notch=F) +
  geom_jitter(width = 0.2) +
  xlab("Charles River Dummy Variable") +
  ylab("Mean Value of Owner-Occupied Homes") + 
  ggtitle("Mean Value of Owner-Occupied Homes and Index of Accessibility to Radial Highways")
chasbp

### Boxplot pro proměnnou 'rad' s transformací ###
Boston["newrad"] <- "Low Accessibility"
Boston$newrad[Boston$rad < 10] = "High Accessibility"
newradbp <- ggplot(Boston, aes(x=newrad, y=medv, group=newrad)) + 
  geom_boxplot(fill="white", colour="darkblue", notch=TRUE) +
  geom_jitter(width = 0.2) + 
  xlab("Index of Accessibility to Radial Highways") +
  ylab("Mean Value of Owner-Occupied Homes") + 
  ggtitle("Mean Value of Owner-Occupied Homes and Index of Accessibility to Radial Highways")
newradbp


############
### Q05: ###
############

#(Description: Another types of visualization)
#Correlogram. It is used to test the level of co-relation among the variable available in the data set.
corrgram(Boston, order=NULL,panel=panel.shade)


############
### Q06: ###
############

boston_lm <- lm(medv ~ crim, Boston)
summary(boston_lm)
# Vidíme, že intercept a proměnná 'crim' jsou signifikantní podle p-value, 
# ale koeficient determinace je docela nízký.

# Podíváme se na chování reziduí:
opar <- par(mfrow=c(2,2))
plot(boston_lm)
par(opar)
# Je vidět, že rezidua nejsou rozmístěny symetricky kolem regresní křivky 
# a navíc z Q-Q plotu neplyne jejich normalita.

# Nyní se podíváme na fit modelu:
ggplot(Boston, aes(x=crim, y=medv)) +
  geom_point(size=1, alpha=0.7) +
  geom_abline(intercept = coef(boston_lm)[1], slope = coef(boston_lm)[2], col='darkred') +
  theme_bw() +
  xlab("Per Capita Crime Rate by Town") +
  ylab("Mean Value of Owner-Occupied Homes") +
  ggtitle("Simple Linear Model Fit") +
  coord_cartesian(xlim=c(0, 90), ylim=c(0, 50))
# Z fitu daného jednoduchého regresního modelu vyplývá, 
# že ceny nemovitiostí klesají v závislosti na míře kriminality
(coef(boston_lm)[1] + coef(boston_lm)[2]*0)-(coef(boston_lm)[1] + coef(boston_lm)[2]*10)
# Vzroste-li kriminalita o 10 jednotek, pak ceny v průměru klesnou o přibližne $4000.
1-(coef(boston_lm)[1] + coef(boston_lm)[2]*2)/(coef(boston_lm)[1]+coef(boston_lm)[2]*1)
# Podle tohoto modelu cena nemovitosti klesne přibližně o 1.8 % 
# při nárůstu kriminality o 1 jednotku.


############
### Q07: ###
############

### (1) Uděláme logaritmickou transformaci odezvy 'medv'. ###
logboston_lm <- lm(log(medv)~ crim, Boston)
summary(logboston_lm)
# Koeficient determinace se zvýšil, p-value ukazuje na signifikantnost proměnných.
opar <- par(mfrow=c(2,2))
plot(logboston_lm)
par(opar)
# Rezidua jsou nyní rozmístěny více symetricky a Q-Q plot také vypadá lépe.
# Stále ale model není ideální.

### (2) Graf s regresní křivkou pro nový model a konfidenčními intervaly na hladině významnosti 5%: ###
# V daném případě konfidenční interval neposkytuje skoro žádnou užitečnou informaci, 
# prediction interval obsahuje v sobě většinu pozorování.
new_crime <- data.frame(crim = seq(0,90,0.1779))
new_conf = predict(logboston_lm, newdata = new_crime, interval = "confidence", level=0.95)
new_pred = predict(logboston_lm, newdata = new_crime, interval = "prediction", level=0.95)
logtrans_p1<- ggplot(Boston, aes(x=crim, y=log(medv))) +
  geom_point(size=1, alpha=0.7) +
  labs(color="Lines:") + 
  geom_line(aes(x=seq(0,90,0.1779), y=new_conf[,1], colour = "Fit")) + 
  geom_line(aes(x=seq(0,90,0.1779), y=new_conf[,2], colour = "Confidence Interval")) + 
  geom_line(aes(x=seq(0,90,0.1779), y=new_conf[,3], colour = "Confidence Interval")) + 
  geom_line(aes(x=seq(0,90,0.1779), y=new_pred[,2], colour = "Prediction Interval"), linetype=2) + 
  geom_line(aes(x=seq(0,90,0.1779), y=new_pred[,3], colour = "Prediction Interval"), linetype=2) + 
  theme_bw() +
  xlab("Per Capita Crime Rate by Town") +
  ylab("Log - Mean Value of Owner-Occupied Homes") +
  ggtitle("Linear Model with Log-Transformation of the Dependant Variable (Tolerance - 5%)") +
  coord_cartesian(xlim=c(0, 90), ylim=c(1, 4))
logtrans_p1

### (3) Porovnáme naši transformaci s transformací navrženou Box-Coxem: ###
dev.off()
# Log-věrohodnostní profil Box-Coxovy transformace:
boxcox(boston_lm)
# Nový model s použitím Box-Coxovy transformace.
lambda <- BoxCox.lambda(Boston$medv)
medv_bc <- BoxCox(Boston$medv, lambda)
boston_lm_bc <- lm(medv_bc ~ crim, Boston)
summary(boston_lm_bc)
summary(logboston_lm)
# Je vidět, že Box-Coxova transformace zlepšila původní model: zvětšil se koeficient determinace.

# Nakonec se podíváme na chování reziduí obou modelů:
opar <- par(mfrow=c(2,2))
plot(boston_lm_bc)  # Box-Coxova transformace
plot(logboston_lm)  # Logaritmická transformace odezvy
par(opar)
# Je vidět, že porovnávané modely mají problém se symetrií reziduí a jejich normalitou.
# Oba modely identifikují stejné problematické body s indexy 381, 400, 490, 419.


############
### Q08: ###
############

# Ještě jednou se podívejme na logaritmickou transformaci odezvy 'medv':
logtrans_p1

exp_beta1 <- exp(coef(logboston_lm)[2])  
# Je to kladné číslo menší než jedna vyjádřující poměr E[Y|X=x+1]/E[Y|X=x].
# Tedy (1-exp_beta1)*100 % vyjadřuje o kolik procent klesne cena nemovitosti 
# při nárustu míry kriminality o 1 jendotku.
(1-exp_beta1)*100
# Což je přibližně 2.48 %.
# Tento model předpovídá větší pokles než jednoduchý lineární model z Q06.


############
### Q09: ###
############ +I()

dev.off()

### (1) Po částech konstantní transformace (Piecewise Regression). ###
const_boston_lm <- segmented(lm(medv ~ crim, Boston), seg.Z = ~ crim)
summary(const_boston_lm)
new_crime <- data.frame(crim = seq(0,90,0.1779))
new_boston_piecewise <- predict(const_boston_lm, newdata = new_crime)

### (2) Lineární transformace. ###
lin_boston_lm <- lm(medv ~ poly(crim, degree=1, raw=TRUE), Boston)
summary(lin_boston_lm)
new_boston_linear <- predict(lin_boston_lm, newdata = new_crime)
crPlots(lin_boston_lm)

### (3) Kvadratická polynomiální transformace. ###
quadratic_boston_lm <- lm(medv ~ poly(crim, degree=2, raw=TRUE), Boston)
summary(quadratic_boston_lm)
new_boston_quadratic <- predict(quadratic_boston_lm, newdata = new_crime)
crPlots(cubic_boston_lm)

### (4) Kubická polynomiální transformace. ###
cubic_boston_lm <- lm(medv ~ poly(crim, degree=3, raw=TRUE), Boston)
summary(cubic_boston_lm)  # Je vidět, že proměnná se stupněm 3 už není tak signifikantní.
new_boston_cubic <- predict(cubic_boston_lm, newdata = new_crime)
crPlots(cubic_boston_lm)  # Neliší se od předchozího crPlots pro kvadratickou transformaci.

### (5) Splines. ###
splines_boston_lm <- lm(medv ~ bs(crim,knots=c(12, 25),degree=1,Boundary.knots=c(0,90)), Boston)
summary(splines_boston_lm)
new_boston_splines <- predict(splines_boston_lm, newdata = new_crime)
transf_plot <- ggplot(Boston, aes(x=crim, y=medv)) +
  geom_point(size=1, alpha=0.7) +
  geom_line(aes(x=new_crime, y=new_boston_piecewise, colour="Piecewise Regression Line")) +
  geom_line(aes(x=new_crime, y=new_boston_linear, colour="Linear Transformation")) +
  geom_line(aes(x=new_crime, y=new_boston_quadratic, colour="Quadratic Transformation")) +
  geom_line(aes(x=new_crime, y=new_boston_cubic, colour="Cubic Transformation")) +
  geom_line(aes(x=new_crime, y=new_boston_splines, colour="Splines Transformation")) +
  labs(color="Lines:") + 
  theme_bw() +
  xlab("Per Capita Crime Rate by Town") +
  ylab("Mean Value of Owner-Occupied Homes") +
  ggtitle("Transformations of the Dependant Variable - Boston Dataset") +
  coord_cartesian(xlim=c(0, 90), ylim=c(0, 50))
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

### Vybereme model 'splines_boston_lm', ale provedeme pro něj navíc logaritmickou transformaci odezvy. ###
opar <- par(mfrow=c(2,2))
logsplines_boston_lm <- lm(log(medv) ~ bs(crim,knots=c(12,25),degree=1,Boundary.knots=c(0,90)), Boston)
summary(logsplines_boston_lm)
plot(logsplines_boston_lm)
par(opar)
# Nyní Q-Q Plot vypadá o něco lepší, stále ale jsou přítomny outliery a leverage pointy.
# Navíc se zvětšila signifikance poslední proměnné.

### Nakreslíme si Scatter Plot s konfidenčními a prediction intervaly na hladine významnosti 5 %: ###
new_crime <- data.frame(crim = seq(0,90,0.1779))
new_conf = predict(logsplines_boston_lm, newdata = new_crime, interval = "confidence", level=0.95)
new_pred = predict(logsplines_boston_lm, newdata = new_crime, interval = "prediction", level=0.95)
boston_crime_final_p1<- ggplot(Boston, aes(x=crim, y=log(medv))) +
  geom_point(size=1, alpha=0.7) +
  labs(color="Lines:") + 
  geom_line(aes(x=seq(0,90,0.1779), y=new_conf[,1], colour = "Fit")) + 
  geom_line(aes(x=seq(0,90,0.1779), y=new_conf[,2], colour = "Confidence Interval")) + 
  geom_line(aes(x=seq(0,90,0.1779), y=new_conf[,3], colour = "Confidence Interval")) + 
  geom_line(aes(x=seq(0,90,0.1779), y=new_pred[,2], colour = "Prediction Interval"), linetype=2) + 
  geom_line(aes(x=seq(0,90,0.1779), y=new_pred[,3], colour = "Prediction Interval"), linetype=2) + 
  theme_bw() +
  xlab("Per Capita Crime Rate by Town") +
  ylab("Log - Mean Value of Owner-Occupied Homes") +
  ggtitle("Mean Value of Owner-Occupied Homes vs. Criminality") +
  coord_cartesian(xlim=c(0, 90), ylim=c(1, 4))
boston_crime_final_p1

### Plot efektů: ###
plot(allEffects(logsplines_boston_lm))

### Shapiro-Wilk Normality Test: ###
shapiro.test(residuals(logsplines_boston_lm)) 
# P-value je docelá nízka, tudíž musíme hypotézu o normalitě reziduí zamítnout.

### Lilliefors (Kolmogorov-Smirnov) test for normality: ###
library(nortest)
lillie.test(residuals(logsplines_boston_lm)) # Analogicky.


############
### Q11: ###
############

pairs(~(.), newdata,  main="Basic Scatterplot Matrix")

with(Boston, table(medv))
median(Boston$medv)
# Tabulka četností jednotlivých hodnot odezvy 'medv' ukazuje na to, že 
# by hodnoty rovné 50 mohly vzniknout useknutím nebo chybně. 
# Proto si je dovolíme odstranit.
newdata <- Boston[which(Boston$medv != 50),]
median(newdata$medv) 
# Po odstranění zmíněných hodnot se hodnota mediánu skoro nezmeníla.
dim(newdata)

mm <- lm(medv ~ ., data = newdata)
sort(residuals(mm))


############
### Q12: ###
############

dev.off()
boston_all_lm <- lm(log(medv) ~ ., newdata)
opar <- par(mfrow=c(2,2))
plot(boston_all_lm)
par(opar)
summary(boston_all_lm)  # Model se všemi proměnnými najednou je přetížený.

### Pro výběr ze všech možných vysvětlujících proměnných použijeme balík 'leaps' a kritéria Cp/r2. ###
summary(newdata)
leapsstatCp <- leaps(x=newdata[,c(1:13)], y=newdata[,14], 
                     names=names(newdata)[c(1:13)], method="Cp")
leapsstatr2 <- leaps(x=newdata[,c(1:13)], y=newdata[,14], 
                     names=names(newdata)[c(1:13)], method="r2")
CpParams <- leapsstatCp$size; CpValues <- leapsstatCp$Cp
r2Params <- leapsstatr2$size; r2Values <- leapsstatr2$r2
opar <- par(mfrow=c(1,2))
plot(CpValues ~ CpParams)
plot(r2Values ~ r2Params) 
par(opar)
# Vybereme proměnné s indexy 4, 5, 6, 8, 11, 13 (podle grafů je vidět, že existují i lepší
# modely, ale chceme dobrý poměr cena/výkon.
names(Boston)[c(4, 5, 6, 8, 11, 13)]
leaps <- regsubsets(log(medv) ~ chas + nox + rm + dis + ptratio + lstat, newdata, nbest=10)
summary(leaps)
plot(leaps,scale="Cp")
plot(leaps,scale="adjr2")
subsets(leaps, statistic="bic") 
subsets(leaps, statistic="adjr2") 
dev.off()

### Nakonec zvolíme model s proměnnými r-p-l. ###
model_final <- lm(log(medv) ~ rm + ptratio + lstat, newdata)
# Otestujeme náš výběr pomocí 'step' (Akaike).
summary(step(model_final))  # Z AIC vyplývá, že všechny vybrané proměnné jsou signifikantní. 
summary(model_final)  # F-statistic také dává dobrý výsledek.

# Na grafu Residuals vs. Fitted lze pozorovat symetrii.
op <- par(mfrow=c(2,2))
plot(model_final)
par(opar)

# Z Shapiro-Wilkova testu neplyne normalita reziduí.
shapiro.test(residuals(model_final))

# Breusch-Paganův test ukazuje na heteroskedascicitu.
bptest(model_final)


############
### Q13: ###
############

# Z diagramu níže a z tvaru kovarianční matice vyplývá, že mezi 
# vybranými proměnnými není významná kolinearita. Vzhledem ke korelacím
# ostatních proměnných si dovolíme říct, že proměnné byly vybrány relativně dobře.
# Lze ale mezi zvolenými proměnnými pozorovat jakousi nelineární závislost. 
pairs(medv ~ rm + ptratio + lstat, data = newdata)
cor(cbind(newdata$crim,newdata$zn,newdata$indus,newdata$chas,newdata$nox,
          newdata$rm,newdata$age,newdata$dis,newdata$rad,newdata$tax,
          newdata$black,newdata$ptratio,newdata$lstat))
# Q05 obsahuje zobrazení matice kolinearity.

# Inflace variance pro vybraný model také není velka (mnohem menší <1.7).
vif(model_final)

# Index podmíněnosti je o hodně menší 30
# (wiki: "If the condition number is above 30, the regression 
# may have significant multicollinearity").
kappa(cor(cbind(newdata$rm,newdata$ptratio,newdata$lstat)),exact=T)


############
### Q14: ###
############

# Zvolený model nezahrnuje v sobě kriminalitu.
# Pro účely zadání Q14 ji přidáme.
model_final_crim <- update(model_final, . ~ . + crim)
summary(model_final)  # Vidíme, že se trochu zvětšil koeficient determinace.
summary(model_final_crim)
opar <- par(mfrow=c(2,2))
plot(model_final_crim)
par(opar)

exp_bet1 <- exp(coef(model_final_crim)[5])  
# Je to kladné číslo menší než jedna vyjádřující poměr E[Y|U,V,X=x+1]/E[Y|U,V,X=x].
# Tedy (1-exp_beta1)*100 % vyjadřuje o kolik procent klesne cena nemovitosti při nárustu míry kriminality o 1 jendotku.
(1-exp_bet1)*100
# Daný model predikuje pokles cen přibližně o 1.02 % při nárůstu kriminality o jednotku.

median(newdata$crim)
plot(allEffects(model_final_crim))


############
### Q15: ###
############

### Výsledný model:
summary(model_final)
op <- par(mfrow=c(2,2))
plot(model_final)
par(opar)
# Tento model má přípustný koeficient determinace (>0.6).
# V Q12 byla pozorována symetrie reziduí a byly provedeny testy na 
# heteroskedascicitu a normalitu reziduí.

# Podívejme se na partial-regression grafy:
avPlots(model_final_crim)

# Na Q-Q plotu jsou stále vidět outliery.
ggplot(newdata, aes(sample = residuals(model_final))) +
  stat_qq(color="firebrick2", alpha=1) +
  geom_abline(intercept = mean(residuals(model_final)), slope = sd(residuals(model_final)))

# Outliery a leverage pointy by šlo odstranit nějakoi robustní metodou (LTS, LWS).


