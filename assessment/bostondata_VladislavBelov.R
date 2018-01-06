###############################
####### Zápočtová úloha #######
####### Vladislav Belov #######
###############################


### Načtení potřebných knihoven ###
needed.libraries <- c('data.table','car','MASS','ggplot2','ISLR','graphics','effects','lattice')
for(libs in needed.libraries) require(libs, character.only = TRUE)


### Načtení dat ###
? Boston
head(Boston)
data <- Boston


############
### Q01: ###
############

# Z použití funkce 'summary' vyplývá, že v datovém souboru nejsou chybějicí hodnoty (NA), navíc
# lze také vidět základní charakteristiky proměnných.
summary(data)

# Rozměry datového souboru jsou 506x14, což odpovídá rozměrům uvedeným v zadání zápočtové úlohy.
dim(data)


############
### Q02: ###
############

# Histogram a odhad hustoty pro odezvu 'medv'.
hist(data$medv, freq=F, main="", xlab="Median Value of Owner-Occupied Homes in $1000s.",xlim=c(0, 55), ylim=c(0, 0.07))
lines(density(data$medv), col="red", lwd=2)


############
### Q03: ###
############

### (a) Scatter plot závislosti cen nemovitostí na kriminalitě (knihovna 'car'). ###
# + smoothery
scatterplot(medv ~ crim, data, grid = T, smoother = NULL, lwd = 2,
            xlab = "Per Capita Crime Rate by Town", 
            ylab = "Mean Value of Owner-Occupied Homes",
            xlim = c(0, 90), 
            ylim = c(0, 50),
            pch = 20)
lines(lowess(x = data$crim, y = data$medv), col = "red", lwd = 2)
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
p1 <- ggplot(data, aes(x=crim, y=medv)) +
  geom_point(size=1, alpha=0.7) +
  geom_smooth(method = 'lm', formula = y ~ x, aes(colour="Linear"))  +
  geom_smooth(method = 'loess', formula = y ~ x, aes(colour="Loess")) +
  scale_colour_manual(name="Smoothers", values=c("blue", "red")) + 
  theme_bw() +
  xlab("Per Capita Crime Rate by Town") +
  ylab("Mean Value of Owner-Occupied Homes") +
  ggtitle("Mean Value of Owner-Occupied Homes and Crime Rate - Boston Dataset") +
  coord_cartesian(xlim=c(0, 90), ylim=c(0, 50))
p1

### Vybereme zbůsob (b) pro zobrazení závislosti 'medv' na ostatních parametrech: ###
p2 <- ggplot(data, aes(x=nox, y=medv)) +
  geom_point(size=1, alpha=0.7) +
  geom_smooth(method = 'lm', formula = y ~ x, aes(colour="Linear"))  +
  geom_smooth(method = 'loess', formula = y ~ x, aes(colour="Loess")) +
  scale_colour_manual(name="Smoothers", values=c("blue", "red")) + 
  theme_bw() +
  xlab("Nitrogen Oxides Concentration (parts per 10 million)") +
  ylab("Mean Value of Owner-Occupied Homes") +
  ggtitle("Mean Value of Owner-Occupied Homes and Nitrogen Oxides Concentration - Boston Dataset") +
  coord_cartesian(xlim=c(0.385, 0.8710), ylim=c(0, 50)) 
p3 <- ggplot(data, aes(x=rm, y=medv)) +
  geom_point(size=1, alpha=0.7) +
  geom_smooth(method = 'lm', formula = y ~ x, aes(colour="Linear"))  +
  geom_smooth(method = 'loess', formula = y ~ x, aes(colour="Loess")) +
  scale_colour_manual(name="Smoothers", values=c("blue", "red")) + 
  theme_bw() +
  xlab("Average Number of Rooms per Dwelling") +
  ylab("Mean Value of Owner-Occupied Homes") +
  ggtitle("Mean Value of Owner-Occupied Homes and Average Number of Rooms - Boston Dataset") +
  coord_cartesian(xlim=c(3.561, 8.780), ylim=c(0, 50))
p4 <- ggplot(data, aes(x=lstat, y=medv)) +
  geom_point(size=1, alpha=0.7) +
  geom_smooth(method = 'lm', formula = y ~ x, aes(colour="Linear"))  +
  geom_smooth(method = 'loess', formula = y ~ x, aes(colour="Loess")) +
  scale_colour_manual(name="Smoothers", values=c("blue", "red")) + 
  theme_bw() +
  xlab("Lower Status of the Population (percent)") +
  ylab("Mean Value of Owner-Occupied Homes") +
  ggtitle("Mean Value of Owner-Occupied Homes and Lower Status of the Population - Boston Dataset") +
  coord_cartesian(xlim=c(1.7, 38), ylim=c(0, 50))
p5 <- ggplot(data, aes(x=ptratio, y=medv)) +
  geom_point(size=1, alpha=0.7) +
  geom_smooth(method = 'lm', formula = y ~ x, aes(colour="Linear"))  +
  geom_smooth(method = 'loess', formula = y ~ x, aes(colour="Loess")) +
  scale_colour_manual(name="Smoothers", values=c("blue", "red")) + 
  theme_bw() +
  xlab("Pupil-Teacher Ratio by Town") +
  ylab("Mean Value of Owner-Occupied Homes") +
  ggtitle("Mean Value of Owner-Occupied Homes and Pupil-Teacher Ratio - Boston Dataset") +
  coord_cartesian(xlim=c(12.6, 22), ylim=c(0, 50))
p6 <- ggplot(data, aes(x=dis, y=medv)) +
  geom_point(size=1, alpha=0.7) +
  geom_smooth(method = 'lm', formula = y ~ x, aes(colour="Linear"))  +
  geom_smooth(method = 'loess', formula = y ~ x, aes(colour="Loess")) +
  scale_colour_manual(name="Smoothers", values=c("blue", "red")) + 
  theme_bw() +
  xlab("Weighted Mean of Distances to Five Boston Employment Centres") +
  ylab("Mean Value of Owner-Occupied Homes") +
  ggtitle("Mean Value of Owner-Occupied Homes and Mean of Distances to Centres - Boston Dataset") +
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
radbp <- ggplot(data, aes(x=rad, y=medv, group=rad)) + 
  geom_boxplot(fill="white", colour="darkblue", notch=F) +
  geom_jitter(width = 0.2) +
  xlab("Index of Accessibility to Radial Highways") +
  ylab("Mean Value of Owner-Occupied Homes") + 
  ggtitle("Mean Value of Owner-Occupied Homes and Index of Accessibility to Radial Highways")
radbp

### Boxplot pro proměnnou 'chas' ###
is.factor(data$chas)
data$chas <- as.factor(data$chas)
chasbp <- ggplot(data, aes(x=chas, y=medv, group=chas)) + 
  geom_boxplot(fill="white", colour="darkblue", notch=F) +
  geom_jitter(width = 0.2) +
  xlab("Charles River Dummy Variable") +
  ylab("Mean Value of Owner-Occupied Homes") + 
  ggtitle("Mean Value of Owner-Occupied Homes and Index of Accessibility to Radial Highways")
chasbp

### Boxplot pro proměnnou 'rad' s transformací ###
data["newrad"] <- "Low Accessibility"
data$newrad[data$rad < 10] = "High Accessibility"
newradbp <- ggplot(data, aes(x=newrad, y=medv, group=newrad)) + 
  geom_boxplot(fill="white", colour="darkblue", notch=TRUE) +
  geom_jitter(width = 0.2) + 
  xlab("Index of Accessibility to Radial Highways") +
  ylab("Mean Value of Owner-Occupied Homes") + 
  ggtitle("Mean Value of Owner-Occupied Homes and Index of Accessibility to Radial Highways")
newradbp


############
### Q05: ###
############

# nahui


############
### Q06: ###
############

medv_lm <- lm(medv ~ crim, data)
summary(medv_lm)
# Vidíme, že intercept a proměnná 'crim' jsou signifikantní podle p-value, ale koeficient determinace je docela nízký.
# Podíváme se na chování reziduí:
opar <- par(mfrow=c(2,2))
plot(medv_lm)
par(opar)
# Je vidět, že rezidua nejsou rozmístěny symetricky kolem regresní křivky a navíc z Q-Q plotu neplyne jejich normalita.

# Nyní se podíváme na fit modelu:
ggplot(data, aes(x=crim, y=medv)) +
  geom_point(size=1, alpha=0.7) +
  geom_abline(intercept = coef(medv_lm)[1], slope = coef(medv_lm)[2], col='darkred') +
  theme_bw() +
  xlab("Per Capita Crime Rate by Town") +
  ylab("Mean Value of Owner-Occupied Homes") +
  ggtitle("Simple Linear Model Fit") +
  coord_cartesian(xlim=c(0, 90), ylim=c(0, 50))
# Z fitu daného jednoduchého regresního modelu vyplývá, že ceny nemovitiostí klesají v závislosti na míře kriminality
(coef(medv_lm)[1] + coef(medv_lm)[2]*0)-(coef(medv_lm)[1] + coef(medv_lm)[2]*10)
# Vzroste-li kriminalita o 10 jednotek, pak ceny v průměru klesnou o přibližne $4000.


############
### Q07: ###
############

### Uděláme logaritmickou transformaci odezvy 'medv'. ###
logmedv_lm <- lm(log(medv)~ crim, data)
summary(logmedv_lm)
# Koeficient determinace se zvýšil, p-value ukazuje na signifikantnost proměnných.
opar <- par(mfrow=c(2,2))
plot(logmedv_lm)
par(opar)
# Rezidua jsou nyní rozmístěny více symetricky a Q-Q plot také vypadá lépe.
# Stále ale model není ideální.

### Graf s regresní křivkou pro nový model a konfidenčními intervaly na hladině významnosti 5%: ###
new_crime <- data.frame(crim = seq(0,90,0.1779))
new_conf = predict(logmedv_lm, newdata = new_crime, interval = "confidence", level=0.95)
new_pred = predict(logmedv_lm, newdata = new_crime, interval = "prediction", level=0.95)
ggplot(data, aes(x=crim, y=log(medv))) +
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

### Porovnáme naši transformaci s transformací navrženou Box-Coxem:
dev.off()
# Log-věrohodnostní profil Box-Coxovy transformace:
boxcox(medv_lm)
# Nový model s použitím Box-Coxovy transformace.
medv_lm_bc <- update(medv_lm, . ~ boxCoxVariable(medv))
summary(medv_lm_bc)


ggplot(data, aes(x=crim, y=medv)) +
  geom_point(size=1, alpha=0.7) +
  geom_abline(intercept = coef(medv_lm_bc)[1], slope = coef(medv_lm_bc)[2], col="darkblue") + 
  theme_bw() +
  xlab("Per Capita Crime Rate by Town") +
  ylab("Mean Value of Owner-Occupied Homes") +
  ggtitle("Box-Cox Transformation") +
  coord_cartesian(xlim=c(0, 90), ylim=c(1, 50))
