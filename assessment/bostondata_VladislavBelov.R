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
