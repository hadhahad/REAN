####################
## TREES ANALYSIS ##
####################

# Factoring

trees
trees$BMI = (trees$Volume * 900)/(trees$Height)^2
as.factor(trees$BMI)
summary(trees$BMI)
trees["Obesity"] <- "Normal"
trees$Obesity[trees$BMI < 3.117] = "Thin"
trees$Obesity[trees$BMI > 6.216] = "Thick"
summary(trees)
trees$Obesity <- as.factor(trees$Obesity)

# Boxplots

par(mfrow = c(1, 2))
num_varaibles <- sapply(trees, is.numeric)
# boxplot(apply(trees[,num_varaibles], 2, scale)) 
boxplot(trees$Height ~ trees$Obesity, ylab="Height"); boxplot(trees$Girth ~ trees$Obesity, ylab="Girth")

# ggplot

ggplot(trees) +
  geom_boxplot(aes(x=Obesity, y=Height), size = 1, notch = F) +
  xlab("BMI") +
  ylab("Girth & Height") +
  theme_bw() +  
  geom_jitter(aes(Obesity,Height),
              position=position_jitter(width=0.3,height=0),
              alpha=0.6,
              size=1,
              show.legend=F)+
  geom_boxplot(aes(x=Obesity, y=Girth), size = 1, notch = F) +
  theme_bw() +  
  geom_jitter(aes(Obesity,Girth),
              position=position_jitter(width=0.3,height=0),
              alpha=0.6,
              size=1,
              show.legend=F)

# Dependancies

par(mfrow = c(1, 1))
plot(trees$Height ~ trees$Girth, xlab="Girth", ylab="Heigth")
plot(trees$Height ~ trees$Volume, xlab="Girth", ylab="Volume")
pairs(trees$Volume ~ (trees$Girth + trees$Height), labels=c("Volume", "Girth", "Height"))

# qqplots

qqnorm(trees$Height, main="Normal Q-Q Plot for Height")
qqline(trees$Height)
qqnorm(trees$Volume, main="Normal Q-Q Plot for Volume")
qqline(trees$Volume)
qqnorm(trees$Girth, main="Normal Q-Q Plot for Girth")
qqline(trees$Girth)

# Histograms

hist(trees$Height, freq=F, main="", xlab="Height",xlim=c(60, 90), ylim=c(0, 0.08))
lines(density(trees$Height), col="red", lwd=2)
x=seq(60,90,length=1000)
y=dnorm(x,mean=mean(trees$Height),sd=sd(trees$Height))
lines(x,y,col="blue", lwd=2, lty = 10)
