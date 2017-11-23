################################## 
##### 01REAN Cviceni 02 ##########
#################################
#
# Pokracovani v rychlem uvodu do R
# Dnesni cviceni bude zamereno na:
#  - opakovani podminek, cyklu, funkci
#  - prace s data frame
#  - prace s faktory
#  - zakladni grafika (boxplot, scatterplot, qqplot, ...)
#
# - ukol zadan na slidech

# knihovny ktere dnes (mozna) vyuzijem :)
library(car)     
library(lattice)
library(pwr)   
library(MASS)
library(gplots)
library(ggplot2)


# loops, 

#simulace hazeni minci (T = tail, H = head)
x <- sample(c(0,1), 100, rep=T)
table(x)
head(ifelse(x==0, "T", "H"))
sum(as.logical(x))


n <- 500
v <- w <- z <- numeric(n)
for (i in 1:n) {
     t <- runif(1)
     u <- runif(1)
     v[i] <- 2 * t - 1
     w[i] <- 2 * u - 1
     a <- v[i]^2 + w[i]^2
     z[i] <- ifelse(a <=1 , a, NA)
     }
round(summary(z), 2)
is.na(z)

rbinom()
toss <- function(n=5, p=0.5) sum(rbinom(n, 1, p))
toss()
toss(20)
toss(20, 0.75)
toss(p=0.75)
replicate(10, toss(n=100))
sapply(10:20, toss)
as.numeric(Map("toss", n=10:20))





# create own dataset - dataframe

pet     = rep(c("cat","dog","fish","other","none"),times=8)
house   = rep(c("bungalow","villa", "row_house", "apartment"),each=10) 
members = rbinom(40,6,0.3)+1
income  = rexp(40,1/19)+11
area    = abs(rnorm(40,100,90))+20

dat    <- data.frame(pet,house,members,income,area)
dim(dat)
summary(dat)

data           <- matrix(c(pet,house,members,income), nc=5)
data$pet #try
colnames(data)
colnames(data) <- c("pet","house","members","income","area")
is.array(data)
is.data.frame(data)
data <- as.data.frame(data)




# Summarizing the dataset

# We use birthwt: dataset from the core packages
?birthwt          # Description + info
head(birthwt)     # first lines

#Copy birthwt to another data.frame that we will change
birth = birthwt
summary(birth)  # summary (we can recognize factors with coding and numerical values)
str(birth)

colSums(is.na(birth))
colMeans(birth)
apply(birth, 2, function(x) sum(is.na(x)))  # 2 - columnwise , 1 -rowwise
apply(birth, 2, function(x) mean(x))

dim(birth)

# Check again the description of each variable and recode them
# unordered factors

#we can change one by one
summary(birth$race)
as.factor(birth$race)
summary(as.factor(birth$race))
birth$race  <- factor(birth$race, labels=c("White","Black","Other"))
summary(birth$race)
                      
birth <- within(birth,{
    low   <- factor(low, labels=c("No","Yes"))
    race  <- factor(race, labels=c("White","Black","Other"))
    smoke <- factor(smoke, labels=c("No","Yes"))
    ui    <- factor(ui, labels=c("No","Yes"))
    ht    <- factor(ht, labels=c("No","Yes"))})

lapply(birth, is.numeric)
num_varaibles <- sapply(birth, is.numeric)
print(num_varaibles)

# check the difference in apply functions:
# lapply and sapply: avoiding loops on lists and data frames
# tapply: avoiding loops when applying a function to subsets
# mapplay, rapplay

par(mfrow=c(1,1))

birth[,sapply(birth, is.numeric)]
boxplot(apply(birth[,num_varaibles], 2, scale)) #(z-scores)

boxplot(birth$age ~ birth$race)
plot(birth$age,birth$race)
plot(birth$race,birth$age)
with(birth,plot(race,age))

ggplot(birth, aes(x=race, y=bwt, fill=smoke)) +
    geom_boxplot(size = 1, notch = T) +
    xlab("Race") +
    ylab("Birth weight") +
    theme_bw() +  
    geom_jitter(aes(race,bwt),
                position=position_jitter(width=0.3,height=0),
                alpha=0.6,
                size=1,
                show.legend=F)

scatterplot(birth$bwt,birth$age)

pairs(birth[,num_varaibles])


library(Hmisc)
describe(birthwt)
summary(low ~ ., data=birth, method="reverse", overall=TRUE)
summary(bwt ~ ., data=birth)
summary(low ~ smoke + ht + ui, data=birth, fun=table)

summaryRc(bwt ~ age , bpplot='top', datadensity=FALSE, trim=.01,data=birth)
summaryRc(bwt ~ age , bpplot='top', datadensity=T, trim=.01,data=birth)
with(birth, plot(age,bwt))



s <- summaryS(bwt ~ age  + race + smoke,  data=birth)
plot(s)  
plot(s, groups='race', datadensity=TRUE, scat1d.opts=list(lwd=.5, nhistSpike=0))



hist(birth$bwt)
hist(birth$bwt, breaks=seq(500, 5500, by=500))
table(cut(birthwt$bwt, breaks=seq(500, 5500, by=500), include.lowest=T, dig.lab=4))

with(birth,hist(bwt, freq=FALSE))
with(birth,hist(bwt, freq=T))


hist(birth$bwt, freq=F, main="", xlab="Birth weight",xlim=c(0, 6000), ylim=c(0, 0.0006))
lines(density(birth$bwt), col="red", lwd=2)
x=seq(0,6000,length=1000)
y=dnorm(x,mean=mean(birth$bwt),sd=sd(birth$bwt))
lines(x,y,col="blue", lwd=2, lty = 10)


qqnorm(birth$bwt, main="")
qqline(birth$bwt)




y1      = birth$bwt[birth$race == "Black"]
y2      = birth$bwt[birth$race == "White"]
y       = c(y1,y2)
y.means = c(mean(y1),mean(y),mean(y2))
y.sd    = c(sd(y1),sd(y),sd(y2))
min(y)
max(y)
max.len = max(length(y1), length(y2))
y1 = c(y1, rep(NA,  length(y) - length(y1)))
y2 = c(y2, rep(NA, length(y) - length(y2)))

opar    = par(mfrow=c(2,1),mar=c(5,7,4,2),las=1)
stripchart(data.frame(Black=y1,Together=y,White=y2),xlab=expression("Birth weight"),pch=19,xlim = c(1000,5000))
arrows(c(y.means,y.means[2]),c(1.5,1.5,2.5,2.5),c(y.means,y.means[2]),c(1.1,1.9,2.9,2.1),length=.1)
text(y.means,c(1.2,1.8,2.8),round(y.means,2),pos=4,cex=.8)
rd = rnorm(100,mean=y.means[2],sd=y.sd[2])        # if y is data frame: sd = as.numeric(apply(y,2,sd))
hist(y,xlab="Observations",ylab="Relative frequency", main="Histogram for Birth weight", breaks = 15, freq = FALSE, ylim=c(0,0.001), xlim = c(1000,5000),dig.lab=4 )
partition = seq(min(rd ), max(rd ), 0.01)
lines(partition, dnorm(partition, y.means[2], y.sd[2]), col = "red")
lines(partition, dnorm(partition, y.means[1], y.sd[1]), col = "blue")
lines(partition, dnorm(partition, y.means[3], y.sd[3]), col = "blue")
par(opar)



bwt.by.race <- with(birth, tapply(bwt, race, median))
barplot(bwt.by.race, ylab="Median birth weight")


bwt.by.race.mean <- with(birthwt, tapply(bwt, race, mean))
bwt.by.race.sd <- with(birthwt, tapply(bwt, race, sd))
bp <- barplot(bwt.by.race.mean, ylab="Mean birth weight",
                   ylim=c(0,4000), col="coral", border=NA)
arrows(bp, bwt.by.race.mean-bwt.by.race.sd,
       bp, bwt.by.race.mean+bwt.by.race.sd,
       code=3, length=.1, angle=90)
# see also segments()


library(lattice)
with(birth,plot(age, bwt, col=race))
with(birth,xyplot(bwt ~ age, groups=race))
with(birth,xyplot(bwt ~ age | race,  layout=c(3,1)))



dev.off()
graphics.off()
#opar <- par(mfrow=c(2,1))
#par(opar)


