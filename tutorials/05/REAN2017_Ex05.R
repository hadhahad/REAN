################################## 
##### 01REAN Cviceni 5 ##########
#################################
#
# Todays exercise
# Multivariable linear regression in R

library(lattice)
library(MASS)
library(car)
library(ggplot2)


setwd("~/Studies/REAN/tutorials/05")  


# We use again trees data to present Multivariable linear regression 
head(trees)
summary(trees)
trees$Forest <- rbinom(nrow(trees),3,0.5)+1


# Basic scatterplot
pairs(trees,  main="Basic Scatterplot Matrix")                     # all variables in data tree
pairs(~(.), data=trees,  main="Basic Scatterplot Matrix")           # all variables in data tree 
pairs(~Girth+Volume,data=trees,  main="Basic Scatterplot Matrix")  # Girth and Volume variables from data tree

# Scatterplot Matrices  from lattice package
splom(trees)  
splom(trees, groups=trees$Forest )  
splom(trees[c("Girth","Height","Volume")], groups=trees$Forest )  


# Scatterplot Matrices from the car package
scatterplotMatrix(~(.)|Forest, data=trees,   main="Three Cylinder Options")
# tune colors and remove regression lines
library(RColorBrewer)
my_colors <- brewer.pal(nlevels(as.factor(trees$Forest)), "Set2")
scatterplotMatrix(~(.)|Forest, data=trees , 
                  reg.line="" , smoother="", col=my_colors , smoother.args=list(col="grey") ,
                  cex=1.5 , pch=c(15,16,17) , main="Scatter plot with Three Cylinder Options")


# xyplot
xyplot(Volume ~ Girth | Forest , data=trees , pch=20 , cex=2 , col="blue" )
### 4x Forest kvuli tomu, ze Forest neni factor (kategorcika promenna)
# Forest as factor
summary(trees)
trees$Forest <- as.factor(trees$Forest)
# Different 
xyplot(Volume ~ Girth | Forest , data=trees , pch=20 , cex=2 , col="red" )


# How to devide plot into two parts and have only one title
#Divide the screen in 1 line and 2 columns

#Make the margin around each graph a bit smaller
op <- par(mfrow=c(1,2),oma = c(0, 0, 2, 0))
    # oma is a vector of the form c(bottom, left, top, right)
    # giving the size of the outer margins in lines of text.
#Classical histogram and density
hist(trees$Volume,  main="" , breaks=10 , col="gray" , xlab="Volume" , ylab="Number of trees in each bin")
hist(trees$Volume, freq=F,  breaks=10, main="", xlab="Volume",xlim=c(10, 80),ylim=c(0, 0.05))
lines(density(trees$Volume), col="red", lwd=2)
#Add only ONE title :
mtext("Histogram and Density plot of Volume", outer = TRUE, cex = 1.4, font=4, col=rgb(0.1,0.3,0.5,0.5) )
## At end of plotting, reset to previous settings:
par(op)



# How to split screen and make nice plots :)
# Divide the screen in 2 line and 1 column only
new_screen_step1 <- split.screen(c(2, 1))
# Add one graph on the screen number 1 which is on top :
screen(new_screen_step1[1])
plot( Volume~Girth , data=trees, pch=20 , xlab="Girth",ylab="Volume", cex=1 , col="black" )
# I divide the second screen in 2 columns :
new_screen_step1=split.screen(c(1, 2), screen = new_screen_step1[2])
screen(new_screen_step1[1])
hist(trees$Girth , breaks = 8, border=F , col="blue" , main="" , xlab="Distribution of Girth")
screen(new_screen_step1[2])
hist(trees$Volume, breaks = 8, border=F , col="red" , main="" ,  xlab="Distribution of Volume")



table(trees$Height)
trees$Height.f<-cut(trees$Height, seq(60,90,10), right=FALSE) 
# cut creates cathegorical variable
is.factor(trees$Height.f)

# ggplot version of scatterplot, where Color and shape depend on factor variable
# multipolot with glot is different
# more possibilities exist, here is only one
require(gridExtra)
plot1 <- ggplot(trees, aes(x=Girth, y=Volume, color=Forest, shape=Forest)) + 
    geom_point(size=5, alpha=0.8)  +   theme_bw() 
plot2 <- ggplot(trees, aes(x=Girth, y=Volume, color=Height.f, size=Height.f)) + 
    geom_point(alpha=0.6) +   theme_bw() 
grid.arrange(plot1, plot2, ncol=2)

# Reset plot settings
dev.new()
dev.off()


##############################
# Lets start with regression


#### Model selection by adding varibles #########
trees_lm1.0  <- lm(Volume ~ (.), data = trees)  # use all what I have in the dataframe
trees_lm1.0  <- lm(Volume ~ (.)^2, data = trees)  # use all what I have in the dataframe 
                                                # with second order interactions

# uff - lets start from the simplest model
# only Girth and Height as a independent variable
trees_lm1.0  <- lm(Volume ~ Girth + Height, data = trees)  # use all what I have in the dataframe
# nearly all information is in summary
summary(trees_lm1.0)

# Constuct X,Y, beta_hat - if you want to analyze it by hand
n = nrow(trees)
p = length(coefficients(trees_lm1.0))
beta_hat = as.matrix(coefficients(trees_lm1.0))
X        = as.matrix(cbind(rep(1,times = n), trees[,c(1,2)]))
Y        = as.matrix(trees[,3])
residuals_lm1.0 = Y - X%*%beta_hat
((residuals_lm1.0 - residuals(trees_lm1.0))<0.00001)
# and so on 


summary(trees)

trees_new <- data.frame(Girth = (seq(5,25,0.1)), Height = seq(60,90,length=length(seq(5,25,0.1))))
conf_new  <- predict(trees_lm1.0, newdata = trees_new, interval = "confidence")
pred_new  <- predict(trees_lm1.0, newdata = trees_new, interval = "prediction")
height_lim = seq(60,90,length=length(seq(5,25,0.1)))

op <- par(mfrow=c(1,2),oma = c(0, 0, 2, 0))
plot(Volume ~ Girth, data = trees, xlim = c(5,25), ylim = c(5,100),pch=20, col = "black", xaxs="i",yaxs="i",
     main="Trees data: Girth and Volume dependence",xlab="Girth", ylab="Volume")
lines(seq(5,25,0.1), pred_new[,1], col='black')
lines(seq(5,25,0.1), pred_new[,2], col='red')
lines(seq(5,25,0.1), pred_new[,3], col='red')
lines(seq(5,25,0.1), conf_new[,2], col='blue')
lines(seq(5,25,0.1), conf_new[,3], col='blue')
legend("topleft",legend=c("observed","fit","Confidence int","Prediction int"),
       pch=c(20,NA,NA,NA),lty = c(NA,1,1,1),col = c("black","black","blue","red"))
plot(Volume ~ Height, data = trees, xlim = c(60,90), ylim = c(5,100),pch=20, col = "black", xaxs="i",yaxs="i",
     main="Trees data: Height and Volume dependence",xlab="Height", ylab="Volume")
lines(height_lim, pred_new[,1], col='black')
lines(height_lim, pred_new[,2], col='red')
lines(height_lim, pred_new[,3], col='red')
lines(height_lim, conf_new[,2], col='blue')
lines(height_lim, conf_new[,3], col='blue')
legend("topleft",legend=c("observed","fit","Confidence int","Prediction int"),
       pch=c(20,NA,NA,NA),lty = c(NA,1,1,1),col = c("black","black","blue","red"))
par(op)


require(scatterplot3d) # for 3d scatter plot
# Another design of scatterplot3d
s3d <-with(trees,scatterplot3d(Girth, Height, Volume, pch=16,
                               highlight.3d=TRUE, type="h",
                               main="3D Scatter Plot with Vertical Lines and Regression Planes",
                               angle=135,scale.y=1, xlab="Girth",ylab="Height",zlab="Volume",
                               cex.lab=1.5, cex.axis=1.5, cex.main=1.1, cex.sub=1.5))
s3d$plane3d(trees_lm1.0)


# Comperable statistics of the model (See lecture 5)
summary(trees_lm1.0)$r.squared
summary(trees_lm1.0)$adj.r.squared
summary(trees_lm1.0)$fstatistic


# polynomila Regression without scaled varialbes 
trees_lm2.0 <- lm(Volume ~ (Girth) + I((Girth)^2), data = trees)
summary(trees_lm2.0)

# polynomial Regression with scaled varialbes
trees_lm2.1 <- lm(Volume ~ scale(Girth) + I(scale(Girth)^2), data = trees)
summary(trees_lm2.1)
plot(Volume ~ scale(Girth), data = trees)
lines(fitted(trees_lm2.1) ~ scale(Girth), data = trees)

conf_new  <- predict(trees_lm2.1, newdata = trees_new, interval = "confidence")



# polynomial Regression with scaled varialbe Girth and add Height
trees_lm2.2 <- lm(Volume ~ scale(Girth) + I(scale(Girth)^2) + Height, data = trees)
summary(trees_lm2.2)




# polynomial Regression with scaled varialbe Girth and add Height and interaction 
trees_lm2.3 <- lm(Volume ~ Height*scale(Girth) + I(scale(Girth)^2) , data = trees)
summary(trees_lm2.3)
# :(


# Qualitative Explanatory Variables - Forest
summary(trees)
trees$Tall <- cut(trees$Height, breaks = c(-Inf, 76, Inf), labels = c("no", "yes"))
treesTall  <- split(trees, trees$Tall)
trees_lm_Tall <- lm(Volume ~ Girth + Tall, data = trees)
summary(trees_lm_Tall)
treesTall[["yes"]]$Fit <- predict(trees_lm_Tall, treesTall[["yes"]])
treesTall[["no"]]$Fit <- predict(trees_lm_Tall, treesTall[["no"]])
plot(Volume ~ Girth, data = trees, type = "n")
points(Volume ~ Girth, data = treesTall[["yes"]], pch = 1)
points(Volume ~ Girth, data = treesTall[["no"]], pch = 2)
lines(Fit ~ Girth, data = treesTall[["yes"]])
lines(Fit ~ Girth, data = treesTall[["no"]])



#### Model Selection by step function ###########
trees_lm1.0 <- lm(Volume ~ (.), data = trees)
summary(trees_lm1.0)

# Choose a model by AIC in a Stepwise Algorithm
trees_lm1.1 <- step(trees_lm1.0)
summary(trees_lm1.1)

AIC(trees_lm1.1)
nrow(trees)*(1+log(2*pi*(summary(trees_lm1.1)$sigma)^2)) + (length(coefficients(trees_lm1.1))+2)



require(scatterplot3d) # for 3d scatter plot
# Another design of scatterplot3d
s3d <-with(trees,scatterplot3d(Girth, Height, Volume, pch=16,
                    highlight.3d=TRUE, type="h",
                    main="3D Scatter Plot with Vertical Lines and Regression Planes",
                    angle=135,scale.y=1, xlab="Girth",ylab="Height",zlab="Volume",
                    cex.lab=1.5, cex.axis=1.5, cex.main=1.1, cex.sub=1.5))
#s3d$plane3d(yield.lm)



# Investigate the "clouds" data, from package HSAUR2, 
# collected in the summer of 1975 from an experiment
# to investigate the use of massive amounts of silver iodide
# in cloud seeding to increase rainfall.

install.packages("HSAUR2")
library(HSAUR2)
summary(clouds)

# Example
clouds_lm1 <- lm( rainfall ~ (.)^2, data = clouds)
summary(clouds_lm1)

# Find suitable model describing rainfall as a response variable

# Plot the relationship between rainfall and other variables + add estimated regression lines

# Check: the assumptions of OLS (by hypothesis testing and graphical diagnostic tools):
#     constant variance - homoscedasticity, missing autocorrelation
#           plot: residuals against each explanatory variable, against order of measurement,
#                 against fitted values 
#     normality of error terms, normal probability plot of the residuals.
#     identify outliers, remove them and run the analysis again 

clouds_lm1 <- step(clouds_lm1)
pairs(clouds)

clouds_lm2 <- lm( rainfall ~ time + prewetness + sne + cloudcover, data = clouds)
summary(clouds_lm2)

plot(rainfall ~ prewetness, data = clouds, col = "black", xaxs="i",yaxs="i",
     main="Clouds data: Rainfall and Time dependence",xlab="Time", ylab="Rainfall")

summary(clouds)

clouds_new <- data.frame(time = (seq(0,85,1)), prewetness = seq(0,1.5,length=length(seq(0,85,1))), sne = seq(1,5,length=length(seq(0,85,1))), cloudcover = seq(2,40,length=length(seq(0,85,1))))
conf_new  <- predict(clouds_lm2, newdata = clouds_new, interval = "confidence")
pred_new  <- predict(clouds_lm2, newdata = clouds_new, interval = "prediction")
# height_lim = seq(60,90,length=length(seq(5,25,0.1)))

op <- par(mfrow=c(1,2),oma = c(0, 0, 2, 0))
plot(rainfall ~ time, data = clouds, xlim = c(0,40), ylim = c(0,15),pch=20, col = "black", xaxs="i",yaxs="i",
     main="Clouds data: Time and Rainfall dependence",xlab="Time", ylab="Rainfall")
lines(seq(0,85,1), pred_new[,1], col='black')
lines(seq(0,85,1), pred_new[,2], col='red')
lines(seq(0,85,1), pred_new[,3], col='red')
lines(seq(0,85,1), conf_new[,2], col='blue')
lines(seq(0,85,1), conf_new[,3], col='blue')
legend("topleft",legend=c("observed","fit","Confidence int","Prediction int"),
       pch=c(20,NA,NA,NA),lty = c(NA,1,1,1),col = c("black","black","blue","red"))
plot(Volume ~ Height, data = trees, xlim = c(60,90), ylim = c(5,100),pch=20, col = "black", xaxs="i",yaxs="i",
     main="Trees data: Height and Volume dependence",xlab="Height", ylab="Volume")
lines(height_lim, pred_new[,1], col='black')
lines(height_lim, pred_new[,2], col='red')
lines(height_lim, pred_new[,3], col='red')
lines(height_lim, conf_new[,2], col='blue')
lines(height_lim, conf_new[,3], col='blue')
legend("topleft",legend=c("observed","fit","Confidence int","Prediction int"),
       pch=c(20,NA,NA,NA),lty = c(NA,1,1,1),col = c("black","black","blue","red"))
par(op)



