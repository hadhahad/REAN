View(faithful)

data <- faithful
View(data)

e_mean <- mean(data$eruptions)
w_mean <- mean(data$waiting)
e_sd <- sd(data$eruptions)
w_sd <- sd(data$waiting)
ew_cor <- cor(data)[1,2]

b2_hat <- ew_cor * e_sd / w_sd
b1_hat <- e_mean - b2_hat*w_mean
beta_hat = rbind(b1_hat,b2_hat)
beta_hat

# do the same with differnet notation
intercept <- matrix(1,length(data$eruptions),1)
X         <- (data$waiting)
IX        <- cbind(intercept,data$waiting)
Y         <- (data$eruptions)

b2_hat    <- cor(X,Y) * (sd(Y)/sd(X))
b2_hat
b1_hat    <- mean(Y) - b2_hat*mean(X)
b1_hat

# find residuals
# %*% je maticove nasobeni
res =  Y - IX%*%beta_hat

# estimate variance of disturbances sigma^2
n=dim(fsdata)[1] # number of observations
p=1              # number of regression coefficients - No. of DF
sigma <- sqrt((1/(n-1-p))*sum((Y - IX%*%beta_hat)^2))

# same diff notation
MSE  <- sum(res^2)/(n-2) # the same as sigma
RMSE <- sqrt(MSE)   

#  variance of parameters
var_b2_hat <- sigma^2*(1/sum((X - mean(X))^2))
# <- sigma^2*(1/((n-1)*var(X))) #the same
sd_b2_hat   <- sqrt(var_b2_hat)
var_b2_hat
sd_b2_hat


var_b1_hat <- sigma^2*(sum(X^2)/(n*sum((X - mean(X))^2)))
sd_b1_hat   <- sqrt(var_b1_hat)
var_b1_hat
sd_b1_hat

# Compute coefficient of determination
# R-squared = Explained variation / Total variation
SS_tot = sum((Y - mean(Y))^2)
SS_reg = sum((IX%*%beta_hat - mean(Y))^2)
SS_res = sum(res^2)
SS_tot == SS_res+SS_reg

R2 = 1-SS_res/SS_tot
R2
R2 = SS_reg/SS_tot
R2

# without intercept
solve(t(X)%*%(X))%*%(t(X)%*%Y)

# with intercept
solve(t(IX)%*%(IX))%*%(t(IX)%*%Y)

############################
#### Use R lm-linear model function !!!

model0 = lm(eruptions ~ -1 + waiting ,data) # -1 for intercept
summary(model0)

model1 = lm(eruptions ~ waiting,data)
summary(model1)

# study summary function for lm
help(summary.lm)



# plot data with regression line
plot(eruptions ~ waiting,data,
     main="Eruptions and Waiting Time",xlab="Waiting Time between Eruptions", ylab="Number of Eruptions")
abline(model0,col ="blue")
abline(model1, col ="red")
par(xpd=TRUE)
legend("topleft",legend = c("without intercept","with intercept"),lty = c(1,1),col = c("blue","red"))

