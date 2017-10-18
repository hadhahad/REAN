trees
trees$BMI = (trees$Volume * 900)/(trees$Height)^2
# trees[BMI] <- (trees$Volume * 900) / trees$Height
as.factor(trees$BMI)
# trees$BMI <- factor(trees$BMI, labels=c("tenke","stredni","silne"))
summary(trees$BMI)
trees["obezita"] <- "normal"
trees$obezita[trees$BMI < 3.117] = "tenky"
trees$obezita[trees$BMI < 6.216] = "silny"
summary(trees)
trees$obezita <- as.factor(trees$obezita)
