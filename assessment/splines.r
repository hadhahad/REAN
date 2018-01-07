## Instruktivní cvičení o používání splinů v regresi

library(splines)

## body pro počítání bází
points <- seq(0,100,by=0.1)

## uzly
knots <- c(10,25,60,70,90)

## utvoříme kubickou bázi ve stanovených bodech (points)
s.bas <- bs(points, knots = knots, degree = 3, Boundary.knots = c(0,100),
            intercept=T)

## Zařadili jsme intercept=T -- to znamená, že se vytvoří všechny
## bázové funkce (jsou lineárně závislé, jak se přesvedčíme níže)

cbind(points,s.bas)[1:25,]
cbind(points,s.bas)[975:1001,]

## nakreslíme bázové funkce
plot(points,s.bas[,1],type='l',axes=F,ylim=c(0,1))
axis(1,at=knots)
axis(2)
box()
for(i in 2:ncol(s.bas))
  {
    lines(points,s.bas[,i])
  }

## bázové funkce v každém bodě se nasčítají do 1
sumbas <- apply(s.bas,1,sum)
sum(sumbas==1)

## teď to tak nevypadá, ale...
sum(abs(sumbas-1)>1e-6)
sum(abs(sumbas-1)>1e-13)
max(abs(sumbas-1))
## bylo to jen kvůli numerickým nepřesnostem



## nyní zkusíme lineární bázi
s.bas <- bs(points, knots = knots, degree = 1, Boundary.knots = c(0,100),
            intercept=T)

cbind(points,s.bas)[1:25,]
cbind(points,s.bas)[975:1001,]


plot(points,s.bas[,1],type='l',axes=F,ylim=c(0,1))
axis(1,at=knots)
axis(2)
box()
for(i in 2:ncol(s.bas))
  {
    lines(points,s.bas[,i])
  }



## A jak se to dělá na datech?

## Vezmeme nějakou nelineární funkci
f <- function(x){x/50+3*dnorm((x-75)/25)}

## Tohle bude regresor
x <- runif(350,0,150)

## Graf zkoumané funkce
y <- f(x)

plot(x,y)

## Teď přidáme náhodný šum
y <- y+rnorm(350,sd=0.5)
  
plot(x,y)

lines(lowess(x,y))
## příliš vyhlazené



## Linearni model
mod1 <- lm(y ~ x)
summary(mod1)

## nakreslíme si odhadnutou regresní přímku a odhadovanou funkci
plot(x,y)

xbody <- seq(0,150,n=500)
fitted <- predict(mod1,data.frame(x=xbody))
lines(xbody,fitted)
lines(xbody,f(xbody),lty=2)

## residuální graf
plot(x,resid(mod1))
lines(lowess(x,resid(mod1)))



## Faktorovy model
(cbody <- c(-Inf,seq(25,125,by=25),Inf))
mod1a <- lm(y ~ factor(cut(x,cbody)))
summary(mod1a)

## nakreslíme si odhadnutou regresní přímku a odhadovanou funkci
plot(x,y)

xbody <- seq(0,150,n=500)
fitted <- predict(mod1a,data.frame(x=xbody))
lines(xbody,fitted)
lines(xbody,f(xbody),lty=2)

## residuální graf
plot(x,resid(mod1a))
lines(lowess(x,resid(mod1a)))



## Kubický model
mod2 <- lm(y ~ poly(x,3))
summary(mod2)

anova(mod2,mod1)

plot(x,y)
xbody <- seq(0,150,n=500)
fitted <- predict(mod2,data.frame(x=xbody))
lines(xbody,fitted)
lines(xbody,f(xbody),lty=2)

plot(x,resid(mod2))
lines(lowess(x,resid(mod2)))


## Polynom 6. řádu
mod2a <- lm(y ~ poly(x,6))
summary(mod2a)

anova(mod2a,mod2)

plot(x,y)
xbody <- seq(0,150,n=500)
fitted <- predict(mod2a,data.frame(x=xbody))
lines(xbody,fitted)
lines(xbody,f(xbody),lty=2)

plot(x,resid(mod2))
lines(lowess(x,resid(mod2)))




## Kubický spline
knots <- c(50,80,120)

xspl <- bs(x,knots=knots,degree=3,Boundary.knots=c(0,150))

mod3 <- lm(y ~ xspl)
summary(mod3)

anova(mod3,mod2)
anova(mod3,mod1)

plot(x,y)
xbody <- seq(0,150,n=500)
xsplnew <- bs(xbody,knots=knots,degree=3,Boundary.knots=c(0,150))

fitted <- coef(mod3)[1]+xsplnew%*%coef(mod3)[-1]
lines(xbody,fitted)
lines(xbody,f(xbody),lty=2)



mod3 <- lm(y ~ bs(x,knots=knots,degree=3,Boundary.knots=c(0,150)))
plot(x,y)
xbody <- seq(0,150,n=500)
fitted <- predict(mod3,data.frame(x=xbody))
lines(xbody,fitted)
lines(xbody,f(xbody),lty=2)

summary(mod3)

plot(x,resid(mod3))
lines(lowess(x,resid(mod3)))


## Lineární spline
knots <- c(20,80,125)

xspl <- bs(x,knots=knots,degree=1,Boundary.knots=c(0,150))

mod4 <- lm(y ~ xspl)
summary(mod4)

anova(mod4,mod3)
anova(mod4,mod1)

plot(x,y)
xbody <- seq(0,150,n=500)
xsplnew <- bs(xbody,knots=knots,degree=1,Boundary.knots=c(0,150))

fitted <- coef(mod4)[1]+xsplnew%*%coef(mod4)[-1]
lines(xbody,fitted)
lines(xbody,f(xbody),lty=2)


