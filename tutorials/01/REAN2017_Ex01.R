################################## 
##### 01REAN Cviceni 01 ##########
#################################
#
# Poznamka na uvod: Text je psan bez hacku a cerek kvuli ruznym kodovanim
#                   (Jinak jako vychozi doporucuji UTF-8)
#
#
# V pocitacove ucebne na KM spustte program R-Studio.
# Je to editor pro praci s R a veskere potrebne veci jsou tam jiz nastaveny.
# V jednom z oken je spustena konzole, kam muzete primo psat prikazy, ktere ma R vykonat.
# Konzoli poznate nejen podle jmena okna (Console), ale i podle vyvolavaciho symbolu >
#
# Pokud vami zadany prikaz nedokoncite a predcasne spustite pomoci ENTER, program zahlasi +
# a ceka na dokonceni prikazu.
# Jiz provedene prikazy lze zpetne vyvovalt postupnym stisknutim kurzorove sipky nahoru.
#
# Zalozte si ve svem /home/ prisusny adresar, zkopirujte do nej tento kod a 
# nastavte ho jako pracovni pomoci prikazu:
#(upravte prikaz podle sve cesty)  
 
setwd("/home/hadhahad/Studies/REAN/")  

#	zkontrolujte nastavení pracovni slozky zkontrolujete pomoci prikazu 
   getwd()

# Zadejte nekolik prikazu s ruzymi matematickymi operacemi primo do konzole
#
# 40+2
# 50-8
# 21*2
#   
# Mnohem praktictejsi je ale psat prikazy do douboru scriptu, 
# ktery je mozno ulozit, opakovane spustit, psat k nemu komenty atd.
# Naprikad tento dokument nesouci nazev REAN2017_Ex01.R
   
# Pozn. znak # uvozuje komentar 
   
# Ze souboru odesleme oznacenou cast kodu do scriptu pomoci prikazu
#   Ctrl+R 
# pokud neni oznacen zadny text odesle se radek, kde je kurzor.
   
#Default package
(getOption("defaultPackages"))  

# informace o spustene session vcetne base balicku   
sessionInfo()
   
# vycet vsech base a recommended balicku
subset(as.data.frame(installed.packages()), Priority %in% c("base","recommended"), select=c(Package, Priority))
   
# vycet vsech nainstalovanych a dostupnych knihoven v pocitaci   
library()
   
# Jak stahnout a nainstalovat knihovnu - balicek (package)
# Priklad balicku "car" - Companion to Applied Regression
?install.packages
install.packages("car")
library(car)
  


# napoveda ke knihovne
library(help = "datasets")   # priority: base
library(help = "MASS")      # priority: recommended

# instalujte balicek MASS, nasteteho a projdete si k nemu help

vignette(all = T)
vignette("embedding", package = "car")


#Priklady dat z balicku datasets
Titanic
WWWusage



######################################
## Aritmetika - R jako kalkulacka
40 + 2              # scitani
44 - 2              # odcitani
6  * 7              # nasobeni
294 / 7             # deleni
42^7                # mocnina
sqrt(1764)          # druha odmocnina
230539333248^(1/7)  # (od)mocnina, zavorky nutne

#Konstanty a funkce 
exp(1)             # eulerovo cislo
exp(42)            # exponenciela
pi                 # konstanta pi
sin(pi/2)          # sinus 
log(exp(1))        # přirozený logaritmus (pozor! jinde se casto značí ln)
ln(exp(1))         # funkci ln R nezna !
factorial(42)      # 42! 
choose(5, 2)       # 5 nad 2 = kombinacni cislo
10/3
options(digits = 15) # chcili zobrazit vice cislic
10/3
options(digits = 7)  # zpet na 7


######################################
### Prace s promennymi

x <- 42    # ulozeni hodnoty do 
x	       # vytisteni hodnoty v promenne ulozene
x = 42     # jiny zpusob, ale  nelze pouzit v kombinaci s jinym prikazem
y <- 3    # uložení do jiné proměnné
x + y    
z <- x + y

print(z <- x + y)
print(z = x + y)	 

### Vymazani promennych
### --------------
ls()          # vypis pouzivanych a definovanych objektu
rm(list=ls()) # vymaze vsechn objekty 
ls() 


##### Typy objektů
#  integer, double: real numbers (rational and irrational)
#  character, logical: includes TRUE, FALSE,
#  NA stands for “not available”, i.e., a missing value.

sqrt(-1) # isn't defined
sqrt(-1+0i) # is defined
sqrt(as.complex(-1)) # same thing
(0 + 1i)^2 # should be -1
typeof((0 + 1i)^2)

x <- (0 + 1i)^2
x
y <- as.numeric(x)
y
class(y)
class(x)
y == x


##### Vectors
a <- c(1,2,5.3,6,-2,4)                   # numeric vector
b <- c("one","two","three")              # character vector
c <- c(TRUE,TRUE,TRUE,FALSE,TRUE,FALSE)  #logical vector

#dalsi vektory:
seq(from = 1, to = 5)
seq(from = 2, by = -0.1, length.out = 4)
1:5
[1] 1 2 3 4 5
x <- c(74, 31, 95, 61, 76, 34, 23, 54, 96)
x[2:4]
x[c(1, 3, 4, 8)]
x[-c(1, 3, 4, 8)]
LETTERS[1:5]
letters[-(6:24)]



#### data frame
trees
head(trees)     # prvnich par radku
summary(trees)  # prehled promennych
table(trees[,"Girth"])
str(trees)

Girth           # nezna
trees$Girth	    # takto nahlidneme na prvni promenou
G = trees$Girth # uz zname G
attach(trees)
Girth
detach(trees)

##	Lze pracovat jen s nekterými sloupci, radky databaze

trees[1,]		      #	jen 1. radek
trees[-1,]		      #	bez 1. radku
trees[c(2,3),]	      #	jen konkretni radky
trees[,c("Girth","Volume")]
trees[!trees$Volume,]	#	vykricnik  znamena negaci
trees[trees$Height>60,] #	jen radky splnujici podminku 
trees$newvariable = c(1:length(trees$Volume))
# podobne pro sloupce

trees
mean(trees$Height); mean(trees$Volume)
trees$BMI = (trees$Volume * 900)/trees$Height^2
trees$Characteristic = c(factor(trees$BMI,c("hubeny","normalni","tlusty")))
