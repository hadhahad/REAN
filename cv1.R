sigma = 1
mu = 1
x = 5
f_normal = 1/(sqrt(2*pi*sigma^(2)))*exp(-(x-mu)^(2)/(2*sigma^(2)))

#######################################################################

n = 5
p = 0.4
p_bi = c()

for (k in 0:5){
  p_bi = c(p_bi, (choose(n, 5)*p^k*(1-p)^(n-k)))
}
