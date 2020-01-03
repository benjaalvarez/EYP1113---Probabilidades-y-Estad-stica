
#################
#		            #
# Laboratorio 8 #
#		            #
#################

####################
## QQplot Weibull ##
####################

set.seed(15102019)
n = 120
beta = 0.8
eta = 2
X = rweibull(n, shape = beta, scale = eta)


QQ.Weibull = function(y){
x=sort(y)
N=length(y)
p=(1:N)/(N+1)
plot(log(x)~log(-log(1-p)),
pch = 20, col = "darkblue", bty = "n", las = 1,
main = expression("QQ-Weibull"),
ylab = expression(log(x[p])),
xlab = expression(log(-log(1-p))))

abline(lm(log(x) ~ log(-log(1-p))), lwd = 3, col = "darkorange")
aux = lm(log(x) ~ log(-log(1-p)))
aux
}
eta = as.numeric(exp(QQ.Weibull(X)$coef[1]))
beta = as.numeric(1/QQ.Weibull(X)$coef[2])
eta
beta

hist(X,freq=F,nclass=40)
aux = seq(0,35,0.01)
lines(aux,dweibull(aux,beta,eta),lwd=2,col="red")
curve(dweibull(x,beta,eta),lwd=2,col="blue",lty=2,add=TRUE)


################################
## Teorema Central del Límite ##
################################

## Ejemplo 1                     ##
## X1,...,Xn iid Exponencial(nu) ##

set.seed(1113)
y.10 = vector("numeric")
y.50 = vector("numeric")
y.100 = vector("numeric")

rep = 10000
nu = 5

for(i in 1:rep){
x = rexp(n = 10, rate = nu)
y.10[i] = sum(x)
x = rexp(n = 50, rate = nu)
y.50[i] = sum(x)
x = rexp(n = 100, rate = nu)
y.100[i] = sum(x)
}

par(mfrow = c(2,2), cex = 0.7)

hist(y.10, freq = F, main = expression(n == 10), 
col = "darkblue", #
xlim = c(0,max(y.10)), 
nclass = 40)

x = seq(0,100, 0.01)
n = 10
lines(x, dgamma(x, rate = nu, shape = n), lwd = 2, col = "green")
lines(x, dnorm(x, mean = n/nu, sd = sqrt(n)/nu), lwd = 2, col = 2)

hist(y.50, freq = F, main = expression(n == 50), 
col = "darkblue", #
xlim = c(0,max(y.50)), nclass = 40)
x = seq(0,100, 0.01)
n = 50
lines(x, dgamma(x, rate = nu, shape = n), lwd = 2, col = "green")
lines(x, dnorm(x, mean = n/nu, sd = sqrt(n)/nu), lwd = 2, col = 2)


hist(y.100, freq = F, main = expression(n == 50), 
col = "darkblue", #
xlim = c(0,max(y.100)), nclass = 40)
x = seq(0,100, 0.01)
n = 100
lines(x, dgamma(x, rate = nu, shape = n), lwd = 2, col = "green")
lines(x, dnorm(x, mean = n/nu, sd = sqrt(n)/nu), lwd = 2, col = 2)


## Ejemplo 2               ##
## X1,...,Xn iid Poisson(nu) ##

y.10 = vector("numeric")
y.50 = vector("numeric")
y.100 = vector("numeric")

rep = 10000
nu = 1

for(i in 1:rep){
x = rpois(n = 10, lambda = nu)
y.10[i] = sum(x)
x = rpois(n = 50, lambda = nu)
y.50[i] = sum(x)
x = rpois(n = 100, lambda = nu)
y.100[i] = sum(x)
}

par(mfrow = c(2,2))

plot(table(y.10)/rep, 
lwd = 3,
 col = "darkblue",
 main = expression(n == 10))
n = 10
x = 0:max(y.10)
lines(x, dpois(x, lambda = n * nu), lwd = 3, col = 1, type= "p")
lines(x, dnorm(x, mean = n * nu, sd = sqrt(n) * sqrt(nu)), lwd = 3, col = 2)

plot(table(y.50)/rep, 
lwd = 3,
 col = "darkblue",
 main = expression(n == 50))
n = 50
x = 0:max(y.50)
lines(x, dpois(x, lambda = n * nu), lwd = 3, col = 1, type= "p")
lines(x, dnorm(x, mean = n * nu, sd = sqrt(n) * sqrt(nu)), lwd = 3, col = 2)

plot(table(y.100)/rep, 
lwd = 3,
 col = "darkblue",
 main = expression(n == 100))
n = 100
x = 0:max(y.100)
lines(x, dpois(x, lambda = n * nu), lwd = 3, col = 1, type= "p")
lines(x, dnorm(x, mean = n * nu, sd = sqrt(n) * sqrt(nu)), lwd = 3, col = 2)



###########################
## Estadísticos de Orden ##
###########################

## Ejemplo 1                     ##
## X1,...,Xn iid Exponencial(nu) ##

n = 100
nu = 0.1
y = vector("numeric")

rep = 10000
for(i in 1:rep){
x = rexp(n = 100, rate = nu)
y[i] = min(x)
}

par(mfrow = c(2,2))

hist(y, freq = F,nclass=40)
z = seq(0, 1, 0.01)
lines(z, dexp(z, rate = n*nu), col = 2, lwd = 3, lty = 1)

