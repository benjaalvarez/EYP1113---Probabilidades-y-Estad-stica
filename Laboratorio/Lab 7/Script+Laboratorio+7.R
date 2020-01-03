#########################
###                   ###
###   Laboratorio 7   ###
###                   ###
#########################

### Gráficos de probabilidad ###

# 1 #
y=rnorm(200, mean=10, sd=1.5)
y=sort(y)
N=length(y)
m=1:N
x=m/(N+1)
p=c(0.01, 0.1, 1,2,5,10,20,30,40,50,60,70,80,90,95,99,99.9,99.99)/100
plot(qnorm(x), y, xaxt="n",
     ylab="Valores de Y",
     xlab = "Probabilidad Acumulada",
     bty="n", lwd=2, las=1, xlim=c(-3.5,3.5))
axis(1,at=qnorm(p),label=p,las=1)
abline(lm(y~qnorm(x)), lwd=2 ,col=2)
fit=lm(y~qnorm(x))$coef
hat.mu=fit[1]
hat.sigma=fit[2]
hat.mu;hat.sigma
hist(y, freq=F, col="darkred", 
     border="white", main="", las=1)
aux = seq(0,20, 0.01)
lines(aux, dnorm(aux, mean = hat.mu, sd = hat.sigma), lwd = 2)

# 2 #
lambda=1
zeta=0.2
y=rlnorm(200, meanlog=lambda, sdlog=zeta)
y=sort(y)
N=length(y)
m=1:N
x=m/(N+1)
p=c(0.01, 0.1, 1,2,5,10,20,30,40,50,60,70,80,90,95,99,99.9,99.99)/100
plot(qnorm(x), log(y), xaxt = "n", 
     ylab="Valores de Y", 
     xlab="Probabilidad Acumulada", 
     bty="n", lwd=2, las=1, xlim=c(-3.5,3.5))
axis(1, at=qnorm(p), label=p, las=1)
abline(lm(log(y)~qnorm(x)), lwd=2, col=2)
fit=lm(log(y)~qnorm(x))$coef
hat.lambda = fit[1]
hat.zeta = fit[2]
hist(y, freq=F, col="darkred", border="white", main="", las=1)
aux=seq(0,20, 0.01)
lines(aux, dlnorm(aux, meanlog=hat.lambda, sdlog=hat.zeta), lwd=2)
# 3 #
k=10
nu=0.1
y=rgamma(200, rate=nu, shape=k)
hist(y, freq=F, col="darkred", border="white", main="", las = 1)

# qqplot Normal
par(mfrow=c(1,2), cex = 0.8)
y=sort(y)
N=length(y)
m=1:N
x=m/(N+1)
p=c(0.01, 0.1, 1,2,5,10,20,30,40,50,60,70,80,90,95,99,99.9,99.99)/100
plot(qnorm(x), y, xaxt="n", 
     ylab = "Valores de Y", 
     xlab = "Probabilidad Acumulada", 
     bty="n", lwd=2, las=1, xlim=c(-3.5,3.5),
     main="Distribución Normal")
axis(1, at = qnorm(p), label=p, las=1)
abline(lm(y~qnorm(x)), lwd = 2, col = 2)
fit = lm(y~qnorm(x))$coef
hat.mu = fit[1]
hat.sigma = fit[2]

# qqplot Log-Normal
plot(qnorm(x), log(y), xaxt="n", 
     ylab="Values of Y", 
     xlab="Cumulative Probability", 
     bty="n", lwd=2, las=1, xlim=c(-3.5,3.5),
     main="Distribución Log-Normal")
axis(1, at = qnorm(p), label=p, las=1)
abline(lm(log(y)~qnorm(x)), lwd = 2, col = 2)
fit = lm(log(y)~qnorm(x))$coef
hat.lambda = fit[1]
hat.zeta = fit[2]

# Histograma
par(mfrow=c(1,1), cex=0.8)
hist(y, freq=F, col="darkred", border="white", main="", 
     las=1, xlim=c(0, 250), ylim=c(0, 0.02), xlab="")
aux=seq(0,300, 0.01)
lines(aux, dnorm(aux, mean=hat.mu, sd=hat.sigma), lwd=2, col="darkblue")
lines(aux, dlnorm(aux, meanlog=hat.lambda, sdlog=hat.zeta), 
      lwd=2, col="darkgreen")
legend("topright", col = c("darkblue", "darkgreen"), 
       lwd=2, c("Normal","Log-Normal"), cex=2, bty="n")

