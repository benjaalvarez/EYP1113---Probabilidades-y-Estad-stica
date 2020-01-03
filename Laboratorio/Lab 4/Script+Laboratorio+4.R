#########################
###                   ###
###   Laboratorio 4   ###
###                   ###
#########################

### Distribuciones ###

# Slide 5
# Exponencial #
dexp(x=1,rate=3)
pexp(q=1.5,rate=3)
rexp(n=1000,rate=3)
qexp(p=0.5,rate=3)

# Slide 7
# Uniforme #
dunif(x=0.5,min=0,max=1)
punif(q=0.5,min=0,max=1)
runif(n=10,min=0,max=1)
qunif(p=0.5,min=0,max=1)

dunif(x=5,min=-10,max=10)
punif(q=5,min=-10,max=10)
runif(n=10,min=-10,max=10)
qunif(p=0.75,min=-10,max=10)

dunif(x=60,min=50,max=100)
punif(q=60,min=50,max=100)
runif(n=10,min=50,max=100)
qunif(p=0.2,min=50,max=100)

dunif(x=0,min=-15,max=4)
punif(q=0,min=-15,max=4)
runif(n=10,min=-15,max=4)
qunif(p=0.7894737,min=-15,max=4) 
qunif(p=15/19,min=-15,max=4) 

# Slide 8
# Normal #
dnorm(x=80,mean=100,sd=20)
pnorm(q=80,mean=100,sd=20)
rnorm(n=10,mean=100,sd=20)
qnorm(p=0.1586553,mean=100,sd=20)

# Slide 9
# Log-Normal #
dlnorm(x=80,meanlog=1,sdlog=2)
plnorm(q=80,meanlog=1,sdlog=2)
rlnorm(n=10,meanlog=1,sdlog=2)
qlnorm(p=0.9545829,meanlog=1,sdlog=2)

# Slide 10
# Gamma #
dgamma(x=10,shape=3,scale=4)
pgamma(q=10,shape=3,scale=4)
rgamma(n=10,shape=3,scale=4)
qgamma(p=0.4561869,shape=3,scale=4)

# Slide 10
# Chi-Cuadrado #
dchisq(x=5,df=10)
pchisq(q=5,df=10)
rchisq(n=10,df=10)
qchisq(p=0.108822,df=10)

# Slide 13 
# curve #
curve(dnorm(x,mean=6,sd=6),from=-6,to=16)
curve(pnorm(x,mean=6,sd=6),from=-6,to=16)

# Slide 14
# Integrales #
integrate(dnorm,lower=-1.96,upper=Inf)
integrand <- function(x){1/((1+x)*sqrt(x))}
integrate(integrand,lower=0,upper=Inf)

# Slide 15
# Ejercicios #
# 1
tasa=2
muestra1=rexp(n=100,rate=tasa)
hist(x=muestra1,freq=FALSE,main="Ejercicio 1",col="pink")
curve(dexp(x,rate=tasa),add=TRUE,lwd=2,lty=2,col="blue")
# 2
muestra2=rlnorm(100,meanlog=0.5,sdlog=0.02)
hist(x=muestra2,freq=FALSE,main="Ejercicio 2",col="pink")
curve(dlnorm(x,meanlog=0.5,sdlog=0.02),add=TRUE,lwd=2,lty=2,col="blue")
# 3
mu=24
sigma=10.2
# a
# P(X>32)
1-pnorm(q=32,mean=mu,sd=sigma)
pnorm(q=32,mean=mu,sd=sigma,lower.tail=FALSE)
# b
# P(20<X<32/X>18)
# P((20<X<32) ^ (X>18))/P(X>18)
# P(20<X<32)/P(X>18)
(pnorm(q=32,mean=mu,sd=sigma)-pnorm(q=20,mean=mu,sd=sigma))/
  pnorm(q=18,mean=mu,sd=sigma,lower.tail=FALSE)
# c 
# exito = "Cliente mantiene cuenta por menos de 24 meses"
# P(exito)=P(X<24)
p=pnorm(q=24,mean=mu,sd=sigma)
# Y = cantidad de exitos de un total de n experimentos
# Y ~ Binomial(n=5,p)
n=5
# Nos preguntan P(Y<=1)
?pbinom
pbinom(q=1,size=n,prob=p)
dbinom(x=0,size=n,prob=p)+dbinom(x=1,size=n,prob=p)
 

### Programacion

# Slide 17
# input
a <- 1;b <- 4;c <- 2
# calculos
raiz1 <- (-b-sqrt(b^2-4*a*c))/(2*a)
raiz2 <- (-b+sqrt(b^2-4*a*c))/(2*a)
# output
c(raiz1,raiz2)

# Slide 19
discriminante <- b^2-4*a*c
if(discriminante>0){
  raiz <- c((-b-sqrt(b^2-4*a*c))/(2*a),
            (-b+sqrt(b^2-4*a*c))/(2*a))
} else{
  if(discriminante==0){
    raiz <- -b/(2*a)
  } else{
    raiz <- c()
  }
}
raiz
a<- 1; b<- 4; c<- 2
a<- 1; b<- 2; c<- 1
a<- 2; b<- 2; c<- 1

# Slide 21
x.seq <- seq(from=1,to=19,by=3)
producto <- 1
for(x in x.seq){
  producto <- producto*x
  print(producto)
}
# otra forma de hacer lo mismo
for(i in 1:length(x.seq)){
  producto <- prod(x.seq[1:i])
  print(producto)
}

# Slide 23
x.seq <- seq(1, 19, 3)
producto <- 1
i <- 1
while(producto < 4000){
  producto <- producto*x.seq[i]
  print(c(i, producto))
  i <- i+1
}

# Slide 24
suma2 <- 0
for(i in 1:100){
  suma2 <- suma2 + i^2
}
suma2
# Usando vectores
sum((1:100)^2)

# Slide 25
x <- c(-2, -1, 1, 2)
ifelse(x > 0, "Positivo", "Negativo")

# Slide 27
AED <- function(x) {
  if(is.numeric(x)==TRUE){
    prom <- mean(x); prom.rec <- mean(x, trim=0.1)
    med <- median(x); cuar <- quantile(x)
    mi <- min(x); ma <- max(x)
    va <- var(x); RIC <- IQR(x)
    par(mfrow=c(2, 1))
    hist(x, freq=F)
    boxplot(x,horizontal=TRUE)
    list(promedio=prom, promedio.recortado=prom.rec, mediana=med, cuartiles=cuar,
         minimo=mi, maximo=ma, varianza=va, RIC=RIC)
  }else{conteo <- table(x)
  par(mfrow=c(1, 2))
  barplot(conteo)
  pie(conteo)
  return(Tabla.conteo=conteo)
  }
}

# Slide 28
muestra3=rnorm(n=1000,mean=100,sd=20)
AED(muestra3)
muestra4=c("a","a","a","b","b")
AED(muestra4)

# Slide 30
x1 <- rnorm(n=10); x1
# por defecto mean=0 y sd=1
sapply(x1,round)
sapply(x1,round,digits=3)

# Slide 31 
X <- matrix(rnorm(n=30,mean=20,sd=3),ncol=3,
            nrow=10)
X
apply(X,2,mean)
apply(X,2,mean,trim=0.1)

# Slide 32
edad <- rnorm(n=30,mean=40,sd=sqrt(10));edad
genero <- rbinom(n=30,size=1,prob=0.3);genero
tapply(edad,genero,mean)
tapply(edad,genero,mean,trim=0.1)
