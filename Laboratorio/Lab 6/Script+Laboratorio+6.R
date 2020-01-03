#########################
###                   ###
###   Laboratorio 6   ###
###                   ###
#########################

### Múltiples Variables Aleatorias ###


# Slide 7
# Primero construiremos la tabla de datos
Sexo=c(rep(0,1+0+4+2+1+1),rep(1,1+3+1+1+3+2))
Edad=c(c(9,rep(11,4),rep(12,2),13,14),
          9,rep(10,3),11,12,rep(13,3),rep(14,2))
data=data.frame(Sexo = Sexo, Edad = Edad)
tabla=table(data)
tabla
# 1
?prop.table
# Esta función divide por la suma de los x
Funcion.Prob.Conjunta <-prop.table(tabla)
Funcion.Prob.Conjunta
# 2
# Sumamos las probabilidades
sum(Funcion.Prob.Conjunta[2,c("10","12","14")])
# Otra forma de calcularlo
P.B=mean(data$Edad%%2==0 & data$Sexo ==1)
P.B
# 3
Funcion.Sexo=apply(Funcion.Prob.Conjunta,1,sum)
Funcion.Sexo
# 4
Funcion.Edad=apply(Funcion.Prob.Conjunta,2,sum)
Funcion.Edad
# 5
Funcion.Y.dado.X=prop.table(tabla,1)
Funcion.Y.dado.X
# 6
# Podemos probar si P(X=0,Y=10)=P(X=0)P(Y=10)
Funcion.Prob.Conjunta["0","10"]==Funcion.Sexo["0"]*Funcion.Edad["10"]
# No son independientes
# 7)
Costo=function(x,y){
  costo=ifelse(x==1,3000+150*y,2500+120*y)
    return(costo)
}
Costo(data$Sexo,data$Edad)
mean(Costo(data$Sexo,data$Edad))
# A mano, sería así:
Costo(0,9)*Funcion.Prob.Conjunta["0","9"]+
Costo(1,9)*Funcion.Prob.Conjunta["1","9"]+
Costo(0,10)*Funcion.Prob.Conjunta["0","10"]+
Costo(1,10)*Funcion.Prob.Conjunta["1","10"]+
Costo(0,11)*Funcion.Prob.Conjunta["0","11"]+
Costo(1,11)*Funcion.Prob.Conjunta["1","11"]+
Costo(0,12)*Funcion.Prob.Conjunta["0","12"]+
Costo(1,12)*Funcion.Prob.Conjunta["1","12"]+
Costo(0,13)*Funcion.Prob.Conjunta["0","13"]+
Costo(1,13)*Funcion.Prob.Conjunta["1","13"]+
Costo(0,14)*Funcion.Prob.Conjunta["0","14"]+
Costo(1,14)*Funcion.Prob.Conjunta["1","14"]

# Slide 8 #
p=0.6
lambda=15
p.x.y=function(x,y){
  p.conjunta=ifelse(x<=y,dbinom(x,y,p)*dpois(y,lambda),0)
  return(p.conjunta)
}
x=seq(0,30)
y=seq(0,30)
z=outer(x,y,p.x.y) 
X=rep(x,times=length(y))
Y=rep(y,each=length(x))
Z=c(z)
library(scatterplot3d)
scatterplot3d(X,Y,Z,type="h",lwd=2,pch="",
              xlab="X",ylab="Y",
              zlab=expression(P(X==x,Y==y)),
              highlight.3d=TRUE,
              angle=45)

# Slide 9 #
alpha=3
beta=3
f.x.y=function(x,y){
  densidad=alpha*beta*exp(-alpha*x-beta*y)
  return(densidad)
}
x=seq(0.001,3,0.01)
y=seq(0.001,3,0.01)
z=outer(x,y,f.x.y)
library(rgl)
rgl.surface(x=x,y=z,z=y,color="red",back="lines")


# Análisis de regresión #
# Slide 20
colnames(iris)
pairs(iris[,1:4])
panel.cor=function(x,y){
  par(usr=c(0,1,0,1))
  txt=as.character(format(cor(x,y),digits=2))
  text(0.5,0.5,txt,cex=6*abs(cor(x,y)))
}
pairs(iris[,1:4],upper.panel=panel.cor)

# Slide 23
f.xy=function(x,y,mu.x=0,mu.y=0,s.x=1,s.y=1,rho=0){
  n.r=length(x)
  n.c=length(y)
  M=matrix(NA,ncol=n.c,nrow=n.r)
  for(i in 1:n.r){
    M[i,]=dnorm(x[i],mean=mu.x,sd=s.x)*
      dnorm(y,mean=mu.y+rho*s.y*(x[i]-mu.x)/s.x,
            sd=s.y*sqrt(1-rho^2))
  }
  M
}
x=seq(-5,5,0.1)
y=seq(-5,5,0.1)
z=f.xy(x,y,rho=0)
rgl.surface(x=x,y=z*10,z=y,color="red",back="lines")

# Slide 25
data=read.table(file.choose(),h=T)
View(data)
X=data$grasas
Y=data$peso
Z=data$edad
# 1
pairs(data,upper.panel=panel.cor)
# 2
b=sum((Z-mean(Z))*(X-mean(X)))/sum((Z-mean(Z))^2)
a=mean(X)-b*mean(Z)
cbind(a,b)
# 3
par(mfrow=c(1,1))
plot(X~Z,pch=20,bty="n",xlim=c(15,70),
     ylim=c(100,500),lwd=5,las = 1,
     xlab="Edad",ylab="Grasas")
abline(a=a,b=b,lwd=3,col="red")
# 4
n=length(Z)
S.x.z=sqrt((sum((X-mean(X))^2)-
              b^2*sum((Z-mean(Z))^2))/(n-2))
S.x=sqrt(var(X))
cbind(S.x.z,S.x)
r2=1-S.x.z^2/S.x^2
r2
# 5
summary(lm(X ~ Z))

