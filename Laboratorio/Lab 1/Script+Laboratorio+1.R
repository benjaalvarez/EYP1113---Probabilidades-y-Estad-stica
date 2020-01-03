###################################
#                                 #
#     EYP1113: Laboratorio 1      #
#                                 #
###################################


# Ejercicio 1:
11+8+2016
3-(2-(1-(15/5*3))-3**2)
3-(2-(1-(15/5*3))-3^2) 
# Para elevar a una potencia se puede usar ** o ^


# Ejercicio 2:
# a)
sqrt(1082016)
# b)
log(235)
# c)
exp(0)
# d)
factorial(85)
# e)
sin(pi)/cos(2*pi)


# Ejercicio 3:
a <- "Laboratorio EYP1113"
b <- 3*6+9/7
d <- sqrt(9)/log(10)
a
b
d
a+b
a*d
b/d
d-b


# Objetos #
ls()
objects()
# rm(a)
# rm(list=ls())
# save.image(file="objetos")
# save(a, file="objeto a")
# load(file="objetos")
# load(file="objeto a")

# Ejercicio 4:
# a)
ls()
# b)
rm(a)
a
# c)
save.image(file="objetos")
# d) 
rm(list=ls())
b
d
# e)
load(file="objetos")
b
d


# Ejemplos funciones 
z <- seq(from=-3, to=81, by=2)
z
length(z)
largo.z <- length(z)
largo.z
elemento2 <- z[2]
elemento2
1:8


# Ejercicio 5:
z[c(1,2,3)]
z[1:3]

# Ejercicio 6:
c(1,2,3)+c(6,8,9,7)


# Ejemplo expresiones lógicas
9<=6
c(2,5,8)>c(9,8,7)


# Ejercicio 7:
a <- matrix(c(1,2,3,4),ncol=2,nrow=2,byrow=FALSE)
a
b <- matrix(c(1,2,3,4),ncol=2,nrow=2,byrow=TRUE)
b
diag(a)
diag(b)
a*b
2*b
a%*%b
dim(a)
dim(b)
ncol(a)
ncol(b)
nrow(a)
nrow(b)
t(a)
t(b)
det(a)
det(b)
solve(a)
solve(b)


# Ejemplo funciones de matrices
cbind(a,b)
rbind(a,b)
is.matrix(a)
is.matrix(b)
c <- c(1,2)
c
is.matrix(c)
c <- as.matrix(c)
is.matrix(c)
c


# Ejemplo paquetes
getOption("defaultPackages")
library(help="datasets")
install.packages("dplyr")
library("dplyr")


# Ejemplo abrir bases de datos
data <- read.table(file.choose(),header=TRUE)
# data <- read.table("Tenis.txt",header=TRUE)


# Ejemplo obtener o cambiar directorio
getwd()
# setwd("C:/direccion de carpeta)


# Ejemplo clases de variables
class(data$Temperatura_Maxima)
data <- read.table(file.choose(),header=TRUE,dec=",")
class(data$Temperatura_Maxima)
class(data$Juega_Tenis)
data$Juega_Tenis=as.factor(data$Juega_Tenis)
class(data$Juega_Tenis)


# Ejercicio 8
# a)
data <- read.table(file.choose(),header=TRUE,dec=",")
attach(data)
names(data)
# b)
summary(Temperatura)
# El pronóstico de temperatura más frecuente es Moderado
# c)
min(Temperatura_Minima)
# El mínimo de las temperaturas mínimas es para el día 5
# d)
summary(Temperatura_Maxima)
summary(Temperatura_Minima)
# e)
median(Temperatura_Maxima)
