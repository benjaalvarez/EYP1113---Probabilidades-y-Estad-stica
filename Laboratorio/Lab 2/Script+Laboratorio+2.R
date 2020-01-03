##################
# Laboratorio 02 # By Natu
##################

# Cargar base de datos en otro formato: .dta
# Se debe cargar librería

library(foreign)
data = read.dta("RentasMunich.dta")

head(data)
dim(data)
summary(data)
cbind(names(data))

 par(mar=c(5.1,5.1,4.1,2.1))
 curve(100*(x^3-x^2)+15, 0, 1,
 	xlab=expression(alpha),
 	ylab=expression(100%*%(alpha^3-alpha^2)+15),
 	main=expression(
 		paste("Funcion: ",
 			f(alpha)==100%*%(alpha^3-alpha^2)+15)))
 sigma1=1.2
 text(0.1,3,bquote(sigma[alpha]==.(sigma1)))
 text(0.8,11,expression(sigma[alpha]==0.25))

plot(data$rent ~ data$area, 
xlab=expression("Tamaño del departamento "*"[m"^2*"]"), 
ylab="Precio de renta [€]",
main="Precio de renta por tamaño, Munich 1999")

# BOXPLOT RENTA
boxplot(data$rentsqm)
boxplot(data$rentsqm,main="Boxplot de Renta por metro cuadrado")

hist(data$rentsqm)
hist(data$rentsqm,main="Histograma de Renta por metro cuadrado",
	xlab="Renta por metro cuadrado",
	ylab="Frecuencia")

boxplot(data$rent~data$location,
xlab="Localización",
ylab="Renta")
# COMENTARIO: Ah no pero que feo con 1 2 3 ¡Vamos a cambiar esos nombres!

data$Localización <- ifelse(data$location == 1, "Promedio",
	ifelse(data$location == 2,"Bueno",
	"Alta"))

# COMENTARIO: ÔJÔ! a la data original le añadimos una nueva variable.

boxplot(data$rent~data$Localización,
xlab="Localización",
ylab="Renta")
title("Valor de la renta según la localización")

data$Baño <- ifelse(data$bath == 0, "Estandar","Premium")
barplot(table(data$Baño,data$Localización))
barplot(table(data$Baño,data$Localización),
        beside=TRUE,ylim=c(0,2000),
main="Distribución de Baños",
        legend=FALSE)

data$Cocina <- ifelse(data$kitchen == 0, "Estandar","Premium")
barplot(table(data$Cocina,data$Localización))
barplot(table(data$Cocina,data$Localización),
beside=TRUE,ylim=c(0,2000),
main="Distribución de Baños",
legend=FALSE)

data$Calefaccion <- ifelse(data$cheating == 0, "No tiene","Tiene")
boxplot(data$rentsqm~data$Calefaccion )
boxplot(data$rentsqm~data$Calefaccion , 
	xlab = "Calefacción", 
	ylab="Renta por metro cuadrado",
	main="Renta por metro cuadrado vs. presencia de Calefacción")
