
############################
# Lectura de base de datos #
############################

install.packages("rio")
library(rio)
data = import(file.choose())
head(data)

install.packages("ggplot2")
library(ggplot2) 
# library(help="ggplot2")
?mpg

# Las packages/librerías traen bases de datos
# precargadas y listas para usar.
# ?mpg
## Base de datos mpg
# Datos de economía de combustible de 1999 y 2008
# para 38 modelos populares de automóviles
# > Este conjunto de datos contiene un subconjunto 
# de los datos de economía de combustible que la EPA 
# pone a disposición en http://fueleconomy.gov. 
# Contiene solo modelos que se han lanzado cada año 
# entre 1999 y 2008; este es un proxy de la popularidad del automóvil. 
# 234 filas y 11 variables
# http://127.0.0.1:14109/library/ggplot2/html/mpg.html

names(mpg)
head(mpg)
str(mpg)
summary(mpg)
View(mpg)

## manufacturer = marca / fabricante
cbind(table(mpg$manufacturer))
length(cbind(table(mpg$manufacturer)))
## model = modelo
cbind(table(mpg$model))
length(cbind(table(mpg$model)))
## displ = displacement / desplazamiento del motor, en litros 
# el volumen total de todos los cilindros en un motor. "cilindrada"
summary(mpg$displ)
sd(mpg$displ)
boxplot(x=mpg$displ)
hist(x=mpg$displ)

## year = año de fabricación
## cyl = número de cilindros
## trans = tipo de transmisión
table(mpg$trans)
cbind(table(mpg$trans))
length(cbind(table(mpg$trans)))

## drv = tracción: f = delantera, r = trasera, 4 = cuatro ruedas
table(mpg$drv )
cbind(table(mpg$drv ))
length(cbind(table(mpg$drv )))

## cty = millas por galón en la ciudad
summary(mpg$cty)
## hwy = millas por galón en la carretera (highway)
summary(mpg$hwy)

## fl = tipo de combustible
table(mpg$fl)
mpg[mpg$fl=="c",]


## class= tipo de vehículo
table(mpg$class)
cbind(table(mpg$class))

## Gráficos bonitos
barplot(x=table(mpg$manufacturer))
qplot(manufacturer, data=mpg, geom="bar", fill=manufacturer)

barplot(table(mpg$trans))
qplot(x=trans, data=mpg, geom="bar", fill=factor(trans))

## Gráficos interesantes

plot(mpg$cty~mpg$displ)
# los plots se pueden crear de la forma plot(x,y) o plot(y~x)

ggplot(data=mpg, aes(x=displ,y=hwy)) +
  geom_point(aes(color = (cyl)))

ggplot(data=mpg, aes(x=displ,y=hwy)) +
  geom_point(aes(size=displ,alpha=displ),
             color="blue") +
  theme_bw()

ggplot(data=mpg, aes(x=displ, y=cty)) +
geom_point(aes(size = hwy, colour = cyl, shape = drv)) +
guides(
colour = guide_colourbar(order = 1),
shape = guide_legend(order = 2),
size = guide_legend(order = 3)
)

colours <- list(~class, ~drv, ~fl)
for (colour in colours) {
print(ggplot(data=mpg, aes_(~ displ, ~ hwy, colour = colour)) +
geom_point())
}

ggplot(data=mpg, aes(x=displ, y=hwy)) +
  geom_point(data = dplyr::select(mpg, -class),
             color = "grey80", alpha = 0.5) +
  geom_point(aes(color = class), show.legend = FALSE) + 
  theme_bw() +
  facet_wrap(~class) 


## boxplot

boxplot(mpg$hwy~mpg$class)

par(mfrow = c(3,3))
for(i in as.vector(names(table(mpg$class)))){
plot(x=mpg$hwy[mpg$class==i])
}

ggplot(data=mpg, aes(x=class, y=hwy)) +
geom_boxplot(colour = "grey50") +
geom_jitter()

ggplot(data=mpg, aes(x = reorder(class, hwy), y = hwy)) + 
    geom_boxplot()+
    geom_jitter() 


ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = displ, y = hwy, linetype = drv, color=drv))

