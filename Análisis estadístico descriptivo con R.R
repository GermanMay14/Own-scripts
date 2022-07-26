
###############################
# AID - Germ�n May
###############################


#limpio la memoria
rm( list=ls() )
gc()

#install.packages(tidyverse)
#install.packages(gtools)


# setwd("C:/Users/User/Documents/Mis documentos/German/MCD Austral/AID/TPs/TP2")
# setwd("C:/Users/German/Documents/Personal/MCD Austral/AID/TPs/TP2")

getwd() #chequeo en que carpeta estoy

library(tidyverse)
library(gtools)
library(summarytools) # para el uso de estad�sticas descriptivas
library(dplyr)
library(ggplot2)
library(plotly)
library(readxl)
library (funModeling)
library(packHV)
library(corrplot)
library(MASS)
library(DMwR2)
library(reshape2)



options(scipen = 999) ### turn off scientific notation


######################################################################################


#### PUNTO 1 - Carga del archivo


# Cargo el archivo dataset.xls


dataset_banco<- read_xls("dataset.xls")



#  COnvierto en Factores toda las columnas que son de formato "character"


dataset_banco[sapply(dataset_banco, is.character)] <-
  lapply(dataset_banco[sapply(dataset_banco, is.character)],as.factor)

# Fin punto 1
####################################################################################################

#### PUNTO 2 - Tipos de Variable


# Realizo un resumen estad�tico para verficar cada tipo de variable

summary(dataset_banco)

# ID: si bien el summary lo identifica como variable cuantitativa, sabemos que los n�meros de
# cliente son solamente formas de identificarlos, por lo tanto es CUALITATIVA - NOMINAL

# COLOR: variable CUALITATIVA. Supongo que podr�a considerarse como Ordinal en funci�n
# de una escala de color prefijada.

# TIENE_PIN: variable CUALITATIVA, BINARIA. Sin embargo, hay 65 clientes que figuran con el
# n�mero "2" en lugar de "si" o "no". Supongo que esto es un error dada la descripci�n de 
# la variable en el enunciado del TP.

# VT: variable CUALITATIVA, BINARIA

# CLIENTE_PAS: variable CUALITATIVA, BINARIA

# EDAD: variable CUANTITATIVA

# ANTIGUEDAD: variable CUANTITATIVA

# CONSUMO_TC: variable CUANTITATIVA

# MOV90_CTA: variable CUANTITATIVA

# SUELDO: variable CUANTITATIVA

# CHURN: variable CUALITATIVA, BINARIA

# Fin punto 2
####################################################################################################

# ACLARACION IMPORTANTE SOBRE EL MANEJO DE LOS OUTLIERS PARA LOS SIGUIENTES ANALISIS

# A partir de este punto y teniendo en cuenta la gran cantidad de outliers de casi todas las variables
# cuantitativas, voy a realizar todos los an�lisis primero con los datos completos y luego, con un nuevo
# set de datos sin outliers (tomando hasta el percentil 95 de cada variable, excepto para EDAD). No voy 
# a eliminar los registros ya que perder�a la informaci�n que poseen para otras variables, sino que voy 
# a convertir en "NA" esos valores extremos. Ser�a conveniente poder hacer un an�lisis espec�fico del 
# conjunto de registros con outliers ya que podr�an dar informaci�n relevante para el negocio.


####################################################################################################

# Nuevo dataset sin outliers. Considero outliers a Edad mayor a 105 y para el resto, cuando la variable
# cuantitativa est� m�s all� del percentil 95. En el caso de la variable SUELDO, quedar�an identificados
# como NA los outliers, de la misma forma que los valores que originalmente eran NAs, pero igualmente, 
# al preservar la base original, se puede hacer la distinci�n de los registros de ser necesario.


dataset_banco_sinOut <-dataset_banco%>%mutate(EDAD = ifelse((EDAD > 105), NA, EDAD))%>%
  mutate(ANTIGUEDAD= ifelse((ANTIGUEDAD > quantile(dataset_banco$ANTIGUEDAD,0.95)), NA, ANTIGUEDAD))%>%
  mutate(CONSUMO_TC= ifelse((CONSUMO_TC > quantile(dataset_banco$CONSUMO_TC,0.95)), NA, CONSUMO_TC))%>%
  mutate(MOV90_CTA= ifelse((MOV90_CTA > quantile(dataset_banco$MOV90_CTA,0.95)), NA, MOV90_CTA))%>%
  mutate(SUELDO= ifelse((SUELDO > quantile(dataset_banco$SUELDO,0.95,na.rm = TRUE)), NA, SUELDO))


# Finalmente, para facilitar el trabajo con algunas funciones, separo los datasets de variables CUANTI
# tanto para los datos originales como para el set de datos sin outliers y tambi�n variables CUALI:


dataset_banco_cuanti <- dataset_banco[c(6,7,8,9,10)]

dataset_banco_SinOut_cuanti <- dataset_banco_sinOut[c(6,7,8,9,10)]

dataset_banco_cuali <- dataset_banco[c(2,3,4,5,11)]



####################################################################################################

#### PUNTO 3 - Estad�sticos Descriptivos para las variables

# Descarto la variable ID por lo ya explicado en el punto 2. 

# Para cada variable CUALITATIVA, obtengo la frecuencia relativa de cada una de los niveles. En estos
# casos trabajo solamente con la base de datos original-

# COLOR
view(freq(dataset_banco$COLOR, style="rmarkdown"))
# Todos los clientes est�n categorizados. La mayor�a (40.5%) son color verde, seguidos por
# los de color azul (35%). Luego, los de color rojo representan el 16.2% y finalmente los violeta (8.2%).


# TIENE_PIN
view(freq(dataset_banco$TIENE_PIN, plain.ascii = FALSE, style = "rmarkdown"))
# EL 74.2% de los clientes tiene acceso al Home Banking. Hay 65 clientes (0.36%) clasificados como
# "2".. Probablemente se trate de un error pero habr�a que averiguar. 


# VT
view(freq(dataset_banco$VT, plain.ascii = FALSE, style = "rmarkdown"))
# Al 62.5% de los clientes se los considera vinculados transaccionalmente con el banco.


#CLIENTE_PAS
view(freq(dataset_banco$CLIENTE_PAS, plain.ascii = FALSE, style = "rmarkdown"))
# El 26.8% de los clientes cobra su sueldo en este banco.


CHURN
view(freq(dataset_banco$CHURN, plain.ascii = FALSE, style = "rmarkdown"))
# s�lo el 1.06% de los clientes se dio de baja dentro de los 2 meses siguientes del an�lisis.



# Para cada variable CUANTITATIVA, obtengo el resumen estad�stico mediante "summary", tanto para 
# los datos originales como para el data set sin outliers.


# EDAD

# Edad - Datos originales
summary(dataset_banco$EDAD)
# Hay un error de datos obviamente en la edad m�xima (900). En cuanto a la edad m�nima de 12 a�os 
# habr�a que consultar si es posible. La edad promedio de 43.5 a�os  y la mediana es 41 a�os.


# Edad - Sin outliers

summary(dataset_banco_sinOut$EDAD)
# Encontramos 18 outliers, es decir con edades registradas mayores a 105 a�os. La mediana qued�
# igual (m�s robusta frente a outliers) pero la media tampoco cambi� pr�cticamente, probablemente
# por la poca cantidad de registros err�neos en relaci�n a la cantidad total.


# ANTIGUEDAD

# Antiguedad - Datos originales
summary(dataset_banco$ANTIGUEDAD)
# El 25% de los clientes tiene a lo sumo 10 meses de antiguedad. La mediana es 39 meses y la media 
# casi 70 por lo que presuponemos una distribuci�n con una cola m�s larga hacia la derecha. (a confirmar
# con el histograma)

# Antiguedad - Sin outliers
summary(dataset_banco_sinOut$ANTIGUEDAD)
# Hay 906 outliers (valores por arriba del percentil 95).La mediana baj� a 34 meses.


# CONSUMO_TC

# CONSUMO_TC - Datos originales

summary(dataset_banco$CONSUMO_TC)
# Se observa que el 50% de los clientes no realiza consumos con tarjeta de cr�dito y que el tercer
# cuartil consume 4472 o menos por mes. Sin embargo, aparece un valor m�ximo de m�s de 9500 millones,
# lo que probablemente sea un error y que, adem�s, eleva mucho a la media (aprox 21 millones ). 
# Ver� esto en profundidad en el an�lisis de outliers.


# CONSUMO_TC - Sin outliers
summary(dataset_banco_sinOut$CONSUMO_TC)
# Hay 909 outliers (valores por arriba del percentil 95).Es tal la incidencia de los outliers en esta
# variable, que la media baj� de aprox. 21 millones a 2663 !!. El tercer cuartil, que es el primero de 
# los cuartiles que no es igual a cero, muestra  su robustez, ya que cambi� pero no tanto como la media:
# pas� de 4472 a 3424...


# MOV90_CTA
 
# MOV90_CTA - Datos originales
summary(dataset_banco$MOV90_CTA)
# EL 25% de los clientes realizaron 3 movimientos � menos en los �ltimos 90 d�as. El 50% realizaron
# 20 movimientos o menos. La media fue de 43.7 movimientos y el m�ximo tambi�n est� para analizar
# m�s adelante entre los outliers.

# MOV90_CTA - Sin outliers
summary(dataset_banco_sinOut$MOV90_CTA)
# Hay 903 outliers (valores por arriba del percentil 95). La mediana pas� de 20 a 18 y la media de
# 43.7 a 34.8 con este set de datos sin outliers.


# SUELDO

# SUELDO - Datos originales
summary(dataset_banco$SUELDO)
# Hay 13298 valores NAs correspondientes a los clientes que no cobran en el banco.
# El valor m�nimo de 10 parecer�a un error (a chequear). Como suele suceder con los sueldos, se observa
# que la mediana de aprox 25500 es menor que la media de aprox 31800, debido a que poca gene cobra sueldos
# m�s altos, que elevan la media, pero no infuyen significativamente en la mediana. 
# El 75% de la gente que cobra sueldo en el banco, recibe 37471 o menos.


# SUELDO - Sin outliers
summary(dataset_banco_sinOut$SUELDO)
# Hay 244 outliers (valores por arriba del percentil 95). La media baj� de 31878 a 25426 y la mediana
# de 22553 a 21629.

# Fin punto 3
####################################################################################################


#### PUNTO 4 - Gr�ficos univariados y bivariados

### Univariados

## Variables Cualitativas

# Gr�fico de barras horizontales 

# Con la siguiente funci�n pueden observarse los gr�ficos de barras horizontales para todas las 
# variables cualitativas. Las conclusiones son las mismas que se realizaron en el punto 3 cuando
# se utiliz� la funci�n "Freq".

view(dfSummary(dataset_banco_cuali))



## Variables Cuantitativas

# Histogramas de las variables cuantitativas - Datos originales


plot_num(dataset_banco_cuanti)

# Vemos que solamente ANTIGUEDAD parece tener un histograma no tan influenciado por los valores extremos 
# grandes que dejan casi todos los valores en muy pocos bins sobre la izquierda de cada gr�fico. Es decir,
# que EDAD, CONSUMO_TC, MOV90_CTA y SUELDO, tienen outliers a la derecha impiden que la distribuci�n pueda
# visualizarse en detalle.


# Histogramas de las variables cuantitativas - Sin outliers


plot_num(dataset_banco_SinOut_cuanti)


# Todas las distribuciones tienen una marcada asimetr�a positiva (cola larga hacia la derecha). La menos
# asim�trica es la variable SUELDO.
# Edad: se observa el modo alrededor de los 30 a�os y luego una disminuci�n paulatina de la frecuencia
# a medida que aumenta.
# Antiguedad: valores muy concentrados entre los 30 meses o menos.
# Consumo: se observa la gran moda en "cero" consumo y luego valores paulatinamente decrecientes  de 
# frecuencia.
# Mov90_Cta: se observa tambi�n una moda importante cercana a valores cero y luego tambi�n un decrecimiento
# paulatino de la frecuencia hacia la izquierda.
# Sueldo: la distribuci�n es un poco m�s sim�trica que las otras con una moda cercana a los 20.000.


# Box plots

boxplot(dataset_banco_cuanti)
# Al graficar todas las variables juntas y con sus outliders, se hace muy dif�cil obtener un gr�fico
# entendible debido a la escala del mismo que debe contemplar los valores m�s altos. 


# Pruebo ahora con todas las variables Cualitativas juntas pero sin outliers

boxplot(dataset_banco_SinOut_cuanti)

# El gr�fico mejor� pero sigue siendo necesaria una escala particular para cada variable. Realizo entonces
# los boxplots para cada una:



# boxplot EDAD - Datos originales

pp <- dataset_banco_cuanti%>%
  ggplot(aes(y=EDAD))+
  geom_boxplot()+
  labs(y="Edad - datos originales",color="Tipo")
theme_bw()
ggplotly(pp)

# boxplot EDAD - Sin outliers

pp <- dataset_banco_SinOut_cuanti%>%
  ggplot(aes(y=EDAD))+
  geom_boxplot()+
  labs(y="Edad - sin outliers",color="Tipo")
theme_bw()
ggplotly(pp)


# gr�fica bastante sim�trica dentro de la caja. 50% de los clientes entre 31 y 54 a�os 


# boxplot ANTIGUEDAD - Datos originales

pp <- dataset_banco_cuanti%>%
  ggplot(aes(y=ANTIGUEDAD))+
  geom_boxplot()+
  labs(y="ANTIGUEDAD - datos originales",color="Tipo")
theme_bw()
ggplotly(pp)

# boxplot ANTIGUEDAD - Sin outliers

pp <- dataset_banco_SinOut_cuanti%>%
  ggplot(aes(y=ANTIGUEDAD))+
  geom_boxplot()+
  labs(y="ANTIGUEDAD - sin outliers",color="Tipo")
theme_bw()
ggplotly(pp)

# En este caso no hay mucho m�s para agregar que lo ya mencionado para esta variable.


# boxplot CONSUMO_TC - Datos originales

pp <- dataset_banco_cuanti%>%
  ggplot(aes(y=CONSUMO_TC))+
  geom_boxplot()+
  labs(y="CONSUMO_TC - datos originales",color="Tipo")
theme_bw()
ggplotly(pp)

# boxplot CONSUMO_TC - Sin outliers

pp <- dataset_banco_SinOut_cuanti%>%
  ggplot(aes(y=CONSUMO_TC))+
  geom_boxplot()+
  labs(y="CONSUMO_TC - sin outliers",color="Tipo")
theme_bw()
ggplotly(pp)

# El boxpolot de datos originales es totalmente ilegible por la gran cantidad de outliers grandes.
# Inclusive en el boxplot "sin outliers" sigue habiendo muchos outliers m�s all� del percentil del
# 95%. Mirando tambi�n el histograma se ve que se trata de una distribuci�n particularmente "achatada"


# boxplot MOV90_CTA - Datos originales

pp <- dataset_banco_cuanti%>%
  ggplot(aes(y=MOV90_CTA))+
  geom_boxplot()+
  labs(y="MOV90_CTA - datos originales",color="Tipo")
theme_bw()
ggplotly(pp)

# boxplot MOV90_CTA - Sin outliers

pp <- dataset_banco_SinOut_cuanti%>%
  ggplot(aes(y=MOV90_CTA))+
  geom_boxplot()+
  labs(y="MOV90_CTA - sin outliers",color="Tipo")
theme_bw()
ggplotly(pp)

# Se observa, al igual que en la variable anterior, la caja grande casi apoyada en cero debido a la
# gran cantidad de valores cero o cercanos a cero (cosa que tambi�n se ve en el histograma)


# boxplot SUELDO - Datos originales

pp <- dataset_banco_cuanti%>%
  ggplot(aes(y=SUELDO))+
  geom_boxplot()+
  labs(y="SUELDO - datos originales",color="Tipo")
theme_bw()
ggplotly(pp)

# boxplot SUELDO - Sin outliers

pp <- dataset_banco_SinOut_cuanti%>%
  ggplot(aes(y=SUELDO))+
  geom_boxplot()+
  labs(y="SUELDO - sin outliers",color="Tipo")
theme_bw()
ggplotly(pp)

# No hay mucho para agregar m�as all� de lo ya mencionado para esta variable.



### Gr�ficos Bivariados

## Cuantitativas vs Cuantiativas

# Utilizo la funci�n Pairs para obtener el diagrama de dispersi�n de todas las variables cuantitativas
# entre s� de a dos. 

# Para los datos originales

pairs(dataset_banco_cuanti)


# Observando el gr�fico, considero que conviene ver en detalle la  relaci�n entre SUELDO,
# CONSUMO_tc y MOV90_CTA



# Diagrama de dispersi�n SUELDO -CONSUMO_TC - datos originales

p <- dataset_banco_cuanti%>%
  ggplot( aes(SUELDO, CONSUMO_TC)) +
  geom_point() +
  theme_bw()
ggplotly(p)

# NO se observa una relaci�n clara entre sueldo y consumo tc, pero Se observan algunos
# puntos outliers que se dan en las dos variables.


# Diagrama de dispersi�n SUELDO - MOV90_CTA - datos originales

p <- dataset_banco_cuanti%>%
  ggplot( aes(SUELDO, MOV90_CTA)) +
  geom_point() +
  theme_bw()
ggplotly(p)

# Tampoco se puede apreciar una relaci�n clara entre sueldo y mov90 cta, pero Se observan 
# tambi�n algunos puntos outliers que se dan en las dos variables.


# Diagrama de dispersi�n CONSUMO_TC - MOV90_CTA - datos originales

p <- dataset_banco_cuanti%>%
  ggplot( aes(CONSUMO_TC, MOV90_CTA)) +
  geom_point() +
  theme_bw()
ggplotly(p)

# No observa una relaci�n clara entre las variables.


## Cuantitativas vs Cualitativas


# Obtengo los promedios de distintas variables Cuanti en funci�n de un agrupamiento de
# variables Cuali


# Variables Cuali vs COLOR

Segmentacion_COLOR <- dataset_banco%>%group_by(COLOR)%>%
  summarise(Edad_promedio = round(mean(EDAD,na.rm=TRUE),2),
            Antiguedad_promedio = round(mean(ANTIGUEDAD,na.rm=TRUE),2),
            Consumo_TC_promedio = round(mean(CONSUMO_TC,na.rm=TRUE),2),
            Mov90_cta_promedio = round(mean(MOV90_CTA,na.rm=TRUE),2),
            Sueldo_prom = round(mean(SUELDO,na.rm=TRUE),2))

# Edad vs Color

ggplot(Segmentacion_COLOR, aes(x=COLOR, y=(Edad_promedio))) +
  geom_bar(stat="identity", position="dodge")  +
  theme_bw() +
  labs(title="",
       x="COLOR",
       y="Edad promedio") +
  theme(text = element_text(size=12), # Tama�o de fuente del gr�fico por defecto
        plot.title = element_text(size=rel(1), # Tama�o del t�tulo, doble del establecido por defecto
                                  vjust=2, #Justificaci�n vertical, para separarlo del gr�fico
                                  face="bold", #Letra negrilla
                                  color="darkgreen", #Color del texto
                                  lineheight=1.5), #Separaci�n entre l�neas)
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position="none",
        axis.title.x = element_text(face="bold", vjust=1.5, colour="blue", size=rel(1)),
        axis.title.y = element_text(face="bold", vjust=1.5, colour="blue", size=rel(1)),
        axis.text = element_text(colour = "black")) +
  coord_cartesian(ylim=c(0, 50)) +
  scale_y_continuous(breaks =seq(0,50, 10))

# NO se observan mucha didferencia, salvo para el color violeta que tiene la mayor edad promedio.


# Antiguedad vs COLOR

ggplot(Segmentacion_COLOR, aes(x=COLOR, y=(Antiguedad_promedio))) +
  geom_bar(stat="identity", position="dodge")  +
  theme_bw() +
  labs(title="",
       x="COLOR",
       y="Antiguedad promedio") +
  theme(text = element_text(size=12), # Tama�o de fuente del gr�fico por defecto
        plot.title = element_text(size=rel(1), # Tama�o del t�tulo, doble del establecido por defecto
                                  vjust=2, #Justificaci�n vertical, para separarlo del gr�fico
                                  face="bold", #Letra negrilla
                                  color="darkgreen", #Color del texto
                                  lineheight=1.5), #Separaci�n entre l�neas)
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position="none",
        axis.title.x = element_text(face="bold", vjust=1.5, colour="blue", size=rel(1)),
        axis.title.y = element_text(face="bold", vjust=1.5, colour="blue", size=rel(1)),
        axis.text = element_text(colour = "black")) +
  coord_cartesian(ylim=c(0, 150)) +
  scale_y_continuous(breaks =seq(0,50, 10))

# El color violeta es el que mayor antiguedad promedio tiene


# CONSUMO_TC vs COLOR

ggplot(Segmentacion_COLOR, aes(x=COLOR, y=(Consumo_TC_promedio))) +
  geom_bar(stat="identity", position="dodge")  +
  theme_bw() +
  labs(title="",
       x="COLOR",
       y="Consumo TC promedio") +
  theme(text = element_text(size=12), # Tama�o de fuente del gr�fico por defecto
        plot.title = element_text(size=rel(1), # Tama�o del t�tulo, doble del establecido por defecto
                                  vjust=2, #Justificaci�n vertical, para separarlo del gr�fico
                                  face="bold", #Letra negrilla
                                  color="darkgreen", #Color del texto
                                  lineheight=1.5), #Separaci�n entre l�neas)
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position="none",
        axis.title.x = element_text(face="bold", vjust=1.5, colour="blue", size=rel(1)),
        axis.title.y = element_text(face="bold", vjust=1.5, colour="blue", size=rel(1)),
        axis.text = element_text(colour = "black")) +
  coord_cartesian(ylim=c(0, 100000000)) +
  scale_y_continuous(breaks =seq(0,50, 10))

# El color violeta y rojo son los de mayor consumo de tarjeta de cr�dito.


# MOV90_CTA vs COLOR

ggplot(Segmentacion_COLOR, aes(x=COLOR, y=(Mov90_cta_promedio))) +
  geom_bar(stat="identity", position="dodge")  +
  theme_bw() +
  labs(title="",
       x="COLOR",
       y="Mov90_cta_promedio") +
  theme(text = element_text(size=12), # Tama�o de fuente del gr�fico por defecto
        plot.title = element_text(size=rel(1), # Tama�o del t�tulo, doble del establecido por defecto
                                  vjust=2, #Justificaci�n vertical, para separarlo del gr�fico
                                  face="bold", #Letra negrilla
                                  color="darkgreen", #Color del texto
                                  lineheight=1.5), #Separaci�n entre l�neas)
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position="none",
        axis.title.x = element_text(face="bold", vjust=1.5, colour="blue", size=rel(1)),
        axis.title.y = element_text(face="bold", vjust=1.5, colour="blue", size=rel(1)),
        axis.text = element_text(colour = "black")) +
  coord_cartesian(ylim=c(0, 150)) +
  scale_y_continuous(breaks =seq(0,50, 10))

# El color violeta y rojo son los de mayor movimieno de cuenta.


# SUELDO vs COLOR

ggplot(Segmentacion_COLOR, aes(x=COLOR, y=(Sueldo_prom))) +
  geom_bar(stat="identity", position="dodge")  +
  theme_bw() +
  labs(title="",
       x="COLOR",
       y="Sueldo_prom") +
  theme(text = element_text(size=12), # Tama�o de fuente del gr�fico por defecto
        plot.title = element_text(size=rel(1), # Tama�o del t�tulo, doble del establecido por defecto
                                  vjust=2, #Justificaci�n vertical, para separarlo del gr�fico
                                  face="bold", #Letra negrilla
                                  color="darkgreen", #Color del texto
                                  lineheight=1.5), #Separaci�n entre l�neas)
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position="none",
        axis.title.x = element_text(face="bold", vjust=1.5, colour="blue", size=rel(1)),
        axis.title.y = element_text(face="bold", vjust=1.5, colour="blue", size=rel(1)),
        axis.text = element_text(colour = "black")) +
  coord_cartesian(ylim=c(0, 100000)) +
  scale_y_continuous(breaks =seq(0,50, 10))

# Nuevamnete, el color violeta es el de mayor Suldo

# Es llamativo que los colores violeta principalemente y rojo tengan los valores promedio
# mayores en todas las variables cuantiativas- Ver� esto mejor en la parte de an�lisis de 
# Outliers.


# Variables Cuali vs CHURN

Segmentacion_CHURN <- dataset_banco%>%group_by(CHURN)%>%
  summarise(Edad_promedio = round(mean(EDAD,na.rm=TRUE),2),
            Consumo_TC_promedio = round(mean(CONSUMO_TC,na.rm=TRUE),2),
            Mov90_cta_promedio = round(mean(MOV90_CTA,na.rm=TRUE),2),
            Sueldo_prom = round(mean(SUELDO,na.rm=TRUE),2))

# Grafico las dos variables cuali que me parecen m�s relevantes (saco COnsumo TC por tener 
# muchos outliers)


# MOV90_Cta vs CHURN

ggplot(Segmentacion_CHURN, aes(x=CHURN, y=(Mov90_cta_promedio))) +
  geom_bar(stat="identity", position="dodge")  +
  theme_bw() +
  labs(title="",
       x="CHURN",
       y="Mov90_cta_promedio") +
  theme(text = element_text(size=12), # Tama�o de fuente del gr�fico por defecto
        plot.title = element_text(size=rel(1), # Tama�o del t�tulo, doble del establecido por defecto
                                  vjust=2, #Justificaci�n vertical, para separarlo del gr�fico
                                  face="bold", #Letra negrilla
                                  color="darkgreen", #Color del texto
                                  lineheight=1.5), #Separaci�n entre l�neas)
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position="none",
        axis.title.x = element_text(face="bold", vjust=1.5, colour="blue", size=rel(1)),
        axis.title.y = element_text(face="bold", vjust=1.5, colour="blue", size=rel(1)),
        axis.text = element_text(colour = "black")) +
  coord_cartesian(ylim=c(0, 50)) +
  scale_y_continuous(breaks =seq(0,50, 10))


# SUELDO vs CHURN

ggplot(Segmentacion_CHURN, aes(x=CHURN, y=(Sueldo_prom))) +
  geom_bar(stat="identity", position="dodge")  +
  theme_bw() +
  labs(title="",
       x="CHURN",
       y="Sueldo_prom") +
  theme(text = element_text(size=12), # Tama�o de fuente del gr�fico por defecto
        plot.title = element_text(size=rel(1), # Tama�o del t�tulo, doble del establecido por defecto
                                  vjust=2, #Justificaci�n vertical, para separarlo del gr�fico
                                  face="bold", #Letra negrilla
                                  color="darkgreen", #Color del texto
                                  lineheight=1.5), #Separaci�n entre l�neas)
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position="none",
        axis.title.x = element_text(face="bold", vjust=1.5, colour="blue", size=rel(1)),
        axis.title.y = element_text(face="bold", vjust=1.5, colour="blue", size=rel(1)),
        axis.text = element_text(colour = "black")) +
  coord_cartesian(ylim=c(0, 40000)) +
  scale_y_continuous(breaks =seq(0,50, 10))


# Observamos que tanto el promedio de MOV90_CTA como de SUELDO,es claramente menor para CHURN = SI 
# que para CHURN= NO


# Fin punto 4
####################################################################################################


#### PUNTO 5 - Chequeo de Outliers 

# La mejor manera de observar los outliers de las variables cuantitativas en forma individual, es mediante
# un boxplot por cada una:

boxplot(dataset_banco_cuanti$EDAD)

boxplot(dataset_banco_cuanti$ANTIGUEDAD)

boxplot(dataset_banco_cuanti$CONSUMO_TC)

boxplot(dataset_banco_cuanti$MOV90_CTA)

boxplot(dataset_banco_cuanti$SUELDO)



# Adicionalmente, y debido a lo observado en un punto anterior, realizo un An�lisis de Outliers en relaci�n
# a la variable COLOR

dataset_banco_OUTLIERS <-dataset_banco%>%mutate(EDAD = ifelse((EDAD > 105), "OUT", EDAD))%>%
  mutate(ANTIGUEDAD= ifelse((ANTIGUEDAD > quantile(dataset_banco$ANTIGUEDAD,0.95)), "OUT", ANTIGUEDAD))%>%
  mutate(CONSUMO_TC= ifelse((CONSUMO_TC > quantile(dataset_banco$CONSUMO_TC,0.95)), "OUT", CONSUMO_TC))%>%
  mutate(MOV90_CTA= ifelse((MOV90_CTA > quantile(dataset_banco$MOV90_CTA,0.95)), "OUT", MOV90_CTA))%>%
  mutate(SUELDO= ifelse((SUELDO > quantile(dataset_banco$SUELDO,0.95,na.rm = TRUE)), "OUT", SUELDO))

dataset_banco_OUTLIERS <- dataset_banco_OUTLIERS%>%mutate(TieneOuliers=ifelse(str_detect(EDAD,"OUT")|str_detect(ANTIGUEDAD,"OUT")|str_detect(CONSUMO_TC,"OUT")|str_detect(MOV90_CTA,"OUT")|str_detect(SUELDO,"OUT"),1,NA))


Suma_Outliers_por_COLOR <- dataset_banco_OUTLIERS%>%group_by(COLOR)%>%
  summarise(Suma_Outliers = sum(TieneOuliers,na.rm=TRUE))


# Cantiad de Outliers por COLOR

ggplot(Suma_Outliers_por_COLOR, aes(x=COLOR, y=(Suma_Outliers))) +
  geom_bar(stat="identity", position="dodge")  +
  theme_bw() +
  labs(title="",
       x="COLOR",
       y="Cant registros con outliers") +
  theme(text = element_text(size=12), # Tama�o de fuente del gr�fico por defecto
        plot.title = element_text(size=rel(1), # Tama�o del t�tulo, doble del establecido por defecto
                                  vjust=2, #Justificaci�n vertical, para separarlo del gr�fico
                                  face="bold", #Letra negrilla
                                  color="darkgreen", #Color del texto
                                  lineheight=1.5), #Separaci�n entre l�neas)
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position="none",
        axis.title.x = element_text(face="bold", vjust=1.5, colour="blue", size=rel(1)),
        axis.title.y = element_text(face="bold", vjust=1.5, colour="blue", size=rel(1)),
        axis.text = element_text(colour = "black")) +
  coord_cartesian(ylim=c(0, 1000)) +
  scale_y_continuous(breaks =seq(0,50, 10))


# Se observa que los clientes clasificados como color ROJo y VIOLETA son los que m�s outliers tienen
# en las variables cuantitativas.


# Elijo 3 variables con outliers y propongo qu� podr�a hacerse. La cantidad de outliers en los 3 casos
# es de alrededor del 5% de los datos.

# 1) EDAD: en principio, debido a que es seguro que las edades muy grandes (por ejemplo los mayores a 105)
# son errores, en primer lugar las imputar�a con la edad mediana, pero luego solicitar�a a los responsables que
# chequeen y completen correctametne la edad de esos clientes.

# 2) CONSUMO_TC: es la variable con valores m�s extremos y llamativos. Lo primero quer har�a es hablar con 
# los responsables del negocio para saber qu� valores m�ximos son posibles de acuerdo a cada segmento de 
# clientes. Los que sobrepasen esos valores, deber�n ser analizados en particular. Mientras tanto se 
# podr�a imputar por la media de la propia variable. 

# 3) SUELDO: es otra variable con valores extremos llamativos. Har�a exactamente lo mismo que con la 
# variable anterior.



# Fin punto 5
####################################################################################################


#### PUNTO 6 - Distancia de Mahalanobis (s�lo para registros completos)

dataset_banco_canti_completecases <- dataset_banco_cuanti[complete.cases(dataset_banco_cuanti), ]

cov <- cov.rob(dataset_banco_canti_completecases, method = "classical", nsamp = "best")


# Iniciamos iteracion

dcov = 0

for(i in 1:65){
  dcov[i]=mahalanobis(dataset_banco_canti_completecases[i,],cov$center, cov$cov, inverted =FALSE)
}
#local outlier factors using the LOF algorithm

distancia.outliers = lofactor(dataset_banco_canti_completecases , k=5)
print(distancia.outliers)

outliers=order(distancia.outliers, decreasing = T)[1:5]

print(outliers)


dataset_banco_canti_completecases2 <- cbind(dataset_banco_canti_completecases,distancia.outliers)
dataset_banco_canti_completecases2 <- mutate(dataset_banco_canti_completecases2, id=as.numeric(rownames(dataset_banco_canti_completecases2)))

ggplot(dataset_banco_canti_completecases2, aes(x=id, y=distancia.outliers)) +
  geom_point() + 
  geom_segment( aes(x=id, xend=id, y=0, yend=distancia.outliers))+
  geom_label(data=dataset_banco_canti_completecases2 %>% filter(distancia.outliers > 2), # Filter data first
             aes(label=id))



# Fin punto 6
####################################################################################################


#### PUNTO 7 - Correlograma y gr�ficos de perfiles


# Correlograma para variables cuantitativas - datos originales

cor_dataset_banco_cuanti <- cor(dataset_banco_cuanti, use = "pairwise.complete.obs")

corrplot(cor_dataset_banco_cuanti, method = "shade",shade.col = NA ,  tl.col = "black" ,
         tl.srt = 45, addCoef.col = "black")

# No se observan correlaciones fuertes entre ninguna de las variables



# Correlograma para variables cuantitativas - datos sin outliers 

cor_dataset_banco_cuanti_sinOUt <- cor(dataset_banco_SinOut_cuanti, use = "pairwise.complete.obs")

corrplot(cor_dataset_banco_cuanti_sinOUt, method = "shade",shade.col = NA ,  tl.col = "black" , 
         tl.srt = 45, addCoef.col = "black")

# En este caso, se nota una correlaci�n m�s alta que antes entre CONSUMO_TC y MOV90_CTA.


## Gr�ficos de Perfiles 

# Trabajo con datos sin NAs  por un lado y sin NAs y sin outliers por el otro.
# Tambi�n trabajar� con el dataset sin NAs pero con las variables cuanti estandarizadas(scale)


# Preparaci�n de datos

dataset_banco_sinNAs <- na.omit(dataset_banco)
dataset_banco_sinOut_sinNAs <- na.omit(dataset_banco_sinOut)
dataset_banco_sinNAS_escaled <- mutate_if(dataset_banco_sinNAs, is.numeric, scale, center = TRUE)

summary(dataset_banco_sinNAs)

# VARIABLE COLOR

# COLOR - SIN NAs datos originales

Factor_azul_SNA<-split(dataset_banco_sinNAs,dataset_banco_sinNAs$COLOR)$AZUL
Factor_verde_SNA<-split(dataset_banco_sinNAs,dataset_banco_sinNAs$COLOR)$VERDE
Factor_rojo_SNA<-split(dataset_banco_sinNAs,dataset_banco_sinNAs$COLOR)$ROJO
Factor_violeta_SNA<- split(dataset_banco_sinNAs,dataset_banco_sinNAs$COLOR)$VIOLETA

Media_azul_SNA <- apply(Factor_azul_SNA[ ,c(6,7,8,9,10)], 2, mean)
Media_verde_SNA <- apply(Factor_verde_SNA[ ,c(6,7,8,9,10)], 2, mean)
Media_rojo_SNA <- apply(Factor_rojo_SNA[ ,c(6,7,8,9,10)], 2, mean)
Media_violeta_SNA <-apply(Factor_violeta_SNA[ ,c(6,7,8,9,10)], 2, mean)

datos.plot_COLOR_SNA <-data.frame(group = c (1,2,3,4,5), value1=Media_azul_SNA, value2= Media_verde_SNA, 
                        value3= Media_rojo_SNA, value4=Media_violeta_SNA)

melteddatos_COLOR_SNA <-melt (datos.plot_COLOR_SNA, id = "group")

ggplot(melteddatos_COLOR_SNA , aes(x=group, y=value , colour= variable ))+
  geom_line()+
  xlab("Variables")+
  ylab ("Medias sin NAs") +
  scale_x_discrete(limit= c("1","2","3","4","5"),
                   labels = c("EDAD","ANTIGUEDAD","CONSUMO_TC","MOV90_CTA","SUELDO"))+
  labs (colour= "COLOR")+
  scale_colour_manual (labels= c ("VERDE", "AZUL", "ROJO","VIOLETA"),
                       values = c("green4", "royalblue","red","purple")) 


# La mayor�a de las variables quedan invisibilizadas debido a la escala del m�ximo de CONSUMO_TC
# por lo cual no es demasiado claro.


# COLOR SIN NAs y Sin Outliers - Datos originales

Factor_azul_SNAyOUT<-split(dataset_banco_sinOut_sinNAs,dataset_banco_sinOut_sinNAs$COLOR)$AZUL
Factor_verde_SNAyOUT <-split(dataset_banco_sinOut_sinNAs,dataset_banco_sinOut_sinNAs$COLOR)$VERDE
Factor_rojo_SNAyOUT <-split(dataset_banco_sinOut_sinNAs,dataset_banco_sinOut_sinNAs$COLOR)$ROJO
Factor_violeta_SNAyOUT <- split(dataset_banco_sinOut_sinNAs,dataset_banco_sinOut_sinNAs$COLOR)$VIOLETA

Media_azul_SNAyOUT <- apply(Factor_azul_SNAyOUT[ ,c(6,7,8,9,10)], 2, mean)
Media_verde_SNAyOUT <- apply(Factor_verde_SNAyOUT[ ,c(6,7,8,9,10)], 2, mean)
Media_rojo_SNAyOUT <- apply(Factor_rojo_SNAyOUT[ ,c(6,7,8,9,10)], 2, mean)
Media_violeta_SNAyOUT <-apply(Factor_violeta_SNAyOUT[ ,c(6,7,8,9,10)], 2, mean)

datos.plot_COLOR_SNAyOUT <-data.frame(group = c (1,2,3,4,5), value1=Media_azul_SNAyOUT, value2= Media_verde_SNAyOUT, 
                        value3= Media_rojo_SNAyOUT, value4=Media_violeta_SNAyOUT)

melteddatos_COLOR_SNAyOUT <-melt (datos.plot_COLOR_SNAyOUT, id = "group")

ggplot(melteddatos_COLOR_SNAyOUT, aes(x=group, y=value , colour= variable ))+
  geom_line()+
  xlab("Variables")+
  ylab ("Medias sin NAs y sin Outliers") +
  scale_x_discrete(limit= c("1","2","3","4","5"),
                   labels = c("EDAD","ANTIGUEDAD","CONSUMO_TC","MOV90_CTA","SUELDO"))+
  labs (colour= "COLOR")+
  scale_colour_manual (labels= c ("VERDE", "AZUL", "ROJO","VIOLETA"),
                       values = c("green4", "royalblue","red","purple")) 

# En este caso, sacando los outliers, el gr�fico mejora un poco pero ahora la variable SUELDO
# sigue teniendo una escala muy grande para el resto de las variables.



# COLOR - SIN NAs - Datos estandarizados

Factor_azul_SNA_std<-split(dataset_banco_sinNAS_escaled,dataset_banco_sinNAS_escaled$COLOR)$AZUL
Factor_verde_SNA_std<-split(dataset_banco_sinNAS_escaled,dataset_banco_sinNAS_escaled$COLOR)$VERDE
Factor_rojo_SNA_std<-split(dataset_banco_sinNAS_escaled,dataset_banco_sinNAS_escaled$COLOR)$ROJO
Factor_violeta_SNA_std<- split(dataset_banco_sinNAS_escaled,dataset_banco_sinNAS_escaled$COLOR)$VIOLETA

Media_azul_SNA_std <- apply(Factor_azul_SNA_std[ ,c(6,7,8,9,10)], 2, mean)
Media_verde_SNA_std <- apply(Factor_verde_SNA_std[ ,c(6,7,8,9,10)], 2, mean)
Media_rojo_SNA_std <- apply(Factor_rojo_SNA_std[ ,c(6,7,8,9,10)], 2, mean)
Media_violeta_SNA_std <-apply(Factor_violeta_SNA_std[ ,c(6,7,8,9,10)], 2, mean)

datos.plot_COLOR_SNA_std <-data.frame(group = c (1,2,3,4,5), value1=Media_azul_SNA_std, value2= Media_verde_SNA_std, 
                                  value3= Media_rojo_SNA_std, value4=Media_violeta_SNA_std)

melteddatos_COLOR_SNA_std <-melt (datos.plot_COLOR_SNA_std, id = "group")

ggplot(melteddatos_COLOR_SNA_std , aes(x=group, y=value , colour= variable ))+
  geom_line()+
  xlab("Variables")+
  ylab ("Medias sin NAs - Estandarizado") +
  scale_x_discrete(limit= c("1","2","3","4","5"),
                   labels = c("EDAD","ANTIGUEDAD","CONSUMO_TC","MOV90_CTA","SUELDO"))+
  labs (colour= "COLOR")+
  scale_colour_manual (labels= c ("VERDE", "AZUL", "ROJO","VIOLETA"),
                       values = c("green4", "royalblue","red","purple")) 


# Ahora s�, con los datos estandarizados pueden observarse las diferencias entre los distintos
# colores, dentro de cada variable- Obviamente, no se puede comparar entre variables ya que la
# escala no es la real.


# Fin punto 7
####################################################################################################


#### PUNTO 8 - Muestreo del 20% simple sin reposici�n y verificaci�n de CHURN


# Muestreo simple al azar sin reposici�n (con semilla) 

set.seed(2214)
dataset_banco_3636_s <- sample(1:nrow(dataset_banco),size=3636,replace=FALSE)

# Asignar los elementos de la muestra al data frame de datos

dataset_banco_muestra_20porc <- dataset_banco[dataset_banco_3636_s, ]

# Verifico ahora si cambi� o no la proporci�n de CHURN

freq(dataset_banco$CHURN)

# % de CHURN en base origina: SI 1.06%; NO 98.94%

freq(dataset_banco_muestra_20porc$CHURN)

# % de CHURN en base origina: SI 0.83%; NO 99.17%

# El porcentaje de CHURN= SI disminuy� en la muestra un 22% pasando de 1.06% a 0.83%

# Fin punto 8 - FIN DEL TP
####################################################################################################
