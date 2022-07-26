
#limpio la memoria
rm( list=ls() )
gc()

#install.packages(tidyverse)
#install.packages(gtools)
#install.packages(funModeling)


###############################
#- Manipular datasets en R - Germ�n May
###############################

setwd("C:/Users/User/Documents/Mis documentos/German/MCD Austral/AID/TPs/TP1")
# setwd("C:/Users/German/Documents/Personal/MCD Austral/AID/TPs/TP1")

getwd() #chequeo en que carpeta estoy

library(tidyverse)
library(gtools)
library(summarytools) # para el uso de estad�sticas descriptivas
library(dplyr)
library(ggplot2)
library(funModeling)
options(scipen = 999) ### turn off scientific notation

# PUNTO 1

# Cargo las tablas de recarga de los tres meses

recargas_enero <- read.csv("RECA_CHAN_01_NEW.csv", stringsAsFactors=TRUE)

recargas_febrero <- read.csv("RECA_CHAN_02_NEW.csv", stringsAsFactors=TRUE)

recargas_marzo <- read.csv("RECA_CHAN_03_NEW.csv", stringsAsFactors=TRUE)


# Verifico que sean compatibles y las unifico en una sola

recargas_1erTrimestre  <- rbind(recargas_enero, recargas_febrero,recargas_marzo )

# La convierto en otra tabla filtrada por "purchaseamount" y elimino columnas innecesarias.


recargas_1erTrimestre2 <- recargas_1erTrimestre %>%
  filter(PURCHASEAMOUNT > 0)%>%
  select(-c("X","RUNID","PURCHASETIME"))


# Ahora cargo base de Clientes

clientes <- read.csv("DNA_03_NEW.csv", stringsAsFactors=TRUE)

# Filtro por clientes activos y elimino columna innecesaria

clientes <- clientes%>%
  filter(BASE_STAT_03 == "ACTIVE BASE" | BASE_STAT_03 == "REJOINNER" )%>%
  select(-c("X"))


# Uno la base de clientes con la de recargas, manteniendo todos los registros de la de clientes


Clientes_Recargas <-left_join (clientes,recargas_1erTrimestre2, by=c("ACCS_MTHD_CD"="CUSTOMERID"))



# Cambio los NAs de "purchaseamount" por Cero y agrego columna que clasifica si el tipo de canal
# es Manual o Tecno. 


Clientes_Recargas2=Clientes_Recargas%>%mutate_at("PURCHASEAMOUNT",
                                                 ~replace(., is.na(.), 0))%>%
  mutate(CHANNELTYPE = if_else(str_detect(CHANNELIDENTIFIER,"EMG") , "Tecno", "Manual"))


# (sigue habiendo NAs en "CHANNELIDENTIFIER" Y "CHANNELTYPE" pero al menos por ahora no molestan)
  
# Convierto nueva columna en Factor ..

Clientes_Recargas2$CHANNELTYPE <- as.factor(Clientes_Recargas2$CHANNELTYPE)



# Ahora que tengo las bases unidas puedo comenzar a realizar los agrupamientos y c�lculos.
# Confecciono tablas para cada c�lculo agrupado por separado para evitar errores y despu�s las uno.


# Agrupo por cliente y calculo el monto total por cliente. Me aseguro de mantener todas las columnas
# porque �sta es la primera tabla que usar� de base para unir todas las restantes

MONTO_TOTAL_por_cliente <- Clientes_Recargas2 %>%
  group_by(ACCS_MTHD_CD,BASE_STAT_03)%>%
  summarise(MONTO_TOTAL = sum(PURCHASEAMOUNT,na.rm=TRUE))



# Agrupo por cliente y calculo Cantidad de recargas. 

CANT_RECARGAS_por_cliente <- Clientes_Recargas2 %>%
  filter(PURCHASEAMOUNT>0)%>%
  group_by(ACCS_MTHD_CD)%>%
  summarise(CANT_RECARGAS = n())


# Calculo el monto tecno por cliente

# primero filtro por tecno, luego agrupo y sumarizo

MONTO_TECNO_por_cliente <- Clientes_Recargas2 %>%
  filter(CHANNELTYPE=="Tecno")%>%
  group_by(ACCS_MTHD_CD)%>%
  summarise(MONTO_TECNO = sum(PURCHASEAMOUNT,na.rm=TRUE))


# Calculo el cantidad de recargas t�cnicas por cliente

# primero filtro por tecno, luego agrupo y sumarizo

Cant_Rec_Tecno_por_cliente <- Clientes_Recargas2 %>%
  filter(CHANNELTYPE=="Tecno")%>%
  group_by(ACCS_MTHD_CD)%>%
  summarise(CANT_RTEC = n())


# Ahora uno todas las tablas 

recargas_por_cliente_borrador1 <-left_join(MONTO_TOTAL_por_cliente,CANT_RECARGAS_por_cliente)
recargas_por_cliente_borrador2 <-left_join(recargas_por_cliente_borrador1,MONTO_TECNO_por_cliente)
recargas_por_cliente<-left_join(recargas_por_cliente_borrador2,Cant_Rec_Tecno_por_cliente)


# Reemplazo los NAs por cero

recargas_por_cliente1 <- mutate_at(recargas_por_cliente, c("CANT_RECARGAS","CANT_RTEC","MONTO_TECNO"),
                                   ~replace(., is.na(.), 0))
 
 
#Ahora agrego las columnas calculadas seg�n lo pedido

recargas_por_cliente2 <-recargas_por_cliente1%>%
  mutate(POR_TECNO_M= round(MONTO_TECNO/MONTO_TOTAL*100,1))%>%
  mutate(POR_TECNO= round(CANT_RTEC/CANT_RECARGAS*100,1))%>%
  mutate(CL_TECNO= case_when (CANT_RECARGAS < 3 ~ "99-NOSEGM",
                              MONTO_TECNO == 0 ~ "4-No_Tecno",
                              POR_TECNO >= 70 ~ "1-Tecno",
                              POR_TECNO < 70 & POR_TECNO >= 40 ~ "2-Mix4070",
                              POR_TECNO < 40 & POR_TECNO > 0 ~ "3-MixH40"))

recargas_por_cliente2$CL_TECNO <- as.factor(recargas_por_cliente2$CL_TECNO)


# Reemplazo los NAs generados por cero en las nuevas columnas 

recargas_por_cliente3 <- mutate_at(recargas_por_cliente2, c("POR_TECNO_M","POR_TECNO"),
                                   ~replace(., is.na(.), 0))


# Verifico lo hecho hasta ac� -  Se ven bien el resultado

summary(recargas_por_cliente3)



# Cambio la ubicaci�n de las columnas, renombro tabla y ordeno por cliente seg�n lo solicitado.

Clientes_Mar21 <-recargas_por_cliente3 %>%select(ACCS_MTHD_CD,BASE_STAT_03,MONTO_TOTAL,MONTO_TECNO,
                                          POR_TECNO_M,CANT_RECARGAS,CANT_RTEC,POR_TECNO,CL_TECNO)%>%
  arrange(ACCS_MTHD_CD)

# Guardo la tabla en formato .csv en el directorio de trabajo.

write.csv(Clientes_Mar21, "Clientes_Mar21.csv")


# #################################### Fin punto 1 ################################################

# PUNTO 2

# En base a la tabla final del punto 1, confecciono la tabla resumen del punto 2

Segmentacion_Mar21 <- Clientes_Mar21%>%group_by(CL_TECNO)%>%
  summarise(Edad_promedio = round(mean(EDAD,na.rm=TRUE),2),
            Monto_tecno_promedio = round(mean(MONTO_TECNO,na.rm=TRUE),2),
            Cant_recargas_promedio = round(mean(CANT_RECARGAS,na.rm=TRUE),2),
            Cant_rec_tecno_prom = round(mean(CANT_RTEC,na.rm=TRUE),2))


# Guardo la tabla en formato .csv en el directorio de trabajo.

write.csv(Segmentacion_Mar21, "Segmentacion_Mar21.csv")

# Borro todas las tablas intermedias y dejo las finales
# rm(list = ls()[ls() != "Clientes_Mar21"& ls() != "Segmentacion_Mar21"])

###################################### Fin punto 2 ################################################

# PUNTO 3 - CONCLUSIONES GENERALES AL FINAL DEL PUNTO 

# En principio, es probable que si pudi�ra hablar con personal de la empresa, tal vez me
# dir�an que s�lo le interesa la informaci�n de los clientes que relizaron recargas. Como no lo s�
# todo el an�lisis es realizado con la base completa aunque al final dejo una recomendaci�n.

# Estad�stica descriptiva del punto 1

### Inicio con exploraci�n general

# Visualizo resumen estad�stico en la consola

summary(Clientes_Mar21)

# Verifico que no hay NAs, que los valores m�nimos de las variables cuantitativas son
# razonables y los m�ximos en algunos casos como ser en Monto Total, parecen ser outliers.
# Llama la atenci�n que en todos las varibles cuantitativas relacionadas a Tecno,
# las medianas dan cero y supongo que esto es por la gran cantidad de datos iguales a cero 
# en estas columnas.


view(dfSummary(Clientes_Mar21))

# En este resumen gr�fico y num�rico tan completo podemos ver un resumen de cada variable.
# Destaco la distribuci�n de frecuencia del tipo de cliente CL_TECNO. M�s adelante realizo un
# an�lisis detallado de cada una.


plot_num(Clientes_Mar21)

# Aqu� observamos en detalle y a color los histogramas de las variables cuantitativas.
# Luego analizo cada uno.


### GRAFICOS PARA VARIABLES CUALITATIVAS


# Grafico de barras Cantidad de Clientes por tipo (CL_TECNO)


ggplot(Clientes_Mar21, aes(x=CL_TECNO))+
  geom_bar(stat="count", width=0.4, fill="steelblue")+
  labs(x="Tipo CL_TECNO",  y="Cantidad de clientes",color="Tipo")+
  ggtitle("Cantidad de clientes por tipo (CL_TECNO)")

# Se observa que la mayor�a de los clientes son No Tecno o que han realizado menos de 3 recargas
# en el trimestre.


# Grafico de barras Cantidad de Clientes por tipo (BASE_STAT_03)


ggplot(Clientes_Mar21, aes(x=BASE_STAT_03))+
  geom_bar(stat="count", width=0.4, fill="steelblue")+
  labs(x="Tipo BASE_STAT_03",  y="Cantidad de clientes",color="Tipo")+
  ggtitle("Cantidad de clientes por tipo(BASE_STAT_03)")

# Se observa que la gran mayor�a de los clientes son ACTIVE_BASE



# Ahora busco relacionar las dos variables cualitativas a trav�s de una tabla de contingencia relativa

prop.table(table(Clientes_Mar21$CL_TECNO,Clientes_Mar21$BASE_STAT_03),margin = 2)

# Se observa en la consola que la distribuci�n del tipo de clientes seg�n CL_TECNO es
# distinta para los clientes Active_Base que para los Rejoinner. Por ejemplo, para los
# Rejoinner el 91,4% son 99-NOSEGEM en cambio para los Active_Base es el 36,4%



### GRAFICOS CON VARIABLES CUANTITATIVAS

# Realizo ahora histogramas y box-plots para analizar cada variable. En el caso de los Box-plots
# grafico el completo y el que no tiene outliers (no los muestra pero s� los tiene en cuenta para los
# c�lculos) para visualizar mejor la mayor�a de los datos.


# ANALISIS DE MONTO TOTAL

# Box-plot completo

# S�lo de monto total, completo

ggplot(Clientes_Mar21, aes(y=MONTO_TOTAL)) + 
  geom_boxplot()

# Se observa una gran cantidad de outliers en hacia los valores mayores 

# S�lo de monto total, sin graficar outliers

boxplot(Clientes_Mar21$MONTO_TOTAL,outline = FALSE)

# Se observa el RIQ entre 5 y 44 (seg�n Summary) y una caja con mayor concentraci�n en la parte baja.

# Box plott Por Categor�a de clientes CL_TECNO 

ggplot(Clientes_Mar21, aes(x=CL_TECNO, y=MONTO_TOTAL)) + 
  geom_boxplot()

# Box-plot sin graficar outliers 


boxplot(Clientes_Mar21$MONTO_TOTAL~ Clientes_Mar21$CL_TECNO,outline = FALSE)

# Se observa mayor consumo para la categor�a 3-MixH40, La dispersi�n seg�n el RIQ es similar
# en todas las categor�as excepto para la 99-NOSEGM.


# Histograma

ggplot(data=Clientes_Mar21)+
  geom_histogram(aes(x=MONTO_TOTAL), fill="blue", alpha=.4,bins=30)

# se nota una distribuci�n muy asim�trica con una gran cola dercha



# ANALISIS DE MONTO TECNO

# Box-plot completo

# S�lo de monto tecno, completo

ggplot(Clientes_Mar21, aes(y=MONTO_TECNO)) + 
  geom_boxplot()

# Se observa una gran cantidad de outliers en hacia los valores mayores y un caja casi impercetible
# cercana a valores cero

# S�lo de monto tecno, sin graficar outliers

boxplot(Clientes_Mar21$MONTO_TECNO,outline = FALSE)

# Al haber tantos valores iguales a cero por todos los cliente que no realizn recargas Tecno, la caja
# se observa muy especial con Q1 y Media = 0.

# Box plott Por Categor�a de clientes CL_TECNO 

ggplot(Clientes_Mar21, aes(x=CL_TECNO, y=MONTO_TECNO)) + 
  geom_boxplot()

# Box-plot sin graficar outliers 


boxplot(Clientes_Mar21$MONTO_TECNO~ Clientes_Mar21$CL_TECNO,outline = FALSE)

# El gr�fico es l�gico mostrando mayor consumo tecno en clientes con mayor cantidad de recargas tecno.
# Tambi�n se observa que aumenta la variabilidad con el mayor consumo.


# Histograma

ggplot(data=Clientes_Mar21)+
  geom_histogram(aes(x=MONTO_TECNO), fill="blue", alpha=.4,bins=30)

# se nota una distribuci�n muy asim�trica con una gran cola derecha y el modo muy cercano a cero.



# ANALISIS DE CANTIDAD DE RECARGAS

# Box-plot completo

# S�lo de cantidad de recargas, completo

ggplot(Clientes_Mar21, aes(y=CANT_RECARGAS)) + 
  geom_boxplot()

# Se observa una gran cantidad de outliers en hacia los valores mayores 


# S�lo de cantidad de recargas, sin graficar outliers

boxplot(Clientes_Mar21$CANT_RECARGAS,outline = FALSE)

# Se observa claramente el RIQ que seg�n (summary) est� entre 1 y 10 recargas con una mediana de 4.

# Box plott Por Categor�a de clientes CL_TECNO 

ggplot(Clientes_Mar21, aes(x=CL_TECNO, y=CANT_RECARGAS)) + 
  geom_boxplot()

# Box-plot sin graficar outliers 


boxplot(Clientes_Mar21$CANT_RECARGAS~ Clientes_Mar21$CL_TECNO,outline = FALSE)

# Se observa mayor cantidad de  recargas para la categor�a 3-MixH40. La dispersi�n seg�n el RIQ es similar
# en todas las categor�as excepto para la 99-NOSEGM.


# Histograma

ggplot(data=Clientes_Mar21)+
  geom_histogram(aes(x=CANT_RECARGAS), fill="blue", alpha=.4,bins=30)

# se nota una distribuci�n muy asim�trica con una gran cola derecha y el modo muy cercano a cero.



# ANALISIS DE CANTIDAD DE RECARGAS TECNO

# Box-plot completo

# S�lo de cantidad de recargas tecno, completo

ggplot(Clientes_Mar21, aes(y=CANT_RTEC)) + 
  geom_boxplot()

# Se observa una gran cantidad de outliers en hacia los valores mayores 


# S�lo de cantidad de recargas tecno, sin graficar outliers

boxplot(Clientes_Mar21$CANT_RTEC,outline = FALSE)

# Se observa el RIQ entre 0 y 1 con mediana igual a cero debido a la gran cantidad recargas tecno = 0

# Box plott Por Categor�a de clientes CL_TECNO 

ggplot(Clientes_Mar21, aes(x=CL_TECNO, y=CANT_RTEC)) + 
  geom_boxplot()

# Box-plot sin graficar outliers 


boxplot(Clientes_Mar21$CANT_RTEC~ Clientes_Mar21$CL_TECNO,outline = FALSE)

# El gr�fico es l�gico con la clasificaci� CL_TECNO. Llama la atenci�n la dispersi�n distinta
# que se observa en la categor�a 3-MixH40 concentrada en la parte baja de la caja.


# Histograma

ggplot(data=Clientes_Mar21)+
  geom_histogram(aes(x=CANT_RTEC), fill="blue", alpha=.4,bins=30)

# se nota una distribuci�n muy asim�trica con una gran cola derecha y el modo muy cercano a cero.



# ANALISIS DE POR_TECNO_M (% de Monto tecno sobre monto total)

# Box-plot completo

# S�lo de por tecno M, completo

ggplot(Clientes_Mar21, aes(y=POR_TECNO_M)) + 
geom_boxplot()

# Se observa una caja apoyada sobre el cero y gran cantidad de outliers en hacia los valores mayores 


# S�lo de cantidad de por tecno M, sin graficar outliers

boxplot(Clientes_Mar21$POR_TECNO_M,outline = FALSE)

# Se observa el RIQ entre 0 y 7.9 con mediana igual a cero debido a la gran cantidad recargas tecno = 0

# Box plott Por Categor�a de clientes CL_TECNO 

ggplot(Clientes_Mar21, aes(x=CL_TECNO, y=POR_TECNO_M)) + 
  geom_boxplot()

# Llamativo gr�fico debido a que se refiere al % pero l�gico por la clasificaci�n hecha por CL_TECNO.

# Box-plot sin graficar outliers 


boxplot(Clientes_Mar21$POR_TECNO_M~ Clientes_Mar21$CL_TECNO,outline = FALSE)


# Histograma

ggplot(data=Clientes_Mar21)+
  geom_histogram(aes(x=POR_TECNO_M), fill="blue", alpha=.4,bins=30)

# Se nota una distribuci�n muy asim�trica y bimodal con una gran cola derecha, uno de los
# picos (principal) cercano a cero y otro menor en 100.



# ANALISIS DE POR_TECNO (% de recargas tecno sobre totales)

# Box-plot completo

# S�lo de por tecno, completo

ggplot(Clientes_Mar21, aes(y=POR_TECNO)) + 
  geom_boxplot()

# Se observa una caja apoyada sobre el cero y gran cantidad de outliers en hacia los valores mayores 


# S�lo de cantidad de por tecno, sin graficar outliers

boxplot(Clientes_Mar21$POR_TECNO,outline = FALSE)

# Se observa el RIQ entre 0 y 8.3 con mediana igual a cero debido a la gran cantidad recargas tecno = 0

# Box plott Por Categor�a de clientes CL_TECNO 

ggplot(Clientes_Mar21, aes(x=CL_TECNO, y=POR_TECNO)) + 
  geom_boxplot()

# Llamativo gr�fico debido a que se refiere al % pero l�gico por la clasificaci�n hecha por CL_TECNO.

# Box-plot sin graficar outliers 


boxplot(Clientes_Mar21$POR_TECNO~ Clientes_Mar21$CL_TECNO,outline = FALSE)


# Histograma

ggplot(data=Clientes_Mar21)+
  geom_histogram(aes(x=POR_TECNO), fill="blue", alpha=.4,bins=30)

# Se nota una distribuci�n muy asim�trica y bimodal con una gran cola derecha, uno de los
# picos (principal) cercano a cero y otro menor en 100.


# Finalmente, realizo un gr�fico de dispersi�n m�ltiple para verificar posibles relaciones
# entre variables cuantitativas aunque a priori entiendo que todas salen b�sicamente
# de monto y cantidad de recargas. La relaci�n que se ve es indeterminada o muy difusa.
# El c�digo lo dejo bloqueado con # poque es muy pesado para ejecutar.

# pairs(~MONTO_TOTAL+MONTO_TECNO+CANT_RECARGAS+CANT_RTEC,data=Clientes_Mar21, 
#       main="Matriz de dispersi�n")



# CONCLUSIONES GENERALES DEL PUNTO 3


# * Hay una cantidad importante de clientes que no realizan recargas tecnol�gicas (39.9%)  y otra
# cantidad tambi�n grande (38,5%>) que realiza muy pocas recargas (menos de 3 en un trimestre).

# * El 96,4 % de los clientes analizados son del tipo Active_Base frente al 3,6% de Rejoinner.

# * Si bien la informaci�n sobre la cantidad de clientes que casi no realizan recargas es 
# importante debido al marcado potencial que significan , se podr�a pensar en realizar un 
# an�lisis paralelo sin tenerlos en cuenta de modo de poder analizar mejor los datos de los
# que s� las realizan. 

# * En relaci�n con el punto anterior, la gran cantidad de valores "cero" en muchas de las 
# variables, produce distribuciones muy asim�tricas. De hecho, en varias tanto Q1 como Q2 
# son cero.

# * Todas las variables cuantitativas se caracterizan por tener muchos outliers.

# * la distribuci�n del tipo de clientes seg�n CL_TECNO es  distinta para los clientes 
# Active_Base que para los Rejoinner. Por ejemplo, para los Rejoinner el 91,4% son 
# 99-NOSEGEM en cambio para los Active_Base es el 36,4%.

# * Hay mayor variabilidad en el monto de consumo dentro de los clientes TECNO.

# * Se observa mayor cantidad de recargas en general para la categor�a 3-MixH40.

# * En cuanto a las recargas TECNO, llama la atenci�n la dispersi�n distinta que se observa
# en la categor�a 3-MixH40 concentrada en la parte baja de la caja.

# * En los histogramas de las variables calculadas como % de monto TECNO y % de 
# recargas TECO, se observa una distribuci�n bimodal con una moda grande cercana al 
# cero (l�gica por lo comentado al principio) y otra m�s baja sobre los valores m�s altos. 



###################################### Fin punto 3 ################################################

# PUNTO 4

# Primero confecciono los gr�ficos de barras separadas para cada variable cuantitativa por
# tipo de cliente CL_TECNO. Luego resumo al final las conclusiones.


# Monto Total promedio seg�n tipo de cliente.

ggplot(Segmentacion_Mar21, aes(x=CL_TECNO, y=(Monto_total_promedio))) +
  geom_bar(stat="identity", position="dodge")  +
  theme_bw() +
  labs(title="Monto Total Promedio por Tipo de Cliente",
       x="Tipo de Cliente CL_TECNO",
       y="promedio monto $") +
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


# Monto Tecno promedio seg�n tipo de cliente.

ggplot(Segmentacion_Mar21, aes(x=CL_TECNO, y=(Monto_tecno_promedio))) +
  geom_bar(stat="identity", position="dodge")  +
  theme_bw() +
  labs(title="Monto Tecno Promedio por Tipo de Cliente",
       x="Tipo de Cliente CL_TECNO",
       y="promedio monto $") +
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


# Cantidad de Recargas Promedio por Tipo de Cliente.

ggplot(Segmentacion_Mar21, aes(x=CL_TECNO, y=(Cant_recargas_promedio))) +
  geom_bar(stat="identity", position="dodge")  +
  theme_bw() +
  labs(title="Cantidad de Recargas Promedio por Tipo de Cliente",
       x="Tipo de Cliente CL_TECNO",
       y="Cant. recargas") +
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
  coord_cartesian(ylim=c(0, 12)) +
  scale_y_continuous(breaks =seq(0,12, 2))



# Cantidad de Recargas Tecnol�gicas Promedio por Tipo de Cliente

ggplot(Segmentacion_Mar21, aes(x=CL_TECNO, y=(Cant_rec_tecno_prom))) +
  geom_bar(stat="identity", position="dodge")  +
  theme_bw() +
  labs(title="Cantidad de Recargas Tecnol�gicas Promedio por Tipo de Cliente",
       x="Tipo de Cliente CL_TECNO",
       y="Cant. recargas") +
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
  coord_cartesian(ylim=c(0, 12)) +
  scale_y_continuous(breaks =seq(0,12, 2))



# CONCLUSIONES GENERALES DEL PUNTO 4

# * Observando tanto la tabla como los gr�ficos, no se ve que existan grandes diferencias
# en el monto total promediO ni en la cantidad de recargas promedio, entre los distimntos
# segmentos (dejando de lado obviamente los de la categor�a  99_NOSEGM). Solamente cabe 
# destacar lo siguiente:

# * Los clientes NO_TECNO son los que, en promedio,  menos cantidad de recargas hacen
# y menos dinero total gastan en recargas.

# * Los clientes 3-MIXH40 son los que, en promedio,  m�s cantidad de recargas hacen
# y m�s dinero total gastan en recargas.

