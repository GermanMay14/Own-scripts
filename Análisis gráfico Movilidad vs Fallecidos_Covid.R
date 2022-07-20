# Gráfico comparativo de evolución de tasa de movilidad y fallecidos por COVID en CABA.
# Autores: COMPLETAR
# Inicio: 19-04-21  

# Inicio común a muchos scripts: Verifica que estén instalados los paquetes necesarios. 
# Los nuevos paquetes agregarlos al vector "packages_needed"

check_packages <- function(packages) {
  if (all(packages %in% rownames(installed.packages()))) {
    TRUE
  } else{
    cat(
      "Instalar los siguientes packages antes de ejecutar el presente script\n",
      packages[!(packages %in% rownames(installed.packages()))],
      "\n"
    )
  }
}
packages_needed <- c("tidyverse","ggplot2", "ggrepel", "plotly",
                     "lubridate", "htmlwidgets" , "RColorBrewer",
                     "sqldf", "grid", "data.table", "readr", "easypackages" )
check_packages(packages_needed)

# Activación de librerías de los paquetes (los trae a la RAM para ser usados)
# Primero instalamos la librería "easypackes" para instalar todo el resto

library(easypackages)
libraries(packages_needed)

# Desarrollo del script 

##################################################################

# Se importa la URL de Mobilidad de Apple donde se filtrará la tasa de movilidad de CABA de los caminantes
# Atención,es necesario actualizar cuando sea necesario la ruta y la fecha en el nombre de archivo

mobility_url <- "https://covid19-static.cdn-apple.com/covid19-mobility-data/2106HotfixDev25/v3/en-us/"

aux <-  paste(mobility_url, "applemobilitytrends-2021-04-29.csv", sep = "")
mobility   <- read.csv(aux, sep = ",", header = T)

# Se pasan algunas columnas a formato Factor


mobility$region                <- as.factor(mobility$region)
mobility$transportation_type   <- as.factor(mobility$transportation_type)

# Se crea un nuevo set de datos. Se pasa del formato ancho al formato largo (todas las columnas de fecha)
# Se crea las columnas nuevas "fecha" y "tasa" donde se almacenan los datos originales de las
# columnas de fecha



library(tidyr)
datos_v   <- mobility %>% gather(fecha, tasa    , 7:ncol(mobility))

# otro set de datos donde se cambia el formato de la fecha

datos_v$fecha <- as.Date(datos_v$fecha, format = "X%Y.%m.%d")

#  si se quiere ver otro pais se modifica aquí.
# Se filtra por "Buenos Aires" y tipo de transporte "caminar"

pais = "Buenos Aires"
trans = "walking"
datos <- subset(datos_v, region == pais & transportation_type == trans)

datos <-  select( datos, fecha, tasa)

######  fin de preparacion de los datos de Movilidad



###########################################################################################

# Ahora se importa y se trabaja con los datos de Fallecidos por Covid en Bs As
# Se abre la base de excel que bajamos previamente de:
# https://cdn.buenosaires.gob.ar/datosabiertos/datasets/salud/casos-covid-19/casos_covid19.csv
# Se lee el archivo desde su ubicación en la PC. Verificar que la ruta sea correcta al correr el script.


library(readr)
casos_covid19_CDN <- read_csv("casos_covid19.csv", 
                              locale = locale(grouping_mark = ""))
View(casos_covid19_CDN)

# Filtramos por CABA y Fallecidos


Cant_Fallecidos_Caba <- filter (casos_covid19_CDN, provincia=="CABA" & fallecido=="si")

# Se selecciona  sólo la columna de "fecha de fallecimiento"

Cant_Fallecidos_Caba <- select(Cant_Fallecidos_Caba,"fecha_fallecimiento")


# Se convierte en formato fecha la columna "fecha_fallecimiento"

Cant_Fallecidos_Caba <- Cant_Fallecidos_Caba%>% mutate(fecha_fallecimiento = as.Date(parse_date_time(fecha_fallecimiento,"d b Y H M S")))


# Se agrupa y cuenta cantidad de fallecidos por fecha, ordenando y renombrando columnas

Cant_Fallecidos_Caba <- Cant_Fallecidos_Caba %>% count(fecha_fallecimiento, sort = TRUE) %>% arrange(fecha_fallecimiento)

colnames(Cant_Fallecidos_Caba) <- c("fecha","Cant_Fallecidos_Caba")

# Fin de preparación de los datos de Fallecidos por Covid

##################################################################################################

# Se unen las dos tablas en una sola tomando "fecha" como key (lo toma solo porque coincide el nombre de la variable en ambas tablas)


Comp_mov_fallecidos <- merge(x= datos, y= Cant_Fallecidos_Caba)


library(zoo) 
library(dplyr)

Comp_mov_fallecidos <-  Comp_mov_fallecidos  %>% mutate(media_movil_tasa = rollmean(tasa, k = 7 , fill = NA, align = "right")) %>% mutate(media_movil_fallecidos = rollmean(Cant_Fallecidos_Caba, k = 7 , fill = NA, align = "right"))

Comp_mov_fallecidos <-Comp_mov_fallecidos[-c(1:64),]

###########################################################################

# Finalmente, se grafican las dos variables en función del tiempo 

library(plot)
library(plotly)

p <-  ggplotly(ggplot( show.legend = TRUE) +
  geom_line(data = Comp_mov_fallecidos,  aes(x = fecha, y=tasa, color = "Movilidad"),alpha=0.5 ) +
  geom_line(data = Comp_mov_fallecidos, aes( x = fecha, y=Cant_Fallecidos_Caba, color = "Fallecidos" ),alpha=0.5) +
  geom_smooth(data = Comp_mov_fallecidos,  aes(x = fecha, y=Cant_Fallecidos_Caba))+
  geom_smooth(data = Comp_mov_fallecidos,  aes(x = fecha, y=tasa))+
  xlab('') +
  ylab('') +
    scale_color_manual(name="Groups",values=c("Movilidad"="red",
                                              "Fallecidos"= "blue"))+
    guides(colour = guide_legend(override.aes = list(linetype = 1)))+
  scale_x_date(date_breaks = "30 day", date_labels =  "%b%y") +
  scale_y_continuous(sec.axis = sec_axis(~., name = "Fallecidos")) +
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10))) %>%
  layout(title = list(text = paste0('COVID-19: Evolucion Movilidad y Fallecimientos en CABA 2020-2021 ',
                                    '<br>',
                                    '<sup>',
                                    ' Fuente Movilidad CABA: Apple - Fuente Fallecimientos CABA: GCBA ',
                                    '</sup>')),titlefont=list(size=16))


p <- p %>% layout(legend = list(x = 0.1, y = 0.9))
p

scale_colour_manual(name = 'the colour', 
                    values =c('blue'='blue','red'='red'), labels = c('c2','c1'))

scale_color_manual(name="Groups",values=c("red", "blue"))+
  guides(colour = guide_legend(override.aes = list(linetype = 1)))

# Fin de gráfico

###############################################################################################



  