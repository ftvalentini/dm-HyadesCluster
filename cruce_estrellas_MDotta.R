library(readr)
hipparcos <- read_delim("C:/Users/miled/Desktop/Maestria DM/Ciencia y Tecnica/tp_estrellas/hipparcos.csv",";", escape_double = FALSE, trim_ws = TRUE)
hipparcos<-hipparcos[,1:8]

symbad <- read_delim("C:/Users/miled/Desktop/Maestria DM/Ciencia y Tecnica/tp_estrellas/symbad.csv",";", escape_double = FALSE, trim_ws = TRUE)

id_cruzada<- read_delim("C:/Users/miled/Desktop/Maestria DM/Ciencia y Tecnica/tp_estrellas/id_cruzada_Symbad_Hipparcos.csv",";", escape_double = FALSE, trim_ws = TRUE)

library(dplyr)
library(tidyr)

#hago el join de las esrellas que ya me aparecen con la ID cruzada ####

#primero separolas columnas para poder usar el numero de ID
#id_cruzada <- id_cruzada %>% separate(id_symbad,c("hd","id_symbad")," ")
id_cruzada <- id_cruzada %>% separate(id_hipparcos,c("hip","id_hipparcos")," ")

#creo una columna en el dataset de hipparcos para distinguir si es o no de HYADES
l1<-as.list(id_cruzada$id_hipparcos)
hipparcos$hyades<-c("no")
hipparcos$hyades[hipparcos$HIP %in% l1]<-"si"

#creo una columna en el dataset de symbad para ver si ya la detecté en hipparcos
l2<-as.list(id_cruzada$id_symbad)
symbad$detectada<-c("no")
symbad$detectada[symbad$identifier %in% l2]<-"si"
symbad<-symbad[complete.cases(symbad),]

#creo una tabla para comparar las cooredenadas entre las dos fuentes
cord_comp<-id_cruzada %>% select(-c(hip))

cord_comp<-left_join(cord_comp,symbad,by=c("id_symbad" = "identifier"))
cord_comp<-cord_comp %>% select(id_symbad,id_hipparcos,RA_J2000,DE_J2000) %>% rename(RA_sym = RA_J2000) %>% rename(DE_sym = DE_J2000)

cord_comp$id_hipparcos<-as.integer(cord_comp$id_hipparcos)
cord_comp<-left_join(cord_comp,hipparcos,by = c("id_hipparcos" = "HIP"))
cord_comp<-cord_comp %>% select(id_symbad,id_hipparcos,RA_sym,DE_sym,RA_J2000,DE_J2000) %>% rename(RA_hip = RA_J2000) %>% rename(DE_hip = DE_J2000)

cord_comp$RA_sym<-round(cord_comp$RA_sym,4)
cord_comp$RA_hip<-round(cord_comp$RA_hip,4)
cord_comp$DE_hip<-round(cord_comp$DE_hip,4)

cord_comp$dif_RA<-cord_comp$RA_sym - cord_comp$RA_hip
cord_comp$dif_DE<-cord_comp$DE_sym - cord_comp$DE_hip

#no hay diferencias entre las coordenadas de los dos datasets
#este camino no sirve

#analisis exploratorio de las coordenadas de las HYADES ####

#me fijo entre que valores se encuentran las coordenadas de estas estrellas
symbad$RA_J2000<-as.numeric(symbad$RA_J2000)

min_ra = min(symbad$RA_J2000)
max_ra = max(symbad$RA_J2000)
rango_ra = max_ra - min_ra
min_de = min(symbad$DE_J2000)
max_de = max(symbad$DE_J2000)
rango_de = max_de - min_de

#en este grafico super sencillo se ve que están bastante cerca
plot(symbad$RA_J2000,symbad$DE_J2000)

#repito el grafico para HIPPARCOS y coloreo las que son de Hyades
library(ggplot2)
ggplot(data=hipparcos,aes(x=RA_J2000,y=DE_J2000,fill=hyades,colour=hyades))+
  geom_point()
#en este grafico se ve que es bastante complicado ya que las hyades estan rodeadas de otras






