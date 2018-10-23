library(RANN)
library(dplyr)
library(cluster)
library(factoextra)

hip_raw <- readxl::read_excel("dm-HyadesCluster/data/raw/hyades_source.xlsx", sheet="Hipparcos", 
                              col_types=c("text", rep("guess", 7)))
# Symbad catalogue (only Hyades (hd))
sym_raw <- readxl::read_excel("dm-HyadesCluster/data/raw/hyades_source.xlsx", sheet="Symbad")
# ids of hd within hip
idcruz_raw <- readxl::read_excel("dm-HyadesCluster/data/raw/id_cruzada_Symbad_Hipparcos.xlsx", col_names=F) 

#HIPPARCOS
#reemplazo los registros faltantes con la mediana de la variable(porque son solo 15 registros a los que les falta una vairable)
mediana_bv<-median(hip_raw$`B-V`,na.rm = TRUE)

hip_raw_comp1<-hip_raw
hip_raw_comp1$`B-V`[is.na(hip_raw_comp$`B-V`)] <- mediana_bv

#tambien está la alternativa de remover las NAs

#nombro las filas
hip_raw_comp<-hip_raw_comp1
hip_raw_comp<-hip_raw_comp[,2:8]
rownames(hip_raw_comp)<-hip_raw_comp1$HIP

#Normalizo las variables

#estandarizacion

#OPCION 1, USAR LA FUNCION SCALE
hip_norm <- hip_raw_comp %>% scale

#k optimo segun SCD y silhouette
factoextra::fviz_nbclust(dat_cs,kmeans,method="wss") #k optimo en 10
factoextra::fviz_nbclust(dat_cs,kmeans,method="silhouette") #h optimo en 8

# OPCION 2, usar la funcion que armo Soria

# Escalamos todos los datos entre 0 y 1 para simplificar los pasos que siguen
esc01 <- function(x) { (x - min(x)) / (max(x) - min(x))}
hip_norm<-hip_raw_comp
hip_norm <- apply(hip_raw_comp, 2, esc01)

#k optimo segun SCD y silhouette
factoextra::fviz_nbclust(hip_norm,kmeans,method="wss") #k optimo en 10
factoextra::fviz_nbclust(hip_norm,kmeans,method="silhouette") #h optimo en 4

##
dat.nrm.dist <- dist(hip_norm)
dat.clus <- hclust(dat.nrm.dist)

#MEDICION DE LA TENDENCIA AL CLUSTERING ####
#Usamos el estadístico de Hopkins de tendencia al clustering

#Creamos 50 puntos al azar en el espacio de muestreo:
cant.muestras <- 50
rnd.pts <- cbind(runif(cant.muestras, 0, 1),
                 runif(cant.muestras, 0, 1),
                 runif(cant.muestras, 0, 1),
                 runif(cant.muestras, 0, 1),
                 runif(cant.muestras, 0, 1),
                 runif(cant.muestras, 0, 1),
                 runif(cant.muestras, 0, 1))

#Y ahora seleccionamos al azar 50 índices del dataset
smp <- sample(nrow(hip_norm), 50)
smp.dat <- hip_norm[smp,]

#Vemos como se ve la data en un dendograma
plot( as.dendrogram( dat.clus ), leaflab="none", main="dat.nrm")


#Calculamos la tendencia al clustering de los datos:

# cálculo de las distancias al vecino más cercano para los datos reales
smp.pts.dist <- nn2(as.data.frame(hip_norm), as.data.frame(smp.dat),
                    k=2)$nn.dist[,2]
# cálculo de las distancias al vecino más cercano para datos al azar
rnd.pts.dist <- nn2(as.data.frame(hip_norm), as.data.frame(rnd.pts),
                    k=1)$nn.dists
# calculo del estadístico
sum(smp.pts.dist) / (sum(smp.pts.dist) + sum(rnd.pts.dist))
#Da 0.09637409 (si removemos las observaciones que tienen NAs da 0.09460663 )

#Armo cluster usando k-means ####
dat.kmeans <- kmeans(hip_norm, centers=4)

#VALIDACION INTERNA: SILHOUETTE
dat.kmeans.sil <- silhouette(dat.kmeans$cluster, dat.nrm.dist)
summary(dat.kmeans.sil)

summary(dat.kmeans.sil)$avg.width #0.2877176
summary(dat.kmeans.sil)$clus.avg.widths
# salida grafica
plot(dat.kmeans.sil, col=1:4, border = NA)



