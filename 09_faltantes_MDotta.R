#Análisis de faltantes en Tycho
source("libraries.R")
source("functions.R")

#preparo datos ####
# ids de Hyades en hipparcos
hdids_raw = read.csv("output/hyades_ids.csv", colClasses="character") %>%
  as_tibble()
# Tychos (tyc) catalogue
tyc_raw <- readxl::read_excel("data/raw/hyades_source.xlsx", sheet="Tycho", 
                              col_types=c(rep("text",4),
                                          rep("numeric",8),
                                          rep("text",2),
                                          rep("numeric",1))) %>% as_tibble() 

tyc = tyc_raw %>% 
  # identificador de hyades
  mutate(hyades = if_else(HIP %in% hdids_raw$id_hip, TRUE, FALSE)) %>% 
  # saca vbles irrelevantes
  select(-c(TYCID1,TYCID2,TYCID3,HD,HIP,pmRA,pmDE)) 

#porcentaje de NAs
nas<-round((sum(is.na(tyc$Plx))/nrow(tyc)),2)

#llevo los NAs a binario
tyc_na<-tyc
tyc_na$na<-c("no")
tyc_na$na[is.na(tyc$Plx) == TRUE ]<-"si"

#borro cosas que no voy a usar a los fines de este estudio
tyc_na <- tyc_na %>% select(-c(recno,Plx,hyades))

#análsis gráfico ####

na_melt = tyc_na  %>%
  tidyr::gather(., key=variable, value=value, 1:6)

# boxplots
box_na = ggplot(na_melt, aes(x=variable, y=value, color=na)) +
  geom_boxplot() +
  facet_wrap(~variable, scales="free") +
  xlab(NULL) +
  ylab(NULL) +
  NULL
ggsave("output/plots/box_na.png", box_na)
#no parece haber gran diferencia

# density
dens_na = ggplot(na_melt, aes(x=value, fill=na, color=na)) +
  geom_density(alpha=0.7, adjust=2) +
  facet_wrap(~variable, scales="free") +
  xlab(NULL) +
  ylab(NULL) +
  NULL
ggsave("output/plots/dens_na.png", dens_na)
#V y VT parecerían mostrar alguna diferencia



#testeo estadistico ####

#test de diferencia de medias
pvalor <- c()
names(tyc_na)
tyc_na <- as.data.frame(tyc_na)
for(i in 1:6) {
  a <- wilcox.test(tyc_na[,i]~tyc_na[,"na"])
  pvalor <- c(pvalor, a$p.value)
}

round(pvalor,2)

#Todas las diferencias de medias son significativas excepto en "DE_J2000", lo que se ve?a en el boxplot
#Se puede intuir que hay una relaci?n entre los valores las variables y la ausencia de valores?



library(BaylorEdPsych)
library(mvnmle)
tyc_raw2 <- as.matrix(sapply(tyc, as.numeric))  

cor(tyc_raw2[,2:7])

colnames(tyc_raw2)

mcar <- BaylorEdPsych::LittleMCAR(tyc_raw2[,c(2:5,8)])

mcar$chi.square
mcar$p.value
#Este test en teoria te dice si es MCAR o no (HO: es MCAR, mira las covarianzas de los NA y no NA)




# Pruebo correr algún modelo a ver si las vairables pueden explicar la variable a predecir ####
library(randomForest)
library(caret)
library(pROC)
tyc_rf <- tyc 

tyc_rf$na<-c("no")
tyc_rf$na[is.na(tyc$Plx) == TRUE ]<-"si"

tyc_rf = tyc_rf %>% select(-c(Plx,hyades))
colnames(tyc_rf)<-c("recno","RA_J2000_24","DE_J2000","BT","VT","V","BV","na")

tyc_rf$na<-as.factor(tyc_rf$na)

smp_size<-floor(0.8*nrow(tyc_rf))
train_ind<-sample(seq_len(nrow(tyc_rf)),size=smp_size)
train<-tyc_rf[train_ind,]
test<-tyc_rf[-train_ind,]


rf = randomForest(na ~ ., ntree = 500,mtry = 6, data = train[,-1])



predicho<- predict(rf, test[,-c(1,8)])
predictdf<- cbind(test[,c('recno','na')],predicho)
predictdf <- as.data.frame(predictdf)
testdf<- predictdf
names(testdf)<-c('recno','na','rf')
predictdf[,2]<- as.factor(predictdf[,2])
predictdf[,3]<- as.factor(predictdf[,3])
cm<- confusionMatrix(predictdf[,2],predictdf[,3])
acc<- cm$overall['Accuracy']
predictdf[,2]<- as.numeric(as.factor(predictdf[,2]))
predictdf[,3]<- as.numeric(as.factor(predictdf[,3]))
aucmulti<-pROC::multiclass.roc(predictdf[,2], predictdf[,3], levels = c(1,2))
aucmulti<-aucmulti$auc

#accuracy del 0.9197 y Multi-class area under the curve: 0.796
#las variables pueden explicar la presencia de faltantes con bastante certeza



#Hacemos imputaciones a través de una regresión lineal
ajuste <- lm(Plx ~ .,data=tyc[,c(2:4,8)])
summary(ajuste)

lm.pred <- predict(ajuste, newdata = tyc)

#Llevamos a DF a los predichos vs los valores originales
library(tibble)
ajuste <- as.tibble(base::cbind(lm.pred[!is.na(tyc$Plx)],tyc$Plx[!is.na(tyc$Plx)]))
colnames(ajuste) <- c("Predicted", "Original")

library(reshape2)
ajuste <- melt(ajuste)

#Comparamos distribuciones. Hay una clara diferencia, regresión lineal no parece ser buen método
modelo_lineal<-ggplot(ajuste, aes(x=variable, y=value)) +
  geom_boxplot()

ggsave("output/plots/modelo_lineal.png", modelo_lineal)

library(mice)
tyc.mice = tyc_raw %>% 
  # saca vbles irrelevantes
  select(-c(TYCID1,TYCID2,TYCID3,HD,HIP,pmRA,pmDE)) 

colnames(tyc.mice) <- c(names(tyc.mice[1:6]),"BV","Plx")

#Corremos imputaciones a través del paquete mice
mice.imp <-mice(tyc.mice[,c(2:4,6,8)],m=5,maxit=50,meth='pmm',seed=500) #tarda un ratito

summary(mice.imp)

#Vemos gráficamente si los datos imputados tienen una distribución similar a los originales
#Siguen siendo bastante malos
densityplot(mice.imp) #Magenta datos imputados, azul originales
stripplot(mice.imp)

mice::complete(mice.imp,1)
