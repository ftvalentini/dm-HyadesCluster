#EXPLORATORIO
source("libraries.R")
source("functions.R")




#Exploratorio Hipparcos ####

hdids_raw = read.csv("output/hyades_ids.csv", colClasses="character") %>% 
  as_tibble()
# hipparcos (hip) catalogue
hip_raw <- readxl::read_excel("data/raw/hyades_source.xlsx", sheet="Hipparcos", 
                              col_types=c("text",rep("numeric",7))) %>% 
  as_tibble() 

#preparacion de los datos
hip = hip_raw %>% 
  # identificador de hyades
  mutate(hyades = if_else(HIP %in% hdids_raw$id_hip, TRUE, FALSE))
# Reemplazo NA en vble "B-V" por la mediana (son solo 15 registros con NA)
# mediana_bv <- median(hip$`B-V`, na.rm = TRUE)
# hip$`B-V`[is.na(hip$`B-V`)] <- mediana_bv

#General ####

hip_melt = hip %>% select(-c(HIP, hyades)) %>%
  tidyr::gather(., key=variable, value=value)

# tabla
df <- hip[,2:8]

tmp <- do.call(data.frame, 
               list(mean = apply(df, 2, mean),
                    sd = apply(df, 2, sd),
                    median = apply(df, 2, median),
                    min = apply(df, 2, min),
                    max = apply(df, 2, max),
                    n = apply(df, 2, length)))
sum_hip<-as.data.frame(tmp)

# boxplots
box_hip = ggplot(hip_melt, aes(x=variable, y=value, color=variable)) +
  geom_boxplot() +
  facet_wrap(~variable, scales="free") +
  xlab(NULL) +
  ylab(NULL) +
  NULL
ggsave("output/plots/box_hip.png", box_hip)

# density
dens_hip = ggplot(hip_melt, aes(x=value, fill=variable)) +
  geom_density(alpha=0.7, adjust=2) +
  facet_wrap(~variable, scales="free") +
  xlab(NULL) +
  ylab(NULL) +
  NULL
ggsave("output/plots/dens_hip.png", dens_hip, width=10, height=5, dpi=100)

# correlacion
library(corrplot)
M <- cor(hip[,2:8], use="pairwise")

png(filename="output/plots/cor_hip.png",  
    width=500, height=500)
cor_hip<-corrplot.mixed(M)

dev.off()

# ggpairs(hip[,2:9], mapping = aes(colour = hyades))

# factoextra::fviz_pca_biplot(prcomp(hip[,2:8]), label="var")

#Hyades vs. resto ####

hip_melt2 = hip %>% select(-c(HIP)) %>%
  tidyr::gather(., key=variable, value=value, 1:7)

hip_melt2$hyades[hip_melt2$hyades==TRUE]<-"hyades"
hip_melt2$hyades[hip_melt2$hyades==FALSE]<-"no_hyades"

# boxplots
box_hip2 = ggplot(hip_melt2, aes(x=variable, y=value, color=hyades)) +
  geom_boxplot() +
  facet_wrap(~variable, scales="free") +
  xlab(NULL) +
  ylab(NULL) +
  NULL
ggsave("output/plots/box_hip2.png", box_hip2)

# density
dens_hip2 = ggplot(hip_melt2, aes(x=value, fill=hyades, color=hyades)) +
  geom_density(alpha=0.7, adjust=2) +
  facet_wrap(~variable, scales="free") +
  xlab(NULL) +
  ylab(NULL) +
  NULL
ggsave("output/plots/dens_hip2.png", dens_hip2, width=10, height=5, dpi=100)

#Exploratorio Tycho ####

tyc_raw <- readxl::read_excel("data/raw/hyades_source.xlsx", sheet="Tycho", 
                              col_types=c(rep("text",4),
                                          rep("numeric",8),
                                          rep("text",2),
                                          rep("numeric",1))) %>% as_tibble() 

# clean data 
tyc = tyc_raw %>% 
  # identificador de hyades
  mutate(hyades = if_else(HIP %in% hdids_raw$id_hip, TRUE, FALSE)) %>% 
  # saca vbles irrelevantes
  select(-c(TYCID1,TYCID2,TYCID3,HD,HIP,BT,VT)) 
# %>% 
  # saca vlbes con muchos faltantes (Plx - 86.1%)
  # select_if(~mean(is.na(.x))<0.75) 

#hacer la aclaración de que la parte de NAs la vamos a ver más adelante !!

#General ####

tyc_melt = tyc %>% select(-c(recno, hyades)) %>%
  tidyr::gather(., key=variable, value=value)

# tabla
df2 <- tyc[,2:7]

tmp2 <- do.call(data.frame, 
               list(mean = apply(df2, 2, mean),
                    sd = apply(df2, 2, sd),
                    median = apply(df2, 2, median),
                    min = apply(df2, 2, min),
                    max = apply(df2, 2, max),
                    n = apply(df2, 2, length)))
sum_tyc<-as.data.frame(tmp2)

# boxplots
box_tyc = ggplot(tyc_melt, aes(x=variable, y=value, color=variable)) +
  geom_boxplot() +
  facet_wrap(~variable, scales="free") +
  xlab(NULL) +
  ylab(NULL) +
  NULL
ggsave("output/plots/box_tyc.png", box_tyc)

# density
dens_tyc = ggplot(tyc_melt, aes(x=value, fill=variable)) +
  geom_density(alpha=0.7, adjust=2) +
  facet_wrap(~variable, scales="free") +
  xlab(NULL) +
  ylab(NULL) +
  NULL
ggsave("output/plots/dens_tyc.png", dens_tyc, width=10, height=5, dpi=100)

# correlacion
M2 <- cor(tyc[,2:7])
cor_tyc<-corrplot.mixed(M2)

png(filename="output/plots/cor_tyc.png",
    width=500, height=500)
cor_tyc<-corrplot.mixed(M2)
dev.off()

#Hyades vs. resto ####

tyc_melt2 = tyc %>% select(-c(recno)) %>%
  tidyr::gather(., key=variable, value=value, 1:6)

tyc_melt2$hyades[tyc_melt2$hyades==TRUE]<-"hyades"
tyc_melt2$hyades[tyc_melt2$hyades==FALSE]<-"no_hyades"

# boxplots
box_tyc2 = ggplot(tyc_melt2, aes(x=variable, y=value, color=hyades)) +
  geom_boxplot() +
  facet_wrap(~variable, scales="free") +
  xlab(NULL) +
  ylab(NULL) +
  NULL
ggsave("output/plots/box_tyc2.png", box_tyc2)

# density
dens_tyc2 = ggplot(tyc_melt2, aes(x=value, fill=hyades, color=hyades)) +
  geom_density(alpha=0.7, adjust=2) +
  facet_wrap(~variable, scales="free") +
  xlab(NULL) +
  ylab(NULL) +
  NULL
ggsave("output/plots/dens_tyc2.png", dens_tyc2, width=10, height=5, dpi=100)










