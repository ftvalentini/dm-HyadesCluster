
# intro -------------------------------------------------------------------
gc()

source("libraries.R")
source("functions.R")

semilla = 1

# read data ---------------------------------------------------------------
# ids de Hyades en hipparcos
hdids_raw = read.csv("output/hyades_ids.csv", colClasses="character") %>% 
  as_tibble()

# Tychos (tyc) catalogue
tyc_raw <- readxl::read_excel("data/raw/hyades_source.xlsx", sheet="Tycho", 
                              col_types=c(rep("text",4),
                                          rep("numeric",8),
                                          rep("text",2),
                                          rep("numeric",1))) %>% 
  as_tibble() 

# ids de candidatas identificadas con clustering en hipparcos
hipcand_raw = read.csv("output/candidates_km_hipparcos.csv", colClasses="character") %>%
  as_tibble()


# clean data --------------------------------------------------------------

tyc = tyc_raw %>% 
  # saca candidatas de clustering de hip que tmbn estan en tyc
  dplyr::filter(!(HIP %in% hipcand_raw$id)) %>% 
  # identificador de hyades
  mutate(hyades = if_else(HIP %in% hdids_raw$id_hip, TRUE, FALSE)) %>% 
  # saca vbles irrelevantes
  select(-c(TYCID1,TYCID2,TYCID3,HD,HIP,VT,BT)) %>% 
  # saca vlbes con muchos faltantes (Plx - 86.1%)
  select_if(~mean(is.na(.x))<0.75) 

# version con todas las estrellas, solo para calcular Hopkins
tyc_hopkins = tyc_raw %>% 
  # saca vbles irrelevantes e ID
  select(-c(TYCID1,TYCID2,TYCID3,HD,HIP,recno,VT,BT)) %>% 
  # saca vlbes con muchos faltantes (Plx - 86.1%)
  select_if(~mean(is.na(.x))<0.75) %>% 
  scale() %>% 
  as.data.frame() %>% 
  # id (recno) como rownames para no perder en id en clustering
  set_rownames(tyc_raw$recno)

## observaciones:
# hay una sola de las 50 hd que no esta en tyc
tyc$hyades %>% table
# alto porcentaje de faltantes en Plx
# tycx %>% map_dbl(~mean(is.na(.x)))

# base para hacer clustering
tyc_sc = tyc %>% 
  # saca ID (recno) y identificador de hd
  select(-c(recno, hyades)) %>% 
  # transforma con minmax/scale
  map_dfr(minmax) %>%
  # scale() %>% 
  as.data.frame() %>% 
  # id (recno) como rownames para no perder en id en clustering
  set_rownames(tyc$recno)

# base con ID y pertenencia a hd
tyc_id = tyc %>% select(recno, hyades) %>% rename("id"="recno")

# k optimo para kmeans -------------------------------------------------------
set.seed(semilla)
# por default usa euclidean distance
# criterio silhouette
gc()
g_k_sil_tyc = factoextra::fviz_nbclust(k.max=15,tyc_sc,FUNcluster=kmeans,nstart=20,algorithm="Lloyd",
                                   method="silhouette") +
  labs(title=NULL)
kopt_sil_tyc = ggplot_build(g_k_sil_tyc)$data[[3]]['xintercept'] %>% as.numeric()
# save
ggsave(plot=g_k_sil_tyc, filename="output/plots/kopt_sil_km_tycho.png",
       width=5,height=2.5,dpi=100)
# criterio SCD (a mano porque factoextra no corre)
set.seed(semilla)
scd_tyc = tibble(k=2:15) %>% 
  mutate(scd = map_dbl(k, function(x) 
    kmeans(tyc_sc, centers=x, nstart=20, algorithm="Lloyd") %$% tot.withinss))
g_k_scd_tyc = ggplot(scd_tyc, aes(x=k, y=scd)) +
  geom_line(color = "steelblue") +
  geom_point(color = "steelblue") +
  geom_vline(xintercept=kopt_sil_tyc, linetype=2, color="steelblue") +
  xlab("Number of clusters k") +
  ylab("Total Within Sums of Squares") +
  theme_minimal() +
  NULL
# save
ggsave(plot=g_k_scd_tyc, filename="output/plots/kopt_scd_km_tycho.png",
       width=5,height=2.5,dpi=100)

# kmeans con k optimo --------------------------------------------------------
set.seed(semilla)
# kmeans (euclidean, k=a ojo segun candidatos, 20 semillas para centroides iniciales)
gc()
km_tyc = kmeans(tyc_sc, centers=15, nstart=20, algorithm="Lloyd", iter.max=100)
saveRDS(km_tyc, file="data/working/km_tyc.RDS")

# identificacion de candidatas --------------------------------------------
# distribucion de hd en los clusters
tab_km_hd_tyc = table(km_tyc$cluster, tyc_id$hyades,
                      dnn=c("Cluster","Hyades"))
# save
saveRDS(tab_km_hd_tyc, file="output/tablehyades_km_tycho.RDS")

# las candidatas son del cluster donde estan la mayor parte de las hd
clus_hd = tab_km_hd_tyc %>% as_tibble() %>% 
  dplyr::filter(Hyades==T) %>% dplyr::filter(n==max(n)) %$% Cluster
id_candidates = tyc_id %>% bind_cols(cluster=km_tyc$cluster) %>% 
  dplyr::filter(cluster %in% clus_hd & hyades==F) %$% id
# dataframe con id de candidatas en tycho y en hipparcos (si existe)
tyc_cand = tyc_raw %>% dplyr::filter(recno %in% id_candidates) %>% select(recno,HIP) %>% 
  rename(id_tycho=recno, id_hipparcos=HIP) %>% 
  mutate(catalog="tycho")
# dataframe con id de candidatas de hipparcos y en tycho (si existe)
hip_cand = hipcand_raw %>% rename(id_hipparcos = id) %>% 
  left_join(tyc_raw, by=c("id_hipparcos"="HIP")) %>% 
  select(catalog, id_hipparcos, recno) %>% 
  rename(id_tycho=recno)

# lista final de candidatos
candidates_full = bind_rows(hip_cand, tyc_cand)
# save
write.csv(candidates_full, "output/candidates_full_km.csv", row.names=F)


# hopkins -----------------------------------------------------------------
set.seed(semilla)
# tendencia al clustering con Hopkins (DATASET NORMALIZADO Y CON TODAS LAS ESTRELLAS)
# valores al azar: 10% del dataset
hop_tyc = clustertend::hopkins(tyc_hopkins, n=0.1*nrow(tyc_sc))$H
# alternativa con igual rdo:
# hop_tyc = factoextra::get_clust_tendency(tyc_sc, n=0.1*nrow(tyc_sc), 
                                         # seed=semilla, graph=F) %$% hopkins_stat
# save
saveRDS(hop_tyc, "output/hopkins_tycho.RDS")

# silhouette plot ---------------------------------------------------------
# sil object
sil_km_tyc = cluster::silhouette(km_tyc$cluster, dist(tyc_sc))
# plot
g_sil_km_tyc = factoextra::fviz_silhouette(sil_km_tyc, print.summary=F) +
  labs(title=NULL)
# save
ggsave(plot=g_sil_km_tyc, filename="output/plots/sil_km_tycho.png")

# resumen por cluster
res_sil_tyc = (summary(sil_km_tyc) %$% cbind(dimnames(clus.sizes)[[1]],
                                          clus.sizes,
                                          clus.avg.widths)) %>% 
  as_tibble() %>% setNames(c("cluster","size","avg.width"))

# PCA plot ---------------------------------------------------------------
km_pca_tyc = prcomp(tyc_sc)
pca_hd_tyc = km_pca_tyc$x[tyc_id$hyades,1:2]
g_pca_km_tyc = factoextra::fviz_pca_ind(prcomp(tyc_sc, scale=T), label="none",
                                        habillage=km_tyc$cluster,
                                        title="",
                                        mean.point=F,
                                        labelsize=3) %>% 
  factoextra::fviz_add(pca_hd_tyc, geom="point", color="black", addlabel=F)
# save
ggsave(plot=g_pca_km_tyc, filename="output/plots/pca_km_tycho.png")