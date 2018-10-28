# intro -------------------------------------------------------------------
gc()
source("libraries.R")
source("functions.R")
semilla = 1

# read data ---------------------------------------------------------------
# ## data generada con clean_hipparcos.R
# hipparcos (hip) catalogue
hip_raw = readRDS("data/working/hip_raw.RDS")
# hipparcos (hip) catalogue (sin NAs y con vble Hyades)
hip = readRDS("data/working/hip.RDS")
# hipparcos (hip) normalizado para clustering
hip_sc = readRDS("data/working/hip_sc.RDS")
# Ids y pertenencia a Hd
hip_id = readRDS("data/working/hip_id.RDS")

### IMPORTANTE ###
# con estandarizacion scale() queda un optimo claro en k=2
# y casi todas las hd quedan juntas
# en cambio con minmax queda k=4 poco claro y no todas las hd quedan juntas
# el tema es que soria en clase usó minmax

# k optimo para kmeans -------------------------------------------------------
set.seed(semilla)
# por default usa euclidean distance
# criterio silhouette
g_k_sil_hip = factoextra::fviz_nbclust(hip_sc,
                                   FUNcluster=kmeans,
                                   nstart=20,
                                   method="silhouette") +
  labs(title=NULL)
kopt_sil_hip = ggplot_build(g_k_sil_hip)$data[[3]]['xintercept'] %>% as.numeric()
# save
ggsave(plot=g_k_sil_hip, filename="output/plots/kopt_sil_km_hipparcos.png",
       width=5,height=2.5,dpi=100)
# criterio SCD
set.seed(semilla)
g_k_scd_hip = factoextra::fviz_nbclust(hip_sc,
                                       FUNcluster=kmeans,
                                       nstart=20,
                                       method="wss") +
  labs(title=NULL) +
  geom_vline(xintercept=kopt_sil_hip, linetype=2, color="steelblue")
# save
ggsave(plot=g_k_scd_hip, filename="output/plots/kopt_scd_km_hipparcos.png",
       width=5,height=2.5,dpi=100)


# kmeans con k optimo --------------------------------------------------------
set.seed(semilla)
# kmeans (euclidean, k=a ojo segun candidatos, 20 semillas para centroides iniciales)
km_hip = kmeans(hip_sc, centers=kopt_sil_hip
                      , nstart=20,iter.max=100
                      )
# save
saveRDS(km_hip, file="data/working/km_hipparcos.RDS")

# hd candidates ---------------------------------------------------------
# distribucion de hd en los clusters
tab_km_hd_hip = table(km_hip$cluster, hip_id$hyades,
                        dnn=c("Cluster","Hyades"))
# save
saveRDS(tab_km_hd_hip, file="output/tablehyades_km_hipparcos.RDS")

# las candidatas son del cluster donde estan la mayor parte de las hd
clus_hd_hip = tab_km_hd_hip %>% as_tibble() %>% 
  dplyr::filter(Hyades==T) %>% dplyr::filter(n==max(n)) %$% Cluster
id_candidates_hip = hip_id %>% bind_cols(cluster=km_hip$cluster) %>% 
  dplyr::filter(cluster %in% clus_hd_hip & hyades==F) %$% id
df_cand_hip = tibble(id=id_candidates_hip, catalog="hipparcos")
# write csv 
write.csv(df_cand_hip, "output/candidates_km_hipparcos.csv", row.names=F)

# hopkins -----------------------------------------------------------------
set.seed(semilla)
# tendencia al clustering con Hopkins
# valores al azar: 10% del dataset
hop_hip = clustertend::hopkins(hip_sc, n=0.1*nrow(hip_sc))$H
# alternativa: (el rdo es el mismo)
# hop_hip = factoextra::get_clust_tendency(hip_sc, n=0.1*nrow(hip_sc), 
                                         # seed=semilla, graph=F) %$% hopkins_stat
# save
saveRDS(hop_hip, "output/hopkins_hipparcos.RDS")

# silhouette plot ---------------------------------------------------------
# sil object
sil_km_hip = cluster::silhouette(km_hip$cluster, dist(hip_sc))
# plot
g_sil_km_hip = factoextra::fviz_silhouette(sil_km_hip, print.summary=F) +
  labs(title=NULL)
# save
ggsave(plot=g_sil_km_hip, filename="output/plots/sil_km_hipparcos.png")

# resumen por cluster
res_sil_hip = (summary(sil_km_hip) %$% cbind(dimnames(clus.sizes)[[1]],
                            clus.sizes,
                            clus.avg.widths)) %>% 
  as_tibble() %>% setNames(c("cluster","size","avg.width"))



# PCA plot ---------------------------------------------------------------
km_pca_hip = prcomp(hip_sc)
pca_hd_hip = km_pca_hip$x[hip_id$hyades,1:2]
g_pca_km_hip = factoextra::fviz_pca_ind(prcomp(hip_sc, scale=F), label="none",
                                        habillage=km_hip$cluster,
                                        title="",
                                        mean.point=F,
                                        labelsize=3) %>% 
  factoextra::fviz_add(pca_hd_hip, geom="point", color="black", addlabel=F)
# save
ggsave(plot=g_pca_km_hip, filename="output/plots/pca_km_hipparcos.png")


# save PMML ---------------------------------------------------------------
# exporta kmeans en lenguaje PMML como XML
km_hip_pmml = pmml::pmml(km_hip)
pmml::savePMML(km_hip_pmml, name="output/km_pmml_hipparcos_raw.xml",version=4.1)
# guarda hip_sc como csv para levantarlo en python
write.csv(hip_sc, file="data/working/hip_sc.csv", row.names=T)




