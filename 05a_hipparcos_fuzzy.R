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

# k optimo -------------------------------------------------------
# se usa el que sale de kmeans
km_hip = readRDS(file="data/working/km_hipparcos.RDS")
kopt_fa_hip = nrow(km_hip$centers)

# fuzzy clustering  --------------------------------------------------------
set.seed(semilla)
# fanny (euclidean dist)
fa_hip = cluster::fanny(hip_sc, k=kopt_fa_hip, memb.exp=1.2)


# probabilities -----------------------------------------------------------
# max prob de cada objeto
fa_maxprob_hip = fa_hip$membership %>% apply(1, max) %>% tibble(prob=.,
                                                            id=rownames(hip_sc))
# histograma de maxprob
g_fa_hist_hip = ggplot(fa_maxprob_hip) +
  geom_histogram(aes(x=prob)) +
  xlab("Máxima probabilidad de pertenencia") +
  ylab("Frecuencia") + 
  theme_minimal() +
  NULL
# save
ggsave("output/plots/prob_hist_fuzzy_hipparcos.png", g_fa_hist_hip)

# hd candidates ---------------------------------------------------------
# estrellas con maxprob <0.6 son asignadas a cluster "0"
fa_hip$clustering[fa_maxprob_hip$prob<0.6] = 0
# distribucion de hd en los clusters
tab_fa_hd_hip = table(fa_hip$cluster, hip_id$hyades,
                        dnn=c("Cluster","Hyades"))
# save
saveRDS(tab_fa_hd_hip, file="output/tablehyades_fuzzy_hipparcos.RDS")

# las candidatas son del cluster donde estan la mayor parte de las hd
clus_hd_fa_hip = tab_fa_hd_hip %>% as_tibble() %>%
  dplyr::filter(Hyades==T) %>% dplyr::filter(n==max(n)) %$% Cluster
id_candidates_fa_hip = hip_id %>% bind_cols(cluster=km_hip$cluster) %>%
  dplyr::filter(cluster %in% clus_hd_hip & hyades==F) %$% id
df_cand_fa_hip = tibble(id=id_candidates_hip, catalog="hipparcos")
# write csv
write.csv(df_cand_fa_hip, "output/candidates_fa_hipparcos.csv", row.names=F)

# PCA plot ---------------------------------------------------------------
fa_pca_hip = prcomp(hip_sc)
pca_hd_hip = fa_pca_hip$x[hip_id$hyades,1:2]
g_pca_fa_hip = factoextra::fviz_pca_ind(prcomp(hip_sc, scale=T), label="none",
                                        habillage=fa_hip$cluster,
                                        title="",
                                        mean.point=F,
                                        labelsize=3) %>% 
  factoextra::fviz_add(pca_hd_hip, geom="point", color="black", addlabel=F)
# save
ggsave(plot=g_pca_fa_hip, filename="output/plots/pca_fuzzy_hipparcos.png")




# silhouette plot ---------------------------------------------------------

# # sil object
# sil_fa_hip = cluster::silhouette(fa_hip$cluster, dist(hip_sc))
# # plot
# g_sil_fa_hip = factoextra::fviz_silhouette(sil_fa_hip, print.summary=F) +
#   labs(title=NULL)
# # resumen por cluster
# res_sil_fa_hip = (summary(sil_fa_hip) %$% cbind(dimnames(clus.sizes)[[1]],
#                                                 clus.sizes,
#                                                 clus.avg.widths)) %>% 
#   as_tibble() %>% setNames(c("cluster","size","avg.width"))

