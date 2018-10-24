# intro -------------------------------------------------------------------
gc()
source("libraries.R")
source("functions.R")
semilla = 1

# read data ---------------------------------------------------------------
# ids de Hyades en hipparcos
hdids_raw = read.csv("output/hyades_ids.csv", colClasses="character") %>% as_tibble()
# Tychos (tyc) catalogue
tyc_raw <- readxl::read_excel("data/raw/hyades_source.xlsx", sheet="Tycho", 
                              col_types=c(rep("text",4),
                                          rep("numeric",8),
                                          rep("text",2),
                                          rep("numeric",1))) %>% as_tibble() 
# ids de candidatas identificadas con clustering en hipparcos
hipcand_raw = read.csv("output/candidates_fa_hipparcos.csv", colClasses="character") %>%
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
# base para hacer clustering
tyc_sc = tyc %>% 
  # saca ID (recno) y identificador de hd
  select(-c(recno, hyades)) %>% 
  # transforma con minmax/scale
  # map_dfr(minmax) %>% 
  scale() %>% 
  as.data.frame() %>% 
  # id (recno) como rownames para no perder en id en clustering
  set_rownames(tyc$recno)
# base con ID y pertenencia a hd
tyc_id = tyc %>% select(recno, hyades) %>% rename("id"="recno")


# k optimo -------------------------------------------------------
# set.seed(semilla)
# # por default usa euclidean distance
# # criterio silhouette
# g_k_sil_fa_tyc = factoextra::fviz_nbclust(tyc_sc,
#                                        FUNcluster=cluster::fanny, memb.exp=1.35,
#                                        method="silhouette") +
#   labs(title=NULL)
# kopt_sil_fa_tyc = ggplot_build(g_k_sil_tyc)$data[[3]]['xintercept'] %>% as.numeric()
# # criterio SCD
# set.seed(semilla)
# g_k_scd_fa_tyc = factoextra::fviz_nbclust(tyc_sc,
#                                        FUNcluster=cluster::fanny, memb.exp=1.35,
#                                        method="wss") +
#   labs(title=NULL) +
#   geom_vline(xintercept=kopt_sil_tyc, linetype=2, color="steelblue")

### NO HAY CONVERGENCIA CON K>2

# fuzzy clustering  --------------------------------------------------------
set.seed(semilla)
# k=k optimo de kmeans
# fanny (euclidean dist)
fa_tyc = cluster::fanny(tyc_sc, k=3, memb.exp=1.35)

### QUE ES EL PARAMETRO memb.exp ???
### no hay convergencia con k=2

# max prob de cada objeto
fa_maxprob_tyc = fa_tyc$membership %>% apply(1, max) %>% tibble(prob=.,
                                                                id=rownames(tyc_sc))
# histograma de maxprob
ggplot(fa_maxprob_tyc) +
  geom_histogram(aes(x=prob))
# proporcion de maxprob>=0.6
fa_maxprob_tyc %>% dplyr::filter(prob>=0.6) %>% nrow %>% "/"(nrow(tyc_sc)) %>% "*"(100)

### 67.6% TIENEN PROB DE PERTENECER MAYOR A 0.6
# las de prob<0.6 son "noise"
fa_tyc$clustering[fa_maxprob_tyc$prob<0.6] = "noise"

# hd candidates ---------------------------------------------------------
# distribucion de hd en los clusters
tab_fu_hd_tyc = table(fa_tyc$clustering, tyc_id$hyades,
                      dnn=c("Cluster","Hyades"))
# xtable::xtable(tab_clus_hd_tyc) # para print lindo en el Rmd


# las candidatas son del cluster donde estan la mayor parte de las hd
# clus_hd_fa_tyc = tab_clus_hd_tyc %>% as_tibble() %>% 
#   dplyr::filter(Hyades==T) %>% dplyr::filter(n==max(n)) %$% Cluster
# id_candidates_fa_tyc = tyc_id %>% bind_cols(cluster=km_tyc$cluster) %>% 
#   dplyr::filter(cluster %in% clus_hd_tyc & hyades==F) %$% id
# df_cand_fa_tyc = tibble(id=id_candidates_tyc, catalog="hipparcos")
# # write csv 
# write.csv(df_cand_fa_tyc, "output/candidates_fa_tycparcos.csv", row.names=F)

# silhouette plot ---------------------------------------------------------
# sil object (sin contar los noise)
sil_fa_tyc = cluster::silhouette(
  as.numeric(fa_tyc$cluster[!fa_tyc$clustering=="noise"]), 
  dist(tyc_sc[!fa_tyc$clustering=="noise",])
  )
# plot
g_sil_fa_tyc = factoextra::fviz_silhouette(sil_fa_tyc, print.summary=F) +
  labs(title=NULL)
# resumen por cluster
res_sil_fa_tyc = (summary(sil_fa_tyc) %$% cbind(dimnames(clus.sizes)[[1]],
                                                clus.sizes,
                                                clus.avg.widths)) %>% 
  as_tibble() %>% setNames(c("cluster","size","avg.width"))

