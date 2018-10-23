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
# hipcand_raw = read.csv("output/candidates_km_hipparcos.csv", colClasses="character") %>%
  # as_tibble()

# clean data --------------------------------------------------------------
tyc = tyc_raw %>% 
  # saca candidatas de clustering de hip que tmbn estan en tyc
  # dplyr::filter(!(HIP %in% hipcand_raw$id)) %>% 
  # identificador de hyades
  mutate(hyades = if_else(HIP %in% hdids_raw$id_hip, TRUE, FALSE)) %>% 
  # saca vbles irrelevantes
  select(-c(TYCID1,TYCID2,TYCID3,HD,HIP)) %>% 
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

### NOTA: no saca candidatas de hip porque dbscan encuentra un solo cluster


# determinacion de eps ----------------------------------------------------
# minPts=5 (default)
# plot
minpts_dist_tyc = dbscan::kNNdist(tyc_sc, k=5) %>% "["(,5) %>% sort()
gdat_knndist_tyc = tibble(index=1:nrow(tyc_sc), dist=minpts_dist_tyc)
g_knndist_tyc = ggplot(gdat_knndist_tyc) +
  geom_line(aes(x=index, y=dist), color="steelblue") +
  xlab("Points sorted by distance") +
  ylab("5-NN distance") +
  NULL
# punto de quiebre en 1.3 aprox (eps=1.3)

# dbscan ------------------------------------------------------------------
set.seed(semilla)
db_tyc = dbscan::dbscan(tyc_sc, eps=1.3, minPts=5)
# el cluster 0 es "noise"
db_tyc$cluster[db_tyc$cluster==0] = "noise"

# hd candidates ---------------------------------------------------------
# distribucion de hd en los clusters
tab_db_hd_tyc = table(db_tyc$cluster, tyc_id$hyades,
                        dnn=c("Cluster","Hyades"))
# xtable::xtable(tab_db_hd_hip) # para print lindo en el Rmd

# tira muchos clusters.. uno solo es enorme
# puede haber clusters con 3 objetos si minpts=5????


# silhouette plot ---------------------------------------------------------
# # sil object (noise pasa a ser 0 para que corra)
# db_hip$cluster[db_hip$cluster=="noise"] = "0"
# sil_hip_db = cluster::silhouette(db_hip$cluster, dist(hip_sc))
# # plot
# g_sil_hip_db = factoextra::fviz_silhouette(sil_hip_db, print.summary=F) +
#   labs(title=NULL)
# # resumen por cluster
# res_sil_hip_db = (summary(sil_hip_db) %$% cbind(dimnames(clus.sizes)[[1]],
#                             clus.sizes,
#                             clus.avg.widths)) %>% 
#   as_tibble() %>% setNames(c("cluster","size","avg.width"))

