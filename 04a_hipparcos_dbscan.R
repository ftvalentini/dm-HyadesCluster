# intro -------------------------------------------------------------------
gc()
source("libraries.R")
source("functions.R")
semilla = 1

# read data ---------------------------------------------------------------
# hipparcos (hip) catalogue
hip_raw = readRDS("data/working/hip_raw.RDS")
# hipparcos (hip) catalogue (sin NAs y con vble Hyades)
hip = readRDS("data/working/hip.RDS")
# hipparcos (hip) normalizado para clustering
hip_sc = readRDS("data/working/hip_sc.RDS")
# Ids y pertenencia a Hd
hip_id = readRDS("data/working/hip_id.RDS")


# determinacion de eps ----------------------------------------------------
# minPts=5 (default)
# plot
minpts_dist_hip = dbscan::kNNdist(hip_sc, k=5) %>% "["(,5) %>% sort()
gdat_knndist_hip = tibble(index=1:nrow(hip_sc), dist=minpts_dist_hip)
g_knndist_hip = ggplot(gdat_knndist_hip) +
  geom_line(aes(x=index, y=dist), color="steelblue") +
  xlab("Points sorted by distance") +
  ylab("5-NN distance") +
  theme_minimal() +
  NULL
# save
ggsave("output/plots/eps_db_hip.png", g_knndist_hip,
       width=5,height=2.5,dpi=100)
# ver punto de quiebre con
# g_knndist_hip %>% plotly::ggplotly()


# dbscan ------------------------------------------------------------------
set.seed(semilla)
db_hip = dbscan::dbscan(hip_sc, eps=0.2, minPts=5)


# hd candidates ---------------------------------------------------------
# distribucion de hd en los clusters
tab_db_hd_hip = table(db_hip$cluster, hip_id$hyades,
                      dnn=c("Cluster","Hyades"))
# el cluster 0 es "noise"
# save
saveRDS(tab_db_hd_hip, file="output/tablehyades_dbscan_hipparcos.RDS")


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

