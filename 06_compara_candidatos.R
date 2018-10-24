# intro -------------------------------------------------------------------
gc()
source("libraries.R")
source("functions.R")
semilla = 1

# read data ---------------------------------------------------------------
# candidatos obtenidos por kmedias de ambos catalogos
candidates = read.csv("output/candidates_full_km.csv", colClasses="character") %>% 
  as_tibble()
# data de hip
hip_raw = readRDS("data/working/hip_raw.RDS")
# data de tyc
tyc_raw = readRDS("data/working/tyc_raw.RDS")


# clean and merge data ----------------------------------------------------
# deja solo ids y variables en comun entre ambos catalogos
# Vmag=V
common_vars = c("V","Vmag","RA_J2000","RA_J2000_24","DE_J2000",
                "Plx","pmRA","pmDE","B-V")
# clean hip
hip = hip_raw %>% "["(names(.) %in% c(common_vars,"HIP"))
# clean tyc
tyc = tyc_raw %>% "["(names(.) %in% c(common_vars,"recno")) %>% 
  rename(RA_J2000=RA_J2000_24, Vmag=V)
# candidates hip with data
hip_cand = candidates %>% dplyr::filter(catalog=="hipparcos") %>% 
  left_join(hip, by=c("id_hipparcos"="HIP"))
# candidates tyc with data
tyc_cand = candidates %>% dplyr::filter(catalog=="tycho") %>% 
  left_join(tyc, by=c("id_tycho"="recno"))
# all candidates
full_cand = bind_rows(hip_cand, tyc_cand)
# melt para ggplot
gdat = full_cand %>% select(-c(id_hipparcos, id_tycho)) %>%
  tidyr::gather(-catalog, key=variable, value=value)



# density -----------------------------------------------------------------
g_dens = ggplot(gdat, aes(x=value, fill=catalog)) +
  geom_density(alpha=0.7, adjust=2) +
  facet_wrap(~variable, scales="free") +
  xlab(NULL) +
  ylab(NULL) +
  NULL
# save
ggsave(filename="output/plots/compared_density_candidates.png",
       g_dens, width=10,height=5,dpi=100)


# scatter -----------------------------------------------------------------
# (por ahora solo RA y DE)
g_scat_cand = ggplot(full_cand %>% select(-c(id_hipparcos, id_tycho))) + 
  geom_point(aes(x=RA_J2000, y=DE_J2000, color=catalog),
             size=1.25, position="jitter")
ggsave(filename="output/plots/scatter_cand_1.png", g_scat_cand,
       width=10,height=5,dpi=100)


# # boxplots
# g_box = ggplot(gdat, aes(x=catalog, y=value, color=catalog)) +
#   geom_boxplot() +
#   facet_wrap(~variable, scales="free") +
#   # EnvStats::stat_n_text(size=3) +
#   ## si lo pongo deja de funcionar free scales 
#   xlab(NULL) +
#   ylab(NULL) +
#   NULL

# tables ------------------------------------------------------------------

# res_tab = gdat %>% group_by(variable, catalog) %>% 
#   summarise_all(funs(N=sum(!is.na(.)),
#                      Min=min(., na.rm=T),
#                      Q1=quantile(., probs=0.25, na.rm=T),
#                      Median=quantile(., probs=0.5, na.rm=T),
#                      Q3=quantile(., probs=0.75, na.rm=T),
#                      Max=max(., na.rm=T),
#                      MAD=mad(., na.rm=T)
#   )) %>% ungroup()

### para el print en pdf:
# res_tab_t = t(res_tab %>% select(-c(variable, catalog))) %>% as.data.frame() %>% 
#   tibble::rownames_to_column()
# res_tab_t %>% 
#   kable("latex", booktabs=T,
#         col.names=c("", rep(c("hip.","tyc."),7))) %>% 
#   kableExtra::add_header_above(c(" ","B-V"=2,"DE_J2000"=2,"Plx"=2,"pmDE"=2,
#                                  "pmRA"=2,"RA_J2000"=2,"Vmag"=2))



# old ---------------------------------------------------------------------

# 
# # (por ahora solo RA y DE)
# pairs_cand = GGally::ggpairs(full_cand %>% select(-c(id_hipparcos, id_tycho)),
#                              aes(colour=catalog, fill=catalog),
#                              columns=2:8, upper="blank", diag="blank") 
# g_pairs_cand_save = pairs_cand[2,1] + geom_point(size=1, alpha=0.5, position="jitter")
# # save
# ggsave("output/plots/scatter_cand_1.png", g_pairs_cand_save)
# 
