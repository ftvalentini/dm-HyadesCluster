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
hip_id = readRDS("data/working/hip_id.RDS")
# data de tyc
tyc_raw = readRDS("data/working/tyc_raw.RDS")
tyc_id = readRDS("data/working/tyc_id.RDS")


# clean data --------------------------------------------------------------
# hip
hipx = hip_raw %>% 
  # tipo de estrella (hyades, candidatas, otras)
  mutate(tipo = case_when(
    HIP %in% hip_id$id[hip_id$hyades==T] ~ "Hyades",
    HIP %in% candidates$id_hipparcos[candidates$catalog=="hipparcos"] ~ "candidatas",
    T ~ "otras")) %>% 
  mutate(tipo = factor(tipo, levels=c("Hyades","candidatas","otras"),
                       labels=c("Hyades","candidatas","otras"),
                       ordered=F)) %>% 
  # saca ID
  select(-HIP)


# tyc
tycx = tyc_raw %>% 
  # tipo de estrella (hyades, candidatas, otras)
  mutate(tipo = case_when(
    recno %in% tyc_id$id[tyc_id$hyades==T] ~ "Hyades",
    recno %in% candidates$id_tycho[candidates$catalog=="tycho"] ~ "candidatas",
    T ~ "otras")) %>% 
  mutate(tipo = factor(tipo, levels=c("Hyades","candidatas","otras"),
                       labels=c("Hyades","candidatas","otras"),
                       ordered=F)) %>% 
  # saca ID
  select(-c(recno,TYCID1,TYCID2,TYCID3,HD,HIP,VT,BT))


# pairs -------------------------------------------------------------------
### plots feos asi que lo hago a mano para una sola combinacion (RA y DE)
# # hip
# pairs_hip = GGally::ggpairs(hipx,
#                             aes(colour=tipo, fill=tipo),
#                             columns=1:7, upper="blank", diag="blank") 
# # indices de los plots a guardar:
# pairs_hip_save_i = list(c(2,1),c(7,6))
# # save plots
# walk2(pairs_hip_save_i,1:length(pairs_hip_save_i), 
#      function(i,n) 
#        ggsave(filename="output/plots/scatter_res_hip_"%+%n%+%".png",
#               plot=pairs_hip[i[1],i[2]] + geom_point(size=0.5, alpha=0.5, position="jitter")))
# 
# # tyc
# pairs_tyc = GGally::ggpairs(tycx,
#                              aes(colour=tipo, fill=tipo),
#                              columns=1:7, upper="blank", diag="blank") 
# # indices de los plots a guardar:
# pairs_tyc_save_i = list(c(2,1),c(7,6))
# # save plots
# walk2(pairs_tyc_save_i,1:length(pairs_tyc_save_i), 
#       function(i,n) 
#         ggsave(filename="output/plots/scatter_res_tyc_"%+%n%+%".png", scale=1.5,
#                plot=pairs_tyc[i[1],i[2]] + geom_point(size=0.5, alpha=0.5, position="jitter")))


# scatter a mano ------------------------------------------------------------
# hip
g_scat_hip_res = ggplot(hipx) + 
  geom_point(aes(x=RA_J2000, y=DE_J2000, color=tipo),
             size=1.25, position="jitter")
ggsave(filename="output/plots/scatter_res_hip_1.png", g_scat_hip_res,
       width=8,height=4,dpi=100)

# tyc
g_scat_tyc_res = ggplot(tycx) + 
  geom_point(aes(x=RA_J2000_24, y=DE_J2000, color=tipo),
             size=0.75, position="jitter")
ggsave(filename="output/plots/scatter_res_tyc_1.png", g_scat_tyc_res,
       width=8,height=4,dpi=320)




# # hip melt
# gdat_hip = hipx %>%
#   tidyr::gather(-tipo, key=variable, value=value)
# # tyc melt
# gdat_tyc = tycx %>%
#   tidyr::gather(-tipo, key=variable, value=value)
