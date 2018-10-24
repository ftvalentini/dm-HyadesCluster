source("libraries.R")
source("functions.R")

fig_width = 6
fig_height = 4

# font latex --------------------------------------------------------------

# Install TTF Latin Modern fonts from http://www.fontsquirrel.com/fonts/latin-modern-roman. 
# extrafont::font_import() # tarda 5 min - run only once
extrafont::loadfonts(device="win", quiet=T)
par(family = "LM Roman 10")

# read data ---------------------------------------------------------------

# Hipparcos (hip) catalogue
hip_raw <- readxl::read_excel("data/raw/hyades_source.xlsx", sheet="Hipparcos", 
                          col_types=c("text", rep("guess", 7)))
# Symbad catalogue (only Hyades (hd))
sym_raw <- readxl::read_excel("data/raw/hyades_source.xlsx", sheet="Symbad")
# ids of hd within hip
idcruz_raw <- readxl::read_excel("data/raw/id_cruzada_Symbad_Hipparcos.xlsx", col_names=F) 

# cambios en data (solo vbles relevantes - nombres de vars)
hip = hip_raw %>% select(c(HIP, RA_J2000, DE_J2000)) %>% 
  setNames(c("id_hip","ra_hip","de_hip"))
sym = sym_raw %>% select(c(identifier, RA_J2000, DE_J2000)) %>% 
  setNames(c("id_sym","ra_sym","de_sym"))
idcruz = idcruz_raw %>% setNames(c("id_sym", "id_hip")) %>% 
  # saca "HIP " de id_hip para hacer match con hip
  mutate_at("id_hip", function(x) stringr::str_extract(x,'[:digit:]+'))


# errores de ubicacion de hd entre sym y hip ------------------------------

# join de hip con idcruz (!is.na(hip$id_sym) son las hd confirmadas) 
hip_m = hip %>%  left_join(idcruz, by="id_hip")
# join de sym con hip (agrega a sym la ubicacion de hip de las hd de idcruz)
sym_m = sym %>% left_join(hip_m, by="id_sym") %>% 
  # distancia euclidea entre las ubicaciones sym y hyp ('error de medicion')
  mutate(error = sqrt((ra_sym-ra_hip)^2+(de_sym-de_hip)^2))
# histograma de los errores
g1 = ggplot(na.omit(sym_m)) +
  geom_histogram(aes(x=error), bins=35) +
  labs(title = "Figura 1. Distribucion de los errores de medicion",
       x = "Distancia euclideana",
       y = "Frecuencia") +
  theme(text=element_text(family="LM Roman 10")) +
  NULL
ggsave("output/plots/pre_01.png", g1, type="cairo-png", width=fig_width, height=fig_height)

# stats
na.omit(sym_m$error) %>% summary %>% broom::tidy()
max_error = na.omit(sym_m$error) %>% max
# ubicacion de cada estrella en el espacio (plot)
# gdat = na.omit(sym_m) %>% 
#   reshape(idvar = c("id_sym","id_hip"),
#           varying = list(c("ra_sym","ra_hip"), c("de_sym","de_hip")),
#           timevar="catalog", times = c("sym","hip"), v.names=c("RA","DE"), 
#           direction="long")
# ggplot(gdat) + 
#   geom_point(aes(x=RA, y=DE, color=catalog, pch=catalog)) +
#   scale_shape_manual(values=c(21,19))


# asignacion inicial -------------------------------------

# stars de sym relevantes ( no se incluyen las hd confirmadas)
symx = sym_m %>% 
  dplyr::filter(is.na(error)) %>% 
  select(c(id_sym, ra_sym, de_sym))

# stars de hip contra las cuales se compara cada star de symx
hipx = hip_m %>% 
  # no se incluyen las hd confirmadas
  dplyr::filter(is.na(id_sym)) %>% 
  select(c(id_hip, ra_hip, de_hip))

# pairwise euclidean distances entre cada par de estrellas hip y sym
# hecho con funcion de internet:
# distances = pwdist(x=symx[-1], y=hipx[-1], names_x=symx[[1]], names_y=hipx[[1]])
  # con Rbase:
tot = rbind(as.matrix(symx[-1]),as.matrix(hipx[-1]))
rownames(tot) = c(symx$id_sym, hipx$id_hip)
distances_tot = dist(tot, method="euclidean", diag=T, upper=T) %>% as.matrix()
distances = distances_tot[rownames(distances_tot) %in% symx$id_sym,
                          colnames(distances_tot) %in% hipx$id_hip]

# iteracion: se selecciona el par de minima distancia y se elimina de la matriz hasta no quedar pares
# si hay empate la eleccion es al azar
dtemp = distances
results = data.frame(id_sym=NA, id_hip=NA, dist=NA)
i = 1
while (i<nrow(symx)) {
  # indexes of min distance
    # primera fila: si hay empate la eleccion es al azar
  minrc = which(dtemp == min(dtemp), arr.ind=TRUE)[1,]
  minr = minrc[1] #row
  minc = minrc[2] #col
  # save results
  results[i,"id_sym"] = rownames(dtemp)[minr]
  results[i,"id_hip"] = colnames(dtemp)[minc]
  results[i,"dist"] = dtemp[minr,minc]
  # drop row and col
  dtemp = dtemp[-minr,-minc]
  i = i + 1
}
# el ultimo valor se hace aparte porque dtemp deja de ser matrix:
results[i,"id_sym"] = symx$id_sym[!symx$id_sym %in% results$id_sym]
results[i,"id_hip"] = names(dtemp)[which(dtemp == min(dtemp))]
results[i,"dist"] = dtemp[which(dtemp == min(dtemp))]
# join con ubicacion en sym y hip
results_m = results %>% left_join(sym, by="id_sym") %>% 
  left_join(hip, by="id_hip") %>% as_tibble()
# results y distancias de valores por encima de umbral (max_error) ("resto")
resto = results %>% dplyr::filter(dist>1*max_error)
distances_resto = distances[rownames(distances) %in% resto$id_sym,
                            colnames(distances) %in% resto$id_hip]

# plot A -- symbad + hip mas cercanas (incluyendo idcruz)
dat1 = (sym_m %>% dplyr::filter(!is.na(id_hip))) %>% 
  bind_rows(results_m)
gdat1 = dat1 %>% 
  reshape(idvar = c("id_sym","id_hip"),
          varying = list(c("ra_sym","ra_hip"), c("de_sym","de_hip")),
          timevar="catalog", times=c("sym","hip"), v.names=c("RA","DE"), 
          direction="long") %>% 
  mutate(catalog = if_else(catalog=="hip" & !is.na(error), "hip_true", catalog))
g2 = ggplot(gdat1) + 
    geom_point(aes(x=RA, y=DE, color=catalog, pch=catalog), size=2) +
    scale_shape_manual(values=c(21,5,19),
                       name ="Catalogo",
                       breaks=c("hip", "hip_true","sym"),
                       labels=c("Hipparcos candidatas pr. (154)", 
                                "Hipparcos efectivas (24)",
                                "Symbad efectivas (178)")) +
    scale_color_manual(values=c("red","black","#619CFF"),
                       name="Catalogo",
                       breaks=c("hip", "hip_true","sym"),
                       labels=c("Hipparcos candidatas pr. (154)", 
                                "Hipparcos efectivas (24)",
                                "Symbad efectivas (178)")) +
    labs(title = "Figura 2. 178 Hyades: efectivas fuente Symbad, \nefectivas fuente Hipparcos y candidatas preliminares",
         x = "RA",
         y = "DE") +
    theme(text=element_text(family="LM Roman 10")) +
    NULL
ggsave("output/plots/pre_02.png", g2, type="cairo-png", width=fig_width, height=fig_height)

# plot B -- symbad + hip mas cercanas (no inc idcruz)
gdat2 = results_m %>% 
  reshape(idvar = c("id_sym","id_hip"),
          varying = list(c("ra_sym","ra_hip"), c("de_sym","de_hip")),
          timevar="catalog", times = c("sym","hip"), v.names=c("RA","DE"), 
          direction="long") %>% 
  mutate(catalog = if_else(catalog=="hip" & !(id_hip %in% resto$id_hip), 
                           "hip_est1", catalog))
g3 = ggplot(gdat2) + 
  geom_point(aes(x=RA, y=DE, color=catalog, pch=catalog), size=2) +
  scale_shape_manual(values=c(21,21,19),
                     name ="Catalogo",
                     breaks=c("hip", "hip_est1","sym"),
                     labels=c("Hipparcos candidatas resto (128)", 
                              "Hipparcos asignadas (26)",
                              "Symbad efectivas (154)")) +
  scale_color_manual(values=c("#F8766D","black","#619CFF"),
                     name="Catalogo",
                     breaks=c("hip", "hip_est1","sym"),
                     labels=c("Hipparcos candidatas resto (128)", 
                              "Hipparcos asignadas (26)",
                              "Symbad efectivas (154)")) +
  labs(title = "Figura 3. 154 Hyades: efectivas fuente Symbad, \nasignadas fuente Hipparcos y candidatas restantes",
       x = "RA",
       y = "DE") +
  theme(text=element_text(family="LM Roman 10")) +
  NULL
ggsave("output/plots/pre_03.png", g3, type="cairo-png", width=fig_width, height=fig_height)


# save HIP ids de efectivas y candidatas ----------------------------------

efect = gdat1 %>% dplyr::filter(catalog=="hip_true") %>% select(id_sym, id_hip) %>% 
  mutate(tipo="efectiva")
cand = gdat2 %>% dplyr::filter(catalog=="hip_est1") %>% select(id_sym, id_hip) %>% 
  mutate(tipo="estimada")
hyades_ids = bind_rows(efect, cand)
write.csv(hyades_ids, file="output/hyades_ids.csv", row.names=F)


# asignacion de restantes -----------------------------------------------------

# plot de la tarea a realizar
gdat3 = gdat2 %>% dplyr::filter(dist>max_error)
g4 = ggplot(gdat3) + 
  geom_point(aes(x=RA, y=DE, color=catalog, pch=catalog), size=2) +
  scale_shape_manual(values=c(21,19),
                     name ="Catalogo",
                     breaks=c("hip","sym"),
                     labels=c("Hipparcos candidatas resto (128)", 
                              "Symbad efectivas (128)")) +
  scale_color_manual(values=c("red","#619CFF"),
                     name ="Catalogo",
                     breaks=c("hip","sym"),
                     labels=c("Hipparcos candidatas resto (128)", 
                              "Symbad efectivas (128)")) +
  labs(title = "Figura 4. 128 Hyades: efectivas fuente Symbad y \ncandidatas restantes de Hipparcos",
       x = "RA",
       y = "DE") +
  theme(text=element_text(family="LM Roman 10")) +
  NULL
ggsave("output/plots/pre_04.png", g4, type="cairo-png", width=fig_width, height=fig_height)


# de las restantes, asigna estrella mas cercana si la segunda mas cercana
# no esta a menos de 'd' distancias de la mas cercana
  # itera por estrella symbad, desde la de menor distancia hasta la mayor, 
  # descartando a medida que hay una asignacion
  # tambien descarta a cualquier otra estrella que tenia como mas cercana a una asignada
# sym ordenadas por minima distancia
mindist_resto = distances_resto %>% apply(1, min) %>% sort
# hip mas cercana de cada sym
nn_resto = distances_resto %>% apply(1, function(x) names(which.min(x)))
d = 1.5
vecino = c()
dist_vecino = c()
dtemp_r = distances_resto
for (i in names(mindist_resto)) { 
  star_dists = dtemp_r[i,]
  min_i = which.min(star_dists)
  # no asigna vecino si el segundo es 'cercano' o si su mas cercano ya fue asignado
  if (sort(star_dists)[2] < d*star_dists[min_i] | nn_resto[i] %in% vecino) {
    vecino[i] = NA; dist_vecino[i] = NA
    # caso contrario asigna y elimina al vecino elegido
  } else {
    vecino[i] = names(min_i); dist_vecino[i] = star_dists[min_i] 
    dtemp_r = dtemp_r[,-min_i]
  }
}  
results_resto = data.frame(id_sym=names(mindist_resto), id_hip=vecino, dist=dist_vecino) %>% 
  as_tibble()

# join con ubicacion en sym y hip (deja solo las asignadas)
results_resto_m = results_resto %>% left_join(sym, by="id_sym") %>% 
  left_join(hip, by="id_hip") %>% na.omit() %>% as_tibble()


# plot
# 4. symbad + hip mas cercanas asignadas (no inc idcruz ni las 26 debajo de umbral)

# gdat4 = gdat3 %>% 
#   mutate(catalog = if_else(catalog=="hip" & id_hip %in% results_resto_m$id_hip, 
#                            "hip_est2", catalog))
# (g4 = ggplot(gdat4) + 
#     geom_point(aes(x=RA, y=DE, color=catalog, pch=catalog), size=2) +
#     scale_shape_manual(values=c(21,21,19)) +
#     scale_color_manual(values=c("#F8766D","black","#619CFF")) +
#     NULL)



# punto de quiebre --------------------------------------------------------

gdat5 = results %>% 
  mutate(cum_dist_abs = cumsum(abs(dist)),
         cum_dist_sq = cumsum(dist^2),
         id = 1:nrow(results))
g5 = ggplot(gdat5 %>% dplyr::filter(id_hip %in% resto$id_hip)) +
  geom_line(aes(id, cum_dist_abs))+
  labs(title = "Figura 5. Valor absoluto de las distancias \n(acumulado)",
       x = "Nro pareja",
       y = "Distancia acumulada") +
  theme(text=element_text(family="LM Roman 10")) +
  NULL
ggsave("output/plots/pre_05.png", g5, type="cairo-png", width=fig_width, height=fig_height)



# umbral mas tolerante ----------------------------------------------------
# factor de multiplicacion del error
f_error = seq(50, by=100, length.out=20)
# ids_hip que quedan sin asignar para cada f_error
ids_hip_resto = f_error %>% map(function(x)
  dplyr::filter(results, dist>x*max_error) %$% id_hip)
# dataframes para cada id_hip_resto
gdat6_list = ids_hip_resto %>% map(
  function(x) 
    results_m %>% 
  reshape(idvar = c("id_sym","id_hip"),
          varying = list(c("ra_sym","ra_hip"), c("de_sym","de_hip")),
          timevar="catalog", times = c("sym","hip"), v.names=c("RA","DE"), 
          direction="long") %>% 
  mutate(catalog = if_else(catalog=="hip" & !(id_hip %in% x), 
                           "hip_est1", catalog))
)
# lista de plots para cada dframe (cambiar labels una vez elegido el mejor)
g6_list = map2(.x=gdat6_list, .y=f_error, function(a,b) 
  ggplot(a) + 
    geom_point(aes(x=RA, y=DE, color=catalog, pch=catalog), size=2) +
    scale_shape_manual(values=c(21,21,19),
                       name ="Catalogo",
                       breaks=c("hip", "hip_est1","sym"),
                       labels=c("Hipparcos candidatas resto (124)", 
                                "Hipparcos asignadas (30)",
                                "Symbad efectivas (154)")) +
    scale_color_manual(values=c("#F8766D","black","#619CFF"),
                       name="Catalogo",
                       breaks=c("hip", "hip_est1","sym"),
                       labels=c("Hipparcos candidatas resto (124)", 
                                "Hipparcos asignadas (30)",
                                "Symbad efectivas (154)")) +
    labs(title = "Figura 6. 154 Hyades: efectivas fuente Symbad,\nasignadas fuente Hipparcos y candidatas restantes\n('f'=" %+% b %+%")",
         x = "RA",
         y = "DE") +
    theme(text=element_text(family="LM Roman 10")) +
    NULL
)
# se guarda grafico de f_error==650
f_best = 650
g_index = which(f_error==f_best)
ggsave("output/plots/pre_06.png",
       g6_list[[g_index]], type="cairo-png", width=fig_width, height=fig_height)


# codigo viejo ------------------------------------------------------------

# sym4 = results_m %>% dplyr::filter(dist>max_error) %>% 
#   select(ra_sym, de_sym) %>% setNames(c("RA","DE"))
# hip4 = results_m %>% dplyr::filter(dist>max_error) %>% 
#   mutate(tipo = if_else(id_hip %in% results_resto_m$id_hip, "est", "other")) %>% 
#   select(ra_hip, de_hip, tipo) %>% setNames(c("RA","DE","tipo"))
# ggplot() +
#   geom_point(data=sym4, aes(x=RA, y=DE), color="black", size=2) +
#   geom_point(data=hip4, aes(x=RA, y=DE, color=tipo), pch=21, size=2) +
#   NULL


# # long para ggplot
# resg = results_m %>% 
#   reshape(idvar = c("id_sym","id_hip"),
#           varying = list(c("ra_sym","ra_hip"), c("de_sym","de_hip")),
#           timevar="catalog", times = c("sym","hip"), v.names=c("RA","DE"), 
#           direction="long")
# # plot
# ggplot(resg) + 
#   geom_point(aes(x=RA, y=DE, color=catalog, pch=catalog)) +
#   scale_shape_manual(values=c(21,19))
# # usando como valor umbral la distancia maxima entre las hd confirmadas
# resg_u = resg %>% dplyr::filter(dist<=100*max_error)
# ggplot(resg_u) + 
#   geom_point(aes(x=RA, y=DE, color=catalog, pch=catalog)) +
#   scale_shape_manual(values=c(21,19))
# 
# # plot con las hd efectivas mas las identificadas
# resg_tot = sym_m %>% dplyr::filter(!is.na(id_hip)) %>% select(c(id_sym,id_hip,ra_sym,de_sym)) %>% 
#   rename(RA=ra_sym, DE=de_sym) %>% cbind(catalog="sym") %>% 
#   bind_rows(resg_u)
# ggplot(resg_tot) + 
#   geom_point(aes(x=RA, y=DE, color=catalog, pch=catalog)) +
#   scale_shape_manual(values=c(8,19))

# # hd totales
# nrow(sym)
# # hd identificadas
# nrow(idcruz) + nrow(resg_u)/2


# # convierte en lista para iterar (cada estrella un elemento)
# symxl = t(symx[-1]) %>% as.data.frame() %>% as.list() %>% setNames(symx$id_sym)
# # convierte en lista para iterar (cada estrella un elemento)
# hipxl = t(hipx[-1]) %>% as.data.frame() %>% as.list() %>% setNames(hipx$id_hip)
# 
# # distancia entre cada star de sym con hip stars
# disti = symxl %>% map(function(x) rowdist(hipx[-1], x, rownames=hipx$id_hip))
# # top 2 mas cercanas
# dist2 = map(disti, function(x) sort(x) %>% head(2))
# # check if hay stars a igual distancia
# map_lgl(dist2, function(x) anyDuplicated(x)>0) %>% any()
# # no hay empates: usamos dist1
# dist1 = map(disti, function(x) sort(x) %>% head(1))
# # check if una star de hip es la mas cercana de 2 o mas de sym
# dupli = dist1 %>% map_chr(names) %>% every_dup()
# # si, y sucede en 113 casos
# dist1[dupli] %>% length
# # top 10 mas cercanas
# dist10 = map(disti, function(x) sort(x) %>% head(10))

# bb = sym %>% dplyr::filter(id_sym %in% names(mindist_resto),
#                       ra_sym>4.4, ra_sym<4.48, de_sym>13, de_sym<13.3) %$% id_sym
# cc = hip %>% dplyr::filter(id_hip %in% nn_resto,
#                            ra_hip>4.4, ra_hip<4.48, de_hip>13.1, de_hip<13.2) %$% id_hip
# nn_resto[bb]
# nn_resto[nn_resto=="20762"]
# sym %>% dplyr::filter(id_sym %in% bb)
# hip %>% dplyr::filter(id_hip %in% nn_resto[bb])
# distances['[BKM2008] HC18c10-169l',] %>% sort %>% head
# dist2$`[BKM2008] HC18c10-169l`
# distances['2MASS J04291927+1317214',] %>% sort %>% head
# 1.25*0.1225154
# 
# aa = c('[BKM2008] HC18c10-169l','2MASS J04291927+1317214')
# ggplot(symx %>% dplyr::filter(id_sym %in% rownames(distances_resto))) +
#   geom_point(aes(x=ra_sym, y=de_sym)) +
#   geom_point(data=symx %>% dplyr::filter(id_sym %in% aa), aes(x=ra_sym, y=de_sym), color="red")
# dd = c('20762','21179')
# ggplot(hipx %>% dplyr::filter(id_hip %in% nn_resto)) +
#   geom_point(aes(x=ra_hip, y=de_hip)) +
#   geom_point(data=hip %>% dplyr::filter(id_hip %in% dd), aes(x=ra_hip, y=de_hip), color="red")
# 
# nn_resto[c('Cl* Melotte 25 LH 111','[BKM2008] HC13c09-1062l')]
# dist1[c('Cl* Melotte 25 LH 111','[BKM2008] HC13c09-1062l')]

# a = hip %>% dplyr::filter(id_hip %in% c('20762','21179')) %>% select(ra_hip, de_hip) %>% 
#   as.matrix() %>% apply(2, )
# b = sym %>% dplyr::filter(id_sym %in% c('[BKM2008] HC18c10-169l')) %>% select(ra_sym, de_sym) %>%
#   as.matrix()
# c = rbind(a, b) %>% set_rownames(c('20762','21179','[BKM2008] HC18c10-169l'))
# dist(c, method="euclidean", diag=T, upper=T)
