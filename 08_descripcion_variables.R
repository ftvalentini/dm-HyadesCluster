# intro -------------------------------------------------------------------
gc()
source("libraries.R")
source("functions.R")
semilla = 1


# descripcion hipparcos ---------------------------------------------------
# datos
hip_raw = readRDS(file="data/working/hip_raw.RDS")
names(hip_raw)
# descripciones
hip_names_desc = c("Identificador",
                   "Ascensión recta",
                   "Declinación",
                   "Paralaje",
                   "Movimiento propio en ascensión recta",
                   "Movimiento propio en declinación",
                   "Magnitud en banda V de Johnson",
                   "Color BV de Johnson")
# tabla
hip_desc = tibble(Nombre=names(hip_raw),
                  Descripción=hip_names_desc,
                  Tipo=map_chr(hip_raw, typeof))

# descripcion hipparcos ---------------------------------------------------
# datos
tyc_raw = readRDS(file="data/working/tyc_raw.RDS")
# descripciones
names(tyc_raw)
tyc_names_desc = c("Identificador",
                   "ID de región según catálogo Guide Star",
                   "ID corriente dentro de la región",
                   "ID específico de Tycho",
                   "Ascensión recta",
                   "Declinación",
                   "Movimiento propio en ascensión recta",
                   "Movimiento propio en declinación",
                   "(B-V)/0.85 + VT",
                   "V + 0.09*(BT-VT)",
                   "Magnitud en banda V de Johnson",
                   "Color BV de Johnson",
                   "Identificador de Hyades",
                   "Identificador de Hipparcos",
                   "Paralaje")
# tabla
tyc_desc = tibble(Nombre=names(tyc_raw),
                  Descripción=tyc_names_desc,
                  Tipo=map_chr(tyc_raw, typeof))


# save tables -------------------------------------------------------------
saveRDS(hip_desc, file="output/desc_hipparcos.RDS")
saveRDS(tyc_desc, file="output/desc_tycho.RDS")
