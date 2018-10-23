# intro -------------------------------------------------------------------
gc()
source("libraries.R")
source("functions.R")
semilla = 1


# descripcion hipparcos ---------------------------------------------------
# datos
hip_raw = readRDS(file="data/working/hip_raw.RDS")
# descripciones
hip_names_desc = c("Identificador",
                   "xxx",
                   "xxx",
                   "Paralaje",
                   "xxx",
                   "xxx",
                   "xxx",
                   "xxx")
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
                   "ID???",
                   "ID???",
                   "ID???",
                   "ra",
                   "de",
                   "pmra",
                   "pmde",
                   "bt",
                   "vt",
                   "v",
                   "bv",
                   "Identificador de Hyades (?)",
                   "Identificador de Hipparcos",
                   "Paralaje")
# tabla
tyc_desc = tibble(Nombre=names(tyc_raw), 
                  Descripción=tyc_names_desc, 
                  Tipo=map_chr(tyc_raw, typeof))


# save tables -------------------------------------------------------------
saveRDS(hip_desc, file="output/desc_hipparcos.RDS")
saveRDS(tyc_desc, file="output/desc_tycho.RDS")




