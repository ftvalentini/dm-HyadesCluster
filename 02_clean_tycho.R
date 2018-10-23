# Genera y guarda:
# - tyc_raw: tycho raw
# - tyc: tycho clean (con identificador de Hyades y NA tratados)
# - tyc_sc: datos normalizados para hacer clustering, con id en rownames
# - tyc_id: base con ID y pertenencia a hd

# intro -------------------------------------------------------------------
source("libraries.R")
source("functions.R")

# read data ---------------------------------------------------------------
# ids de Hyades en hipparcos
hdids_raw = read.csv("output/hyades_ids.csv", colClasses="character") %>%
  as_tibble()
# Tychos (tyc) catalogue
tyc_raw <- readxl::read_excel("data/raw/hyades_source.xlsx", sheet="Tycho", 
                              col_types=c(rep("text",4),
                                          rep("numeric",8),
                                          rep("text",2),
                                          rep("numeric",1))) %>% as_tibble() 

# clean data --------------------------------------------------------------
tyc = tyc_raw %>% 
  # identificador de hyades
  mutate(hyades = if_else(HIP %in% hdids_raw$id_hip, TRUE, FALSE)) %>% 
  # saca vbles irrelevantes
  select(-c(TYCID1,TYCID2,TYCID3,HD,HIP)) %>% 
  # saca vbles que repite info (V tiene la misma info que VT - B-V es TL de BT y VT)
  select(-c(VT,BT)) %>% 
  # saca vlbes con muchos faltantes (Plx - 86.1%)
  select_if(~mean(is.na(.x))<0.75) 
# base para hacer clustering
tyc_sc = tyc %>% 
  # saca ID (recno) y identificador de hd
  select(-c(recno, hyades)) %>% 
  # transforma con minmax/scale
  map_dfr(minmax) %>%
  # scale() %>% 
  as.data.frame() %>% 
  # id (recno) como rownames para no perder en id en clustering
  set_rownames(tyc$recno)
# base con ID y pertenencia a hd
tyc_id = tyc %>% select(recno, hyades) %>% rename("id"="recno")

# save data ---------------------------------------------------------------
out_dfs = list(tyc_raw, tyc, tyc_id, tyc_sc) %>% 
  setNames(c("tyc_raw", "tyc", "tyc_id", "tyc_sc"))
walk2(out_dfs, names(out_dfs), 
      ~saveRDS(.x, file="data/working/"%+%.y%+%".RDS"))
