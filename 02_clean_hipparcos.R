# Genera y guarda:
# - hip_raw: hipparcos raw
# - hip: hipparcos clean (con identificador de Hyades y NA tratados)
# - hip_sc: datos normalizados para hacer clustering, con id en rownames
# - hip_id: base con ID y pertenencia a hd

# intro -------------------------------------------------------------------

source("libraries.R")
source("functions.R")

# read data ---------------------------------------------------------------
# ids de Hyades en hipparcos
hdids_raw = read.csv("output/hyades_ids.csv", colClasses="character") %>% 
  as_tibble()
# hipparcos (hip) catalogue
hip_raw <- readxl::read_excel("data/raw/hyades_source.xlsx", sheet="Hipparcos", 
                              col_types=c("text",rep("numeric",7))) %>% 
  as_tibble() 



# clean data --------------------------------------------------------------
hip = hip_raw %>% 
  # identificador de hyades
  mutate(hyades = if_else(HIP %in% hdids_raw$id_hip, TRUE, FALSE))
# Reemplazo NA en vble "B-V" por la mediana (son solo 15 registros con NA)
mediana_bv <- median(hip$`B-V`, na.rm = TRUE)
hip$`B-V`[is.na(hip$`B-V`)] <- mediana_bv

# index de no outliers (fijamos 10 outliers)
good_i_hip = hip %>% select(-c(HIP, hyades)) %>% 
  MASS::cov.rob(quantile.used=nrow(.)-10) %$% best

# base para hacer clustering
hip_sc = hip %>% 
  # saca ID (HIP) y identificador de hd
  select(-c(HIP, hyades)) %>% 
  # saca outliers (???)
  # "["(good_i_hip,) %>% 
  # transforma con min max/scale
  map_dfr(minmax) %>%
  # scale() %>%
  as.data.frame() %>% 
  # id como rownames para no perder en id en clustering
  set_rownames(hip$HIP
               # [good_i_hip]
               )

# base con ID y pertenencia a hd (saca outliers)
hip_id = hip %>% select(HIP, hyades) %>% rename("id"="HIP") 
# %>% 
  # "["(good_i_hip,)


# save data ---------------------------------------------------------------
out_dfs = list(hip_raw, hip, hip_id, hip_sc) %>% 
  setNames(c("hip_raw", "hip", "hip_id", "hip_sc"))
walk2(out_dfs, names(out_dfs), 
      ~saveRDS(.x, file="data/working/"%+%.y%+%".RDS"))
