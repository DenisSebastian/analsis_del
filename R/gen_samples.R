# Objetivos: Generar Muestras de datos para book
# Autor: denis.berroeta@uai.cl
# Fecha: 06-05-2023

source("R/funciones.R")
source("R/librerias.R")


# lectura datos
datos_2017 <- readRDS(file = "data/delitos/raw/Casos_2017.rds")


# 2017 
sample_100 <-  muestra_n(datos_2017, n = 100, random = T)
saveRDS(sample_100, "data_samples/raw_2017_100.rds")


# crear una muestra de 200 registro por año
delitos_raw <-  list.files("data/delitos/raw/", full.names = T) %>% 
  map(readRDS) %>%
  map_df(~(muestra_n(., n = 200, random = T)))
  
saveRDS(delitos_raw, "data_samples/raw_all.rds")

  
delitos <- readRDS( "data/delitos/delitos_all_categ.rds")
del_cat_sampled <- delitos %>% 
  muestra_n(., n = 5000, random = T)

saveRDS(del_cat_sampled, "data_samples/del_cat_sampled.rds")




delitos_cat_bc_URB <- readRDS("data/delitos/delitos_eval_bcom_urb.rds")

sample_cat_bc_URB <- delitos_cat_bc_URB %>% 
  muestra_n(., n = 5000, random = T)


saveRDS(sample_cat_bc_URB, "data_samples/delitos_eval_bcom_urb.rds")
