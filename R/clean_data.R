knitr::opts_chunk$set(echo = TRUE)
options(scipen = 999, warn=-1)

#cargar Librerias
source("R/librerias.R")
# Cargar Funciones
source("R/funciones.R")


#Unificar Bases

delitos_raw <-  list.files("data/delitos/raw/", full.names = T) %>% 
  map_df(readRDS)


# Seleccion de variables

# selección de variables
cols_names <- c("DELITO", "GRUPO", "CLASIFICAC", 
                "CATEGORIA","GRUPO_DELI", "GRUPO_INTE",
                "FECHA_DEL",  "DIA",  "MES", "ANO",
                "HORA_DEL_D", "HORA24", "BLOQUE",
                "LUGAR", "geometry")

# delitos <- delitos %>% map(~select_at(.tbl = ., .vars = cols_names))
delitos_raw <- delitos_raw %>% select_at(.tbl = ., .vars = cols_names)


## Eliminar Duplicados


# Primero creamos una variable temporal de delitos con las columnas de interés:
del <-  delitos_raw %>% 
  sf2df() %>% #variable temporal tipo df (sacar lon lat)
  dplyr::select(HORA_DEL_D, FECHA_DEL,DELITO,  lon, lat)


# Se genera el ID para valores únicos
delitos_raw$ID <- cumsum(!duplicated(del[,1:5]))

# Se extraen los duplicados (son los duplicados)
duplicados <- delitos_raw[duplicated(delitos_raw$ID), ]
# head(duplicados)
id_casos_dup <- duplicados$ID
# Eliminar duplicados
delitos_clean <- delitos_raw[!duplicated(delitos_raw$ID), ]



# ### Imputar datos de Zonas Censales a los Delitos
zonas <- readRDS("data/ine/zonas_urb_consolidadas.rds") 



zonas_inf_fil <- zonas %>% 
  dplyr::select(REGION, NOM_REGION, PROVINCIA, NOM_PROVIN,
                COMUNA, NOM_COMUNA, URBANO,
                DISTRITO, LOC_ZON, GEOCODIGO)

delitos_info <- add_info_intersects(zonas_inf_fil, delitos_clean)
saveRDS(delitos_info, "data/delitos/delitos_all_info.rds")


delitos_info <-  readRDS("data/delitos/delitos_all_info.rds")


## CAtogroización
del_sigla <- read.xlsx("data/excel/delito_native.xlsx", 
                       sheet = "delitos") %>% 
  mutate(DELITO = toupper(DELITO)) %>% 
  rename(Sigla =CATEGORIA)
head(del_sigla)

delitos_cat <- left_join(delitos_info, del_sigla,
                         by = c("DELITO"))



categorias <- read.xlsx("data/excel/delito_native.xlsx", 
                        sheet = "categorias")%>% 
  rename(Tipo_del = Tipo)

head(categorias)

delitos_cat <- left_join(delitos_cat, categorias, 
                         by = "Sigla")
# head(delitos_cat)

grupos <-  read.xlsx("data/excel/categorias_grupos.xlsx") %>% 
  select(-cantidad, -GRUPO_DELI,-GRUPO_INTE )

head(grupos)

delitos_cat <- left_join(delitos_cat, grupos, 
                         by = "DELITO")

saveRDS(delitos_cat, "data/delitos/delitos_all_categ.rds")

# #Descartar los NA en Categoria
delitos_cat_sigla <- delitos_cat%>%
  filter(!is.na(Categoria))

saveRDS(delitos_cat_sigla, "data/delitos/delitos_only_cat.rds")



## Filtrar por Barrios Comerciales
# delitos_cat <- readRDS( "data/delitos/delitos_all_categ.rds")

delitos_cat_bc_URB <- delitos_cat%>%
  filter(Barrios_Comerciales == 1) 


saveRDS(delitos_cat_bc_URB, "data/delitos/delitos_eval_bcom_urb.rds")


region_num <- "05"
del_reg <-  delitos_cat_bc_URB %>% 
  filter(REGION == region_num) 

# mapview(del_reg, zcol = "Categoria")

