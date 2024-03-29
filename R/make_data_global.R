


# librerías ---------------------------------------------------------------
library(openxlsx)
library(dplyr)
library(lubridate)
library(sf)


# Excel comunas Meses -----------------------------------------------------


# Base Denuncia Seguro Total

# Lectura denuncia seguro
denuncias <- readRDS("data/datos_trabajo_final/denuncia_seguro.rds") %>% 
  st_as_sf() %>% st_drop_geometry() # Descartar información espacial
head(denuncias)

# limpieza denuncia seguro
denuncias <- denuncias %>% rename(NOM_REGION = REGION) %>% 
  mutate(MESES = (ANIO*100) + MES)   #unión de año y mes

# Lectura de clasificación de denuncia seguro
clasificaden <- read.xlsx("data/datos_trabajo_final/clasificacion_denuncias.xlsx", 
                          sheet = "tipos") 

clasificaden <-  clasificaden %>% 
  select(-tip)


# Aromización de tablas denuncia seguro

# denuncias %>% count(SUBDELITO) 
# clasificaden %>% count(SUBDELITO)

denuncias <- denuncias %>% left_join(clasificaden, by = "SUBDELITO") 



# Armonización de tablas (compatibilizar comunas y regiones)
indcom <- read.xlsx("data/datos_trabajo_final/comunas_carabineros_spd.xlsx")
indreg <- read.xlsx("data/datos_trabajo_final/regiones_carabineros_spd.xlsx")

denuncias <- denuncias %>% left_join(indcom, by = "COMUNA", 
                                     relationship = "many-to-many")
denuncias <- denuncias %>% left_join(indreg, by = "NOM_REGION")

# selección de variables

denuncias <- denuncias %>% 
  mutate(CLASIFICAC = "MENSAJE") %>% 
  select(REGION,NOM_REGION,COMUNA,NOM_COMUNA,MESES,TIPO,CLASIFICAC)

denuncias %>% select(COMUNA, NOM_COMUNA, NOM_REGION, REGION) %>% head(5)


# Casos Policiales Totales

# Lectura Casos Policiales
casos <- readRDS("data/datos_trabajo_final/delitos_all_categ.rds")

# Limpieza Casos Policiales
casos <- casos %>% 
  st_drop_geometry() %>% 
  select(-NOM_REGION, -COMUNA) %>%  # eliminar columnas
  mutate(REGION = as.numeric(REGION),
         MESES = year(FECHA_DEL)*100 + month(FECHA_DEL), 
         COMUNA = floor(as.numeric(ZONA)/1000000))

# Asignación manual delitos con problemas tipo
obj_count <- casos %>% count(TIPO) # llama la atención Tipo de baja fecuencia
saveRDS(obj_count, "data_samples/tab_count.rds")


# ver NA
obj_na <- casos %>% 
  filter(is.na(TIPO)) %>%  #casos con Tipo NA
  pull(DELITO) # mostrar los delitos

saveRDS(obj_na, "data_samples/tab_na.rds")
# corrección NA y comercio Ilegal

casos <- casos %>% 
  mutate(TIPO = case_when(
    DELITO == "BOTILLERIA,SUPERMERC.,MINIMARKET QUE EXP.LICOR CONSUMIDO INT.LOCAL S/P" ~ "Desórdenes",
    DELITO == "ROBO DE VEHICULO MOTORIZADO POR SORPRESA, VIOLENCIA O I_" ~ "Robo vehículos",
    DELITO == "ROBO CON VIOLENCIA ART.436 INC 1 433,438,439" ~ "Robo violento",
    DELITO == "VIOLENCIA INTRAFAMILIAR OTROS (LESIONES PSICOLOGICAS O LEVES)" ~ "Violencia familiar",
    TIPO == "Comercio Ilegal" ~ "Comercio ilegal",
    .default = TIPO
    ))

# Asignación manual delitos con problemas tipo
count_total <- casos %>% count(TIPO) # llama la atención Tipo de baja fecuencia
saveRDS(count_total, "data_samples/tab_total.rds")

# Codificación  de tipos de delitos

# Armomización de Tablas

casos <- casos %>% left_join(indreg, by = "REGION")
casos <- casos %>% 
  filter(!is.na(NOM_COMUNA)) 


# guardar resultados clean
saveRDS(casos, "data/datos_trabajo_final/resultados/casos_clean.rds")


# selección de variables
casos <- casos %>% 
  select(REGION,NOM_REGION,COMUNA,NOM_COMUNA,MESES,TIPO,CLASIFICAC)



# Fusión de tablas y estructuración en panel long
hechos <- rbind(casos,denuncias)
hechoscomunames <- hechos %>%
  group_by(REGION,NOM_REGION,COMUNA,NOM_COMUNA,MESES,CLASIFICAC,TIPO) %>%
  summarise(HECHOS = n(), .groups = "keep")

# filtrar por fecha 201700
hechoscomunames <- hechoscomunames %>% 
  filter(MESES > 201700)

# Guardar Excel
write.xlsx(hechoscomunames, "data/datos_trabajo_final/resultados/hechoscomunames.xlsx", 
           rownames = F)
saveRDS(hechoscomunames, "data/datos_trabajo_final/resultados/hechoscomunames.rds")




# Shape evolución comunas -------------------------------------------------


delcomuna <- hechoscomunames %>% 
  filter(CLASIFICAC %in% c("DENUNCIA","DETENCION"))



varianzas_base <- delcomuna %>% 
  # filter(!is.na(TIPO)) %>% 
  group_by(COMUNA, TIPO) %>% 
  summarise(varianza = sd(HECHOS),
            cantidad = sum(HECHOS),  .groups = "keep")

head(varianzas_base)


# **Periodo 1**
  

ini <- delcomuna %>%
  filter(MESES %in% 201707:201712) %>% 
  group_by(COMUNA, TIPO) %>% 
  summarise(inicio = sum(HECHOS),  .groups = "keep")


# **Periodo 2**
  
fin <- delcomuna %>%
  filter(MESES %in% 202207:202212) %>% 
  group_by(COMUNA, TIPO) %>% 
  summarise(fin = sum(HECHOS),  .groups = "keep")



### Consolidación
adjvar <- varianzas_base %>% 
  left_join(ini, by = c("COMUNA", "TIPO")) %>% 
  left_join(fin, by = c("COMUNA", "TIPO")) %>% 
  mutate_if(.predicate = is.numeric,
            .funs = function(x) ifelse(is.na(x), 0, x))

# Indicador de Variación 
adjvar <- adjvar %>% 
  mutate(adjvar = (fin-inicio)/(fin+inicio)) %>% 
  mutate_if(.predicate = is.numeric,
            .funs = function(x) ifelse(is.na(x), 0, x))

# Visualizacions
library(hrbrthemes)
p <- adjvar %>%
  ggplot( aes(x=adjvar)) +
  geom_histogram( binwidth=0.1, 
                  fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  scale_x_continuous(breaks = seq(-1, 1, by = 0.2))+
  ggtitle("Histograma de Variación Normalizada - Comunal") +
  theme_ipsum() +
  theme(
    plot.title = element_text(size=15)
  )
p


var_max <- max(adjvar$fin-adjvar$inicio, na.rm = T)
a <- 1
b <- 1:20

data <- expand_grid(a, b) %>% 
  mutate(cambio = (b-a)/(a+b))

v <- ggplot(data = data, aes(x = b, y = cambio))+
  geom_line(color = "#69b3a2", linewidth =1)+
  scale_y_continuous(breaks = seq(0, 1.2, by = 0.1))+
  scale_x_continuous(breaks = seq(0, 20, by = 1))+
  ggtitle("Representación de la Variación Normalizada")+
  theme_bw()

v


# renombrar TIPO con diccionario
abrev_tipos <- read.xlsx("data/datos_trabajo_final/abreviaciones.xlsx") 

adjvar <- adjvar %>% 
  left_join(abrev_tipos, by = c("TIPO" = "Tipo.delito")) %>% 
  mutate(TIPO = Abreviacion) %>% 
  select(-Abreviacion)


# Pivot Wider
varianzas_w <- adjvar%>% 
  select(-varianza, -cantidad, -inicio, -fin ) %>% 
  pivot_wider(names_from = TIPO,
              values_from = c(adjvar)) %>% 
  mutate(COMUNA = sprintf("%05d", as.numeric(COMUNA)))



# Guardar Comunal Espacial
comunas_point <- readRDS("data/ine/Comunas_Chile.rds") %>% 
  st_centroid() %>% 
  select(-Shape_Leng, -Shape_Area)

varianzas_w_points <- comunas_point %>%
  left_join(varianzas_w, by  = "COMUNA") %>% 
  mutate_if(.predicate = is.numeric,
            .funs = function(x) ifelse(is.na(x), 0, x))

# r <-  "13"
# tipo_del <- "Desor"
# dif = filter(varianzas_w_points, REGION == r)
# mapview::mapview(dif, zcol = tipo_del)

st_write(varianzas_w_points, 
         "data/datos_trabajo_final/resultados/comdel_dif_points.shp", 
         delete_dsn = T)

# como poligonos comunales
comunas_pol <- readRDS("data/ine/Comunas_Chile.rds") %>% 
  select(-Shape_Leng, -Shape_Area)


varianzas_w_pol <- comunas_pol %>%
  left_join(varianzas_w, by  = "COMUNA") %>% 
  mutate_if(.predicate = is.numeric,
            .funs = function(x) ifelse(is.na(x), 0, x))

dif_pol = filter(varianzas_w_pol, REGION == r)
mapview::mapview(dif_pol, zcol = tipo_del)


st_write(varianzas_w_pol, 
         "data/datos_trabajo_final/resultados/comdel_dif_pol.shp", 
         delete_dsn = T)



# Zonas -------------------------------------------------------------------


casos <- readRDS("data/datos_trabajo_final/resultados/casos_clean.rds")
casos %>% st_drop_geometry() %>% count(TIPO)


hechos <- casos %>% 
  select(REGION,NOM_REGION,COMUNA,NOM_COMUNA,ZONA, 
         MESES,TIPO,CLASIFICAC)




# Contabilizar los Delitos 
hechosZonas <- hechos %>%
  group_by(REGION,NOM_REGION,COMUNA,NOM_COMUNA,ZONA,
           MESES,CLASIFICAC,TIPO) %>%
  summarise(HECHOS = n(), .groups = "keep")

# hechoscomunames

# filtrar por fecha desde  enero del 2017, encones podríamos filtar con la fechas superior al número `201700`

hechosZona <- hechosZonas %>% 
  filter(MESES > 201700)


# **Selección de Periodos, cálculo de frecuencias y variación**
  
varianzas_base_z <- hechosZonas %>% 
  group_by(ZONA, TIPO) %>% 
  summarise(varianza = sd(HECHOS),
            cantidad = sum(HECHOS),  .groups = "keep")

head(varianzas_base)


# - Periodo 1

ini_z <- hechosZonas %>%
  filter(MESES %in% 201707:201712) %>% 
  group_by(ZONA, TIPO) %>% 
  summarise(inicio = sum(HECHOS),  .groups = "keep")


# - Periodo 2

fin_z <- hechosZonas %>%
  filter(MESES %in% 202207:202212) %>% 
  group_by(ZONA, TIPO) %>% 
  summarise(fin = sum(HECHOS),  .groups = "keep")

# - Consolidación


adjvar <- varianzas_base_z %>% 
  left_join(ini_z, by = c("ZONA", "TIPO")) %>% 
  left_join(fin_z, by = c("ZONA", "TIPO")) %>% 
  mutate_if(.predicate = is.numeric,
            .funs = function(x) ifelse(is.na(x), 0, x)) # si es NA -> 0


# - Indicador de Variación 
adjvar <- adjvar %>% 
  mutate(adjvar = (fin-inicio)/(fin+inicio)) %>% 
  mutate_if(.predicate = is.numeric,
            .funs = function(x) ifelse(is.na(x), 0, x))
adjvar


# - Histograma de las diferencias normalizadas entre periodos

library(hrbrthemes)
p <- adjvar %>%
  ggplot( aes(x=adjvar)) +
  geom_histogram( binwidth=0.2, 
                  fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  scale_x_continuous(breaks = seq(-1, 1, by = 0.2))+
  ggtitle("Histograma de Variación Normalizada - Zona") +
  theme_ipsum() +
  theme(
    plot.title = element_text(size=15)
  )
p


  
  
  
  # **Estructuración en Panel Wide**
  
adjvar <- adjvar %>% 
  left_join(abrev_tipos, by = c("TIPO" = "Tipo.delito")) %>% 
  mutate(TIPO = Abreviacion) %>% 
  select(-Abreviacion)
adjvar


# Transformar en formato wider

# Pivot Wider
varianzas_w_z <- adjvar%>% 
  select(-varianza, -cantidad, -inicio, -fin ) %>% 
  pivot_wider(names_from = TIPO,
              values_from = c(adjvar), values_fill = 0) 

# **Guardar Resultados Espacial**
  
  
  # - Lectura de Archivo de poligonos comunales del INE

zonas_poligonos <- readRDS("data/ine/zonas_urb_consolidadas.rds") %>% 
  st_transform(4326) %>%  # crs de latitud longitud
  select(REGION, NOM_REGION, PROVINCIA, NOM_PROVIN,
         COMUNA, NOM_COMUNA, URBANO, GEOCODIGO, AREA)


### Puntos Comunales

#convertir a puntos
zonas_point <-  zonas_poligonos %>% 
  st_centroid()


#agregar los resultados de evolución
varianzas_w_points_z <- zonas_point %>%
  left_join(varianzas_w_z, by  = c("GEOCODIGO" = "ZONA")) %>% 
  mutate_if(.predicate = is.numeric,
            .funs = function(x) ifelse(is.na(x), 0, x))

# Inspección visual
r <-  "13"
tipo_del <- "Desor"
dif = filter(varianzas_w_points_z, REGION == r)
mapview::mapview(dif, zcol = tipo_del)


#Guardar Resultados
st_write(varianzas_w_points_z, 
         "data/datos_trabajo_final/resultados/zondel_dif_points.shp", 
         delete_dsn = T)


#agregar los resultados de evolución
varianzas_w_pol_z <- zonas_poligonos %>%
  left_join(varianzas_w_z, by  =  c("GEOCODIGO" = "ZONA")) %>% 
  mutate_if(.predicate = is.numeric,
            .funs = function(x) ifelse(is.na(x), 0, x))


# Inspección visual
r <-  "13"
tipo_del <- "Desor"
dif_pol = filter(varianzas_w_pol_z, REGION == r)
mapview::mapview(dif_pol, zcol = tipo_del)

#Guardar Resultados
st_write(varianzas_w_pol_z, 
         "data/datos_trabajo_final/resultados/zondel_dif_pol.shp", 
         delete_dsn = T)
  
  


