knitr::opts_chunk$set(echo = TRUE)
options(scipen = 999, warn=-1)

#cargar Librerias
source("R/librerias.R")
# Cargar Funciones
source("R/funciones.R")


delitos <- readRDS( "data/delitos/delitos_all_categ.rds")

# Crear variable de tiempo ------------------------------------------------
# se debe crear una varible de tiempo de que conserve el orden

delitos <- delitos %>%
  st_drop_geometry() %>% 
  mutate(FECHA_DEL = ymd(FECHA_DEL)) %>% 
  mutate(tiempo = paste0(year(FECHA_DEL), "_",
                         sprintf("%02d",month(FECHA_DEL)))) %>% 
  mutate(tiempo = ym(tiempo))


# Contabilizar por mes ----------------------------------------------------

# General
del_mes <- delitos %>% 
  group_by(tiempo, GRUPO, TIPO) %>% 
  summarise(cantidad = n(), .groups = "keep") %>% 
  arrange(tiempo) %>% 
  as.data.frame()
saveRDS(del_mes, "data/resumen/del_mes.rds")
write.xlsx(del_mes, "data/tablas/del_mes.xlsx", overwrite = T)


# Regional
del_region <- delitos %>% 
  group_by(tiempo, REGION, NOM_REGION, GRUPO, TIPO) %>% 
  summarise(cantidad = n(), .groups = "keep") %>% 
  arrange(tiempo)%>% 
  as.data.frame()

saveRDS(del_region, "data/resumen/del_regional.rds")
write.xlsx(del_region, "data/tablas/del_region.xlsx", overwrite = T)


# Comunal
del_comuna <- delitos %>% 
  group_by(tiempo, REGION, NOM_REGION,COMUNA,  NOM_COMUNA, GRUPO, TIPO) %>% 
  summarise(cantidad = n(), .groups = "keep") %>% 
  arrange(tiempo)%>% 
  as.data.frame()

saveRDS(del_comuna, "data/resumen/del_comuna.rds")
write.xlsx(del_comuna, "data/tablas/del_comuna.xlsx", overwrite = T)


# Zona
del_zona <- delitos %>% 
  group_by(tiempo, REGION, NOM_REGION,COMUNA,  NOM_COMUNA,  ZONA,  GRUPO, TIPO) %>% 
  summarise(cantidad = n(), .groups = "keep") %>% 
  arrange(tiempo)%>% 
  as.data.frame()

saveRDS(del_zona, "data/resumen/del_zona.rds")
write.xlsx(del_zona, "data/tablas/del_zona.xlsx", overwrite = T)


del_tipo_zona <- delitos %>% 
  group_by(tiempo, REGION, NOM_REGION,COMUNA,  NOM_COMUNA,  ZONA,  TIPO) %>% 
  summarise(cantidad = n(), .groups = "keep") %>% 
  arrange(tiempo)%>% 
  as.data.frame()

saveRDS(del_tipo_zona, "data/resumen/del_tipo_zona.rds")
write.xlsx(del_tipo_zona, "data/tablas/del_tipo_zona.xlsx", overwrite = T)




# Contabilidad por mes - DENUNCIA -----------------------------------------

# General
del_mes <- delitos %>% 
  filter(CLASIFICAC == "DENUNCIA") %>% 
  group_by(tiempo, GRUPO, TIPO) %>% 
  summarise(cantidad = n(), .groups = "keep") %>% 
  arrange(tiempo) %>% 
  mutate(CLASIFICAC = "DENUNCIA") %>% 
  as.data.frame()
saveRDS(del_mes, "data/resumen/del_mes_denuncia.rds")
write.xlsx(del_mes, "data/tablas/del_mes_denuncia.xlsx", overwrite = T)

# Regional
del_region <- delitos %>% 
  filter(CLASIFICAC == "DENUNCIA") %>% 
  group_by(tiempo, REGION, NOM_REGION, GRUPO, TIPO) %>% 
  summarise(cantidad = n(), .groups = "keep") %>% 
  arrange(tiempo)%>% 
  mutate(CLASIFICAC = "DENUNCIA") %>% 
  as.data.frame()

saveRDS(del_region, "data/resumen/del_regional_denuncia.rds")
write.xlsx(del_region, "data/tablas/del_regional_denuncia.xlsx", overwrite = T)

# Comunal
del_comuna <- delitos %>% 
  filter(CLASIFICAC == "DENUNCIA") %>% 
  group_by(tiempo, REGION, NOM_REGION,COMUNA,  NOM_COMUNA, GRUPO, TIPO) %>% 
  summarise(cantidad = n(), .groups = "keep") %>% 
  arrange(tiempo)%>% 
  mutate(CLASIFICAC = "DENUNCIA") %>% 
  as.data.frame()

saveRDS(del_comuna, "data/resumen/del_comuna_denuncia.rds")
write.xlsx(del_comuna, "data/tablas/del_comuna_denuncia.xlsx", overwrite = T)

# Zona
del_zona <- delitos %>% 
  filter(CLASIFICAC == "DENUNCIA") %>% 
  group_by(tiempo, REGION, NOM_REGION,COMUNA,  NOM_COMUNA,  ZONA,  GRUPO, TIPO) %>% 
  summarise(cantidad = n(), .groups = "keep") %>% 
  arrange(tiempo)%>% 
  mutate(CLASIFICAC = "DENUNCIA") %>% 
  as.data.frame()

saveRDS(del_zona, "data/resumen/del_zona_denuncia.rds")
write.xlsx(del_zona, "data/tablas/del_zona_denuncia.xlsx", overwrite = T)


# Zona tipo
del_tipo_zona <- delitos %>% 
  filter(CLASIFICAC == "DENUNCIA") %>% 
  group_by(tiempo, REGION, NOM_REGION,COMUNA,  NOM_COMUNA,  ZONA,  TIPO) %>% 
  summarise(cantidad = n(), .groups = "keep") %>% 
  arrange(tiempo)%>% 
  mutate(CLASIFICAC = "DENUNCIA") %>% 
  as.data.frame()

saveRDS(del_tipo_zona, "data/resumen/del_tipo_zona_denuncia.rds")
write.xlsx(del_tipo_zona, "data/tablas/del_tipo_zona_denuncia.xlsx", overwrite = T)



# Contabilidad por mes DETENCIONES ----------------------------------------

# General
del_mes <- delitos %>% 
  filter(CLASIFICAC == "DETENCION") %>% 
  group_by(tiempo, GRUPO, TIPO) %>% 
  summarise(cantidad = n(), .groups = "keep") %>% 
  arrange(tiempo) %>% 
  mutate(CLASIFICAC = "DETENCION") %>% 
  as.data.frame()
saveRDS(del_mes, "data/resumen/del_mes_detencion.rds")
write.xlsx(del_mes, "data/tablas/del_mes_detencion.xlsx", overwrite = T)


# Regional
del_region <- delitos %>% 
  filter(CLASIFICAC == "DETENCION") %>% 
  group_by(tiempo,  REGION, NOM_REGION,GRUPO, TIPO) %>% 
  summarise(cantidad = n(), .groups = "keep") %>% 
  mutate(CLASIFICAC = "DETENCION") %>% 
  arrange(tiempo)%>% 
  as.data.frame()

saveRDS(del_region, "data/resumen/del_regional_detencion.rds")
write.xlsx(del_region, "data/tablas/del_regional_detencion.xlsx", overwrite = T)

# Comunal
del_comuna <- delitos %>% 
  filter(CLASIFICAC == "DETENCION") %>% 
  group_by(tiempo,  REGION, NOM_REGION,COMUNA,  NOM_COMUNA,GRUPO, TIPO) %>% 
  summarise(cantidad = n(), .groups = "keep") %>% 
  arrange(tiempo)%>% 
  mutate(CLASIFICAC = "DETENCION") %>% 
  as.data.frame()

saveRDS(del_comuna, "data/resumen/del_comuna_detencion.rds")
write.xlsx(del_comuna, "data/tablas/del_comuna_detencion.xlsx", overwrite = T)


# Zona
del_zona <- delitos %>% 
  filter(CLASIFICAC == "DETENCION") %>% 
  group_by(tiempo,  REGION, NOM_REGION,COMUNA,  NOM_COMUNA,ZONA,  GRUPO, TIPO) %>% 
  summarise(cantidad = n(), .groups = "keep") %>% 
  arrange(tiempo)%>% 
  mutate(CLASIFICAC = "DETENCION") %>% 
  as.data.frame()

saveRDS(del_zona, "data/resumen/del_zona_detencion.rds")
write.xlsx(del_zona, "data/tablas/del_zona_detencion.xlsx", overwrite = T)


# Zona tipo
del_tipo_zona <- delitos %>% 
  filter(CLASIFICAC == "DETENCION") %>% 
  group_by(tiempo,  REGION, NOM_REGION,COMUNA,  NOM_COMUNA,  ZONA,  TIPO) %>% 
  summarise(cantidad = n(), .groups = "keep") %>% 
  arrange(tiempo)%>% 
  mutate(CLASIFICAC = "DETENCION") %>% 
  as.data.frame()

saveRDS(del_tipo_zona, "data/resumen/del_tipo_zona_detencion.rds")
write.xlsx(del_tipo_zona, "data/tablas/del_tipo_zona_detencion.xlsx", overwrite = T)



