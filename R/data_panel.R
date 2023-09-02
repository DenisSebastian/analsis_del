knitr::opts_chunk$set(echo = TRUE)
options(scipen = 999, warn=-1)

#cargar Librerias
source("R/librerias.R")
# Cargar Funciones
source("R/funciones.R")


# Lectura de data
delitos_cat_bc_URB <- readRDS("data/delitos/delitos_eval_bcom_urb.rds")
head(delitos_cat_bc_URB)


# Agregar la variable mes
delitos_urb <- delitos_cat_bc_URB %>% 
  st_drop_geometry() %>% 
  rename(fecha = FECHA_DEL) %>% 
  mutate(mes = month(fecha),
         anno = year(fecha)) %>% 
  filter(!is.na(Categoria))
head(delitos_urb %>%  select(fecha, mes, anno))


tab_del_all <- delitos_urb%>%
  mutate(a_mes = paste0( anno, "_", sprintf("%02d",mes))) %>% 
  group_by(ZONA, Categoria, a_mes )%>%
  summarise(Cantidad = n(), .groups = "keep")

head(tab_del_all)


#pivot wider

tab_del_all_mes <- tab_del_all%>%
  pivot_wider(names_from = a_mes, values_from = Cantidad, 
              names_prefix = "a_", values_fill = 0)%>% 
  dplyr::select(ZONA, Categoria,sort(names(.)[3:ncol(.)]))
head(tab_del_all_mes)

#guargar resultados
write.xlsx(tab_del_all_mes,
           "data/tablas/tablas_del_mes/del_zonas_all.xlsx", overwrite = T)
saveRDS(tab_del_all_mes, "data/tablas/tablas_del_mes/del_zonas_all.rds")




# Cálculo de tablas de densidad casos policiales --------------------------



# Lectura de Zonas Urbanas consolidadas

zonas <- readRDS("data/ine/zonas_urb_consolidadas.rds")


info_zonas_base <- zonas %>%
  st_drop_geometry() %>% 
  dplyr::select(GEOCODIGO = COD_INE_16, AREA) %>% 
  mutate(GEOCODIGO = as.character(GEOCODIGO))


# Calcular delitos anual y agregar área

tab_del_all_mes <- tab_del_all_mes %>%
  as.data.frame() %>% #acelera el proceso
  mutate(anual = rowSums(across(.cols = starts_with("a_")))) %>% 
  left_join(info_zonas_base, by = "GEOCODIGO")



# Calcular la densidad delitos por Hectárea (10000 metros)

tab_all_dens <- tab_del_all_mes %>% 
  mutate(across(starts_with("a_"), ~ .x/(AREA/10000))) %>% 
  mutate(dens_anual = anual / (AREA/10000))

head(tab_all_dens[,1:10])

write.xlsx(tab_all_dens, 
           "data/tablas/tabla_dens_mes/del_dens_all.xlsx",
           overwrite = T)

saveRDS(tab_all_dens, "data/tablas/tabla_dens_mes/del_dens_all.rds")


# Tablas de Densidad de Delitos por Mes (2013 al 2022)

info_zonas_base <- zonas %>%
  dplyr::select(GEOCODIGO = COD_INE_16, REGION, 
                NOM_REGION, COMUNA, NOM_COMUNA) %>% 
  mutate(GEOCODIGO = as.character(GEOCODIGO))

del_dens_zonas <- info_zonas_base %>% 
  left_join(tab_all_dens, by ="GEOCODIGO")
head(del_dens_zonas[,1:10])



# Selección de Zonas Censales con Mayor Densidad


hist(del_dens_zonas$dens_anual, breaks = 100)
# hist(del_dens_zonas[del_dens_zonas$dens_anual>50,]$dens_anual, breaks = 100)


# definición de zonas que corresponde al porcentaje
n_porc <- porc_df(del_dens_zonas, porcentaje = 5)
n_porc

dens_max <- del_dens_zonas %>% 
  slice_max(dens_anual, n = n_porc) 


# Densidades 

mapview(dens_max, zcol = "dens_anual")



# Conocer que región tiene las zonas con mayor concentración de delitos


regiones_max <- dens_max %>% 
  st_drop_geometry() %>% 
  group_by(REGION) %>% 
  summarise(Cantidad = n()) %>% 
  arrange(desc(Cantidad))

regiones_max
reg_dens_max2 <- dens_max %>% 
  filter(REGION == 13) %>% 
  filter(!is.na(dens_anual))



unique(del_dens_zonas$Categoria)
categoria_tipo <-  "Comercio ilegal"  

dens_max_cat <- del_dens_zonas %>% 
  filter(Categoria==categoria_tipo) %>% 
  slice_max(dens_anual, n = n_porc) 



regiones_max_cat <- dens_max_cat %>% 
  st_drop_geometry() %>% 
  group_by(REGION) %>% 
  summarise(Cantidad = n()) %>% 
  arrange(desc(Cantidad))

regiones_max_cat


reg_dens_max_cat <- dens_max_cat %>% 
  filter(REGION == 13) %>% 
  filter(!is.na(dens_anual))%>% 
  slice_max(dens_anual, n = 100) 

mapview(reg_dens_max_cat, zcol = "dens_anual")



#  Densidades con Respecto al Tiempo --------------------------------------


categoria_tipo <-  "Comercio ilegal" 
ts_info <- dens_max %>% 
  st_drop_geometry() %>% 
  dplyr::select(Categoria, a_2017_01:a_2022_12) %>% 
  pivot_longer(cols = a_2017_01:a_2022_12,
               names_to = "tiempo", values_to = "densidades") %>% 
  mutate(tiempo = gsub("a_", "", tiempo),
         tiempo = gsub("_", "-", tiempo),
         tiempo = ym(tiempo)) 

head(ts_info)



pline <- ggplot() + 
  geom_line(data = ts_info, aes(x = tiempo, y = densidades,
                                colour = Categoria),
            alpha = 0.4) +
  xlab('Meses ') +
  ylab('Densidades') +
  theme_bw()
pline


pline <- ggplot() + 
  geom_line(data = ts_info, aes(x = tiempo, y = densidades,
                                colour = Categoria),
            alpha = 0.4) +
  xlab('Meses ') +
  ylab('Densidades') +
  theme_bw()+
  facet_grid(Categoria ~ .) + 
  theme(legend.position = "none")
# pline

ggplotly(pline)


# Gráficos sobre médidas de centralidad (8 Primeras Categoría

unique(ts_info$Categoria)

df_tidy_mean_1 <- ts_info %>%
  filter(!is.na(densidades)) %>%
  filter(Categoria %in% unique(ts_info$Categoria)[1:8]) %>%
  group_by(tiempo, Categoria) %>%
  summarise(n = n(),
            mean = mean(densidades),
            median = median(densidades),
            sd = sd(densidades),
            .groups = "keep") %>%
  mutate(sem = sd / sqrt(n - 1),
         CI_lower = mean + qt((1-0.95)/2, n - 1) * sem,
         CI_upper = mean - qt((1-0.95)/2, n - 1) * sem)

mean_plot_1 <- ggplot(df_tidy_mean_1, aes(x = tiempo, y = mean, color = Categoria)) +
  geom_line(aes(x = tiempo, y = mean, color = Categoria)) +
  geom_ribbon(
    aes(ymin = CI_lower, ymax = CI_upper, fill = Categoria),
    color = "grey90",
    alpha = 0.2 )+
  theme_bw()

ggplotly(mean_plot_1)



color_list <- viridis::viridis(length(unique(ts_info$Categoria)))
mean_plot_1_solos <- ggplot(df_tidy_mean_1, aes(x = tiempo, y = mean, color = Categoria)) +
  geom_line(aes(x = tiempo, y = mean, color = Categoria)) +
  geom_ribbon(
    aes(ymin = CI_lower, ymax = CI_upper, fill = Categoria),
    color = "grey90",
    alpha = 0.2) +  
  theme_bw()+
  scale_fill_manual(values = color_list) +
  scale_color_manual(values = color_list) +
  facet_grid(Categoria ~ .) + 
  theme(legend.position = "none")


ggplotly(mean_plot_1_solos)

