knitr::opts_chunk$set(echo = TRUE, eval = FALSE)
options(scipen = 999, warn=-1)

#cargar Librerias
source("R/librerias.R")
# Cargar Funciones
source("R/funciones.R")

tab_all_dens <- readRDS("data/tablas/tabla_dens_mes/del_dens_all.rds")
del_dens_zonas <-  readRDS("data/tablas/tabla_dens_mes/del_dens_zonas_pol.rds")

delcomuna <- readRDS("data/resumen/del_comuna.rds") 
delcomuna <- delcomuna %>% 
  mutate(COMUNA = sprintf("%05d", as.numeric(COMUNA)))


varianzas <- delcomuna %>% 
  filter(!is.na(TIPO)) %>% 
  group_by(COMUNA, TIPO) %>% 
  summarise(varianza = sd(cantidad),
            cantidad = n(),  .groups = "keep")

ini <- delcomuna %>%
  filter(tiempo >= ymd("2017-07-01") & tiempo <= ymd("2017-12-31")) %>% 
  group_by(COMUNA, TIPO) %>% 
  summarise(inicio = n(),  .groups = "keep")


fin <- delcomuna %>%
  filter(tiempo >= ymd("2022-07-01") & tiempo <= ymd("2022-12-31")) %>% 
  group_by(COMUNA, TIPO) %>% 
  summarise(fin = n(),  .groups = "keep")


adjvar <- varianzas %>% 
  left_join(ini, by = c("COMUNA", "TIPO")) %>% 
  left_join(fin, by = c("COMUNA", "TIPO")) %>% 
  mutate_if(.predicate = is.numeric,
            .funs = function(x) ifelse(is.na(x), 0, x))


adjvar <- adjvar %>% 
  mutate(adjvar = (fin-inicio)/(fin+inicio))

hist(adjvar$adjvar)


# pivot wider


varianzas <- adjvar %>% 
  mutate(TIPO = gsub(pattern = " ", replacement = "_", x = TIPO)) %>% 
  pivot_wider(names_from = TIPO,
                       values_from = c(varianza, cantidad, inicio, fin, adjvar))



Comunas_Chile <- readRDS("data/ine/Comunas_Chile.rds")
varianzas

comdel_dif <- Comunas_Chile %>% 
  filter(COMUNA %in% varianzas$COMUNA) %>%
  left_join(varianzas, by  = "COMUNA") %>% 
  mutate_if(.predicate = is.numeric,
            .funs = function(x) ifelse(is.na(x), 0, x))


mapview(comdel_dif %>% filter(REGION == "05"), zcol = "adjvar_Secuestros")
