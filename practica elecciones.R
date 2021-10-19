# 1 - Leemos los datos -------------------------- 
# electorales para la tercera seccion electoral
datos <- read.csv("https://raw.githubusercontent.com/LeanColisko/elecciones_2019/main/datos/elecciones_2019_datos_tercera_seccion.csv", fileEncoding = "UTF-8")



# 2 Funciones exploratorias -----------------------
# Nombre de columnas
names(datos)

# Estructura de datos
str(datos)

# Resumen de las variables
summary(datos)

# Primeras 6 lineas de datos
head(datos)

# Últimas 6 lineas de datos
tail(datos)



# 3 - Modificación de los datos -----------------
# Hay dos columnas que no nos hacen falta: PROVINCIA y CATEGORÍA
# Los valores de estas columnas son siempre los mismos.
# Vamos a quedarnos con las que necesitamos

# Como vamos a trabajar con dplyr: primero activamos la librería
library(dplyr)

datos_clean <- datos %>% 
  select(-c(PROVINCIA, NOMBRE_CATEGORIA))

# Ahora tenemos 5 variables:
# Sección
# Circuito
# NOMBRE_AGRUPACION
# PASO
# GRAL


# 4 - Resultados por agrupación -------------------

# Con los datos limpios vamos a ver la suma de votos de cada agrupación
datos_clean %>% 
  group_by(NOMBRE_AGRUPACION) %>% 
  summarise(PASO_x_AGRUP = sum(PASO),
            GRAL_x_AGRUP = sum(GRAL)) %>% 
  mutate(`% PASO` = PASO_x_AGRUP / sum(PASO_x_AGRUP) * 100,
         `% GRAL` = GRAL_x_AGRUP / sum(GRAL_x_AGRUP) * 100)


# Resultados para Avellaneda
datos_clean %>% 
  filter(SECCION == 'Avellaneda') %>% 
  group_by(NOMBRE_AGRUPACION) %>% 
  summarise(PASO_x_AGRUP = sum(PASO),
            GRAL_x_AGRUP = sum(GRAL)) %>% 
  mutate(`% PASO` = PASO_x_AGRUP / sum(PASO_x_AGRUP) * 100,
         `% GRAL` = GRAL_x_AGRUP / sum(GRAL_x_AGRUP) * 100)



# Datos para todas las secciones
resultados <- datos_clean %>% 
  group_by(SECCION, NOMBRE_AGRUPACION) %>% 
  summarise(PASO_x_AGRUP = sum(PASO),
            GRAL_x_AGRUP = sum(GRAL)) %>% 
  mutate(`% PASO` = PASO_x_AGRUP / sum(PASO_x_AGRUP) * 100,
         `% GRAL` = GRAL_x_AGRUP / sum(GRAL_x_AGRUP) * 100) %>% 
  arrange(SECCION, desc(`% GRAL`)) %>% 
  mutate(Puestos_GRAL = row_number()) %>% 
  arrange(SECCION, desc(`% PASO`)) %>% 
  mutate(Puestos_PASO = row_number())


# Mostramos los resultados de quiene salieron primeros en la GRAL 
resultados %>%
  filter(Puestos_GRAL==1)



# 5 - Transposición de datos ----------------

# Queremos construir una tabla con los datos 
# A nivel de circuito. Cada fila representa un circuito y los circuitos 
# No están duplicados

# Esto lo podemos hacer pivoteando con las funciones de tidyr
library(tidyr)

datos_circuito <- datos_clean %>% 
  pivot_wider(id_cols = c(SECCION, Circuito), 
              names_from = NOMBRE_AGRUPACION,
              names_sep = " ; ",
              values_from = c(PASO, GRAL))

ganadores_gral <- datos_clean %>% 
  filter(GRAL != 0) %>% 
  group_by(SECCION, Circuito) %>% 
  arrange(Circuito, desc(GRAL)) %>% 
  mutate(POSICION_GRALES = row_number()) %>%
  ungroup() %>% 
  filter(POSICION_GRALES == 1) %>% 
  select(SECCION, Circuito, NOMBRE_AGRUPACION) %>% 
  rename(GANADOR_GENERAL = NOMBRE_AGRUPACION)

ganadores_paso <- datos_clean %>% 
  filter(PASO != 0) %>% 
  group_by(SECCION, Circuito) %>% 
  arrange(Circuito, desc(PASO)) %>% 
  mutate(POSICION_PASO = row_number()) %>%
  ungroup() %>% 
  filter(POSICION_PASO == 1) %>% 
  select(SECCION, Circuito, NOMBRE_AGRUPACION) %>% 
  rename(GANADOR_PASO = NOMBRE_AGRUPACION)


ganadores <- ganadores_gral %>% 
  left_join(ganadores_paso)



# 6 - Lectura de datos geográficos ---------------
library(sf)

# Datos del circuito electoral
circuitos <- sf::read_sf("https://mapa2.electoral.gov.ar/geoserver/wfs?service=WFS&version=1.0.0&request=GetFeature&authkey=6f5439345ec2e15026534251f16a709a&typeName=descargas:circuito_02&maxFeatures=2000&outputFormat=application%2Fjson")



library(ggplot2)
ggplot(data = circuitos) +
  geom_sf()



# 7 - Unión de datos geográficos y electorales ----------
circuitos <- circuitos %>% 
  left_join(ganadores, by = c("circuito"="Circuito")) %>% 
  filter(!is.na(SECCION))

## Graficamos los datos de los ganadores de la eleccion PASO
ggplot(data = circuitos, aes(fill = GANADOR_PASO)) +
  geom_sf() +
  scale_fill_manual(values = c("#18C2EC", "#FFFF33")) +
  labs(title = "Elección PASO 2019", 
       subtitle = "Categoría a Presidente. Ganador por circuito electoral.",
       caption = "Tercera sección electoral") +
  theme(legend.position = c(0.1, 0.1))


## Graficamos los datos de los ganadores de la eleccion GRAL
ggplot(data = circuitos, aes(fill = GANADOR_GENERAL)) +
  geom_sf() +
  scale_fill_manual(values = c("#18C2EC", "#FFFF33")) +
  labs(title = "Elección GENERAL 2019", 
       subtitle = "Categoría a Presidente. Ganador por circuito electoral.",
       caption = "Tercera sección electoral") +
  theme(legend.position = c(0.1, 0.1))




