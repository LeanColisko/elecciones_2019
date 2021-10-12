

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

# Primeros datos
head(datos)

# Últimos datos
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

