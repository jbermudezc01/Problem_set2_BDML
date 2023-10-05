##########################################################
# Prediccion precios de vivienda
# Autores: Juan Pablo Bermudez. Lina Bautista. Esteban Meza. Pharad Sebastian Escobar
##########################################################

## unir base de datos original ##

#Limpieza area de trabajo 
rm(list=ls())
cat('\014')

# cargar paquetes 
install.packages("pacman")
library(pacman)
# cargar librerias 
p_load(tidyverse, # Manipular dataframes
       rio, # Importar datos fácilmente
       plotly, # Gráficos interactivos
       leaflet, # Mapas interactivos
       rgeos, # Calcular centroides de un polígono
       units, # unidades
       sf, # Leer/escribir/manipular datos espaciales
       osmdata, # Obtener datos de OpenStreetMap (OSM)
       tidymodels, # Modelado de datos limpios y ordenados
       randomForest, # Modelos de bosque aleatorio
       rattle, # Interfaz gráfica para el modelado de datos
       spatialsample,
       xgboost) # Muestreo espacial para modelos de aprendizaje automático

# cargar base de datos 

test <- import('https://raw.githubusercontent.com/jbermudezc01/Problem_set2_BDML/main/stores/test.csv')
train <- import('https://raw.githubusercontent.com/jbermudezc01/Problem_set2_BDML/main/stores/train.csv')

# crear variable de etiqueta test 
test <- test %>%
  mutate(type_data = 2)%>%
  select(type_data, everything())
view(test)

# crear variable de etiqueta train

train <- train%>%
  mutate(type_data = 1)%>%
  select(type_data, everything())
view(train)

# unir bases de datos 
df <-  bind_rows(train, test)

# exportar a csv
write_csv(df, "bd.csv")

