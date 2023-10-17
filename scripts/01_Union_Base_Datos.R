##########################################################
# Union bases de datos
# Autores: Juan Pablo Bermudez. Lina Bautista. Esteban Meza. Pharad Sebastian Escobar
##########################################################

# Limpiar environment -----------------------------------------------------
rm(list = ls())
cat('\014')

# Librerias ---------------------------------------------------------------
require(pacman)
p_load(tidyverse, # Manipular dataframes
       rio, # Importar datos fácilmente
       plotly, # Gráficos interactivos
       leaflet, # Mapas interactivos
       # rgeos, # ya no se encuentra en el CRAN, por buenas practicas no se utilizara
       units, # unidades
       sf, # Leer/escribir/manipular datos espaciales
       osmdata, # Obtener datos de OpenStreetMap (OSM)
       tidymodels, # Modelado de datos limpios y ordenados
       randomForest, # Modelos de bosque aleatorio
       rattle, # Interfaz gráfica para el modelado de datos
       spatialsample,
       xgboost, # Muestreo espacial para modelos de aprendizaje automático
       tmaptools,
       terra) 

# Directorios -------------------------------------------------------------
stores <- paste0(getwd(),'/stores/') # Directorio de base de datos
views  <- paste0(getwd(),'/views/')  # Directorio para guardar imagenes

# Lectura de datos --------------------------------------------------------
train <- read_csv(paste0(stores,'train.csv'))
test  <- read_csv(paste0(stores,'test.csv'))


# Etiquetas para test y train ---------------------------------------------
# La etiqueta 2 sera para 'test'
test <- test %>%
  mutate(type_data ='test')%>%
  select(type_data, everything())

# La etiqueta 1 sera para 'train'
train <- train%>%
  mutate(type_data = 'train')%>%
  select(type_data, everything())


# Union bases de datos ----------------------------------------------------
df <- bind_rows(train, test)

# Exportarlas a csv
write_csv(df, paste0(stores,"raw_complete_base.csv"))

