##########################################################
#Creación de la variable estrato
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
       terra,
       geojsonR) 

# Directorios -------------------------------------------------------------
stores <- paste0(getwd(),'/stores/') # Directorio de base de datos
views  <- paste0(getwd(),'/views/')  # Directorio para guardar imagenes
datos.bogota <- paste0(stores,'Bases_Bogota/')

# Cargar base de datos ----------------------------------------------------
load(paste0(stores,'Datos_limpios.RData'))

#Datos de Bogotá estratificación ------------------------------------------
estrato          <-st_read(paste0(datos.bogota,'manzanaestratificacion/ManzanaEstratificacion.shp'))

# Joint espacial ----------------------------------------------------------
# Corregir las geometrias invalidas
estrato_valid     <- st_make_valid(estrato)

# Realizar la union
#estrato
bd <- st_join(bd,estrato_valid, join=st_within)

