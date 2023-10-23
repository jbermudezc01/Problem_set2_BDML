##########################################################
# Limpieza datos + datos espaciales OSM + datos extraidos de descripcion + datos espaciales distritales
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

# Abrir las bases de datos a unir -----------------------------------------
load(paste0(stores,'Datos_limpios.RData'))
df.python <- read.csv(paste0(stores,'base_datos_kvecinos.csv'))

# Dejar observaciones unicas de df.python
df.python <- df.python %>% 
  distinct_all()

# Agregar las columnas ----------------------------------------------------
# En las bases de python hay dos columnas de interes: surface_total_kv y surface_covered_kv
# Se van a agregar a la base principal. Lo importante de estas variables es que se uso k neighbour
# para imputar los NA de las variables de surface
bd$surface_total_kv   <- df.python$surface_total_kv
bd$surface_covered_kv <- df.python$surface_covered_kv

# Guardar nuevamente la base de datos -------------------------------------

# Exportar a csv y .RData -------------------------------------------------
write_csv(bd, file = paste0(stores,'base_datos_tratada.csv'))
save(bd, geometria.osm, coordenadas.x.centroides, coordenadas.y.centroides, file = paste0(stores,'Datos_limpios.RData'))

# En //Datos_limpios.RData van a guardarse la base de datos bd, la geometria.osm, coordenadas.x.centroides y 
# coordenadas.y.centroides
