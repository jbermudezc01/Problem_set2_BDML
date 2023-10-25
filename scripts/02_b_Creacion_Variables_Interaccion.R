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

bd <- bd %>%
  select(-c(OBJECTID,CODIGO_MAN, CODIGO_CRI, CODIGO_ZON, NORMATIVA, ACTO_ADMIN,
            NUMERO_ACT, FECHA_ACTO, FECHA_CAPT, RESPONSABL, SHAPE_AREA, SHAPE_LEN, ESCALA_CAP))

#Identificar los NA de la variable creada
bd %>% 
  summarise(na_count_estrato = sum(is.na(ESTRATO))) 

#Imputamos los NA a través de la moda según la UPZ.
moda <- function(x) {
  return(as.numeric(names(which.max(table(x)))))
}

bd <- bd %>%
  group_by(UPZ) %>%
  mutate(ESTRATO=ifelse(is.na(ESTRATO),moda(ESTRATO), ESTRATO))

bd <- bd%>%
  rename(estrato=ESTRATO)

# Creación de las variables de interacción-------------------------------------

bd<-bd%>%
  mutate(inter_transmixsurface=distancia_tm*surface2)%>%
  mutate(inter_transmixarearesidencial=distancia_tm*area_residencial_manzana)%>%
  mutate(inter_transmixciclovia=distancia_cycleway*distancia_mall)%>%
  mutate(inter_estratoxmall=estrato*distancia_mall)%>%
  mutate(inter_estratoxbar=estrato*distancia_bar)%>%
  mutate(inter_estratoxnightclub=estrato*distancia_nightclub)%>%
  mutate(inter_estratoxcollege=estrato*distancia_college)%>%
  mutate(inter_estratoxcollege=estrato*distancia_restaurant)%>%
  mutate(inter_estratoxuniversidad=estrato*distancia_university)

#Agregación de la variable surface a través de KNN
KNN<-read_csv(paste0(stores,'base_datos_kvecinos.csv'))

  bd$surface_total_knn=KNN$surface_total_kv
  bd$surface_covered_knn =KNN$surface_covered_kv

# Exportar a csv y .RData -------------------------------------------------
write_csv(bd, file = paste0(stores,'base_datos_tratada.csv'))
save(bd, geometria.osm, coordenadas.x.centroides, coordenadas.y.centroides, file = paste0(stores,'Datos_limpios.RData'))

