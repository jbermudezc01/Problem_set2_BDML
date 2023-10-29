# Análisis descriptivo 
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
       spatialsample, # Muestreo espacial para modelos de aprendizaje automático
       xgboost,
       tmaptools,
       terra,
       purrr,
       glmnet,
       huxtable) 

# Directorios -------------------------------------------------------------
stores    <- paste0(getwd(),'/stores/') # Directorio de base de datos
views     <- paste0(getwd(),'/views/')  # Directorio para guardar imagenes
templates <- paste0(getwd(),'/templates/') # Directorio para crear templates

# Cargar base de datos ----------------------------------------------------
load(paste0(stores,'Datos_limpios.RData'))

# Estadisticas descriptivas -----------------------------------------------

# crear un data frame sin geometria para facilitar calculos 

df <-bd
df$geometry <- NULL

# caracteristicas internas de la vivienda por train y test -----------------

# definimos función para calcular la moda 

# Si no tienes una función para calcular la moda, aquí hay una:
mode_with_frequency <- function(v) {
  uniqv <- unique(v)
  mode_value <- uniqv[which.max(tabulate(match(v, uniqv)))]
  frequency <- max(tabulate(match(v, uniqv)))
  return(list(mode = mode_value, freq = frequency))
}

# Seleccionamos variables internas 
internas <- df %>%
  select(property_type, rooms:bathrooms, piso_numerico, parqueadero:piso_laminado, surface_total_knn) %>%
  group_by(property_type) %>%
# calculamos moda, frecuencia y promedio
  summarise(
    rooms_media = median(rooms, na.rm = TRUE),
    bedrooms_media = median(bedrooms, na.rm = TRUE),
    bathrooms_media = median(bathrooms, na.rm = TRUE),
    piso_mean = mean(piso_numerico, na.rm = TRUE),
    parqueadero_moda = mode_with_frequency(parqueadero)$mode,
    parqueadero_freq = mode_with_frequency(parqueadero)$freq,
    terraza_moda = mode_with_frequency(terraza)$mode,
    terraza_freq = mode_with_frequency(terraza)$freq,
    ascensor_moda= mode_with_frequency(ascensor)$mode,
    ascensor_freq = mode_with_frequency(ascensor)$freq,
    vigilancia_moda = mode_with_frequency(vigilancia)$mode,
    vigilancia_freq = mode_with_frequency(vigilancia)$freq,
    deposito_moda = mode_with_frequency(deposito)$mode,
    deposito_freq = mode_with_frequency(deposito)$freq,
    cocina_moda = mode_with_frequency(cocina_integral)$mode,
    cocina_freq = mode_with_frequency(cocina_integral)$freq,
    piso_moda = mode_with_frequency(piso_laminado)$mode,
    piso_freq = mode_with_frequency(piso_laminado)$freq,
    surface_total = mean(surface_total_knn, na.rm = TRUE)
  )

# variables externas de la vivienda: distancia y condiciones urbanisticas del entorno ------

externas <- df%>%
  select(property_type,distancia_bank,distancia_mall,distancia_bus_station,distancia_cafe,distancia_clinic,
         distancia_fitness_centre,distancia_police,distancia_restaurant,distancia_sitp,distancia_tm,
         area_residencial_manzana,numero_predios_manzana)

# crear vector de variables númericas

numeric_vars <- c("distancia_bank","distancia_mall","distancia_bus_station","distancia_cafe","distancia_clinic",
                  "distancia_fitness_centre","distancia_police","distancia_restaurant","distancia_sitp","distancia_tm",
                  "area_residencial_manzana","numero_predios_manzana")

# aplicar función de calculo de estadisticas descriptivas 

all_summaries <- lapply(numeric_vars, generate_summary, data = externas)
combined_summary <- bind_rows(all_summaries)
combined_summary


# Distribucion variables de interes ---------------------------------------

# variable precio 

# Convertir variable <price> a log para escalar graficos 
bd <- bd%>%
  mutate(log_price=log(price))

# Resumen de la distibución de la variable escalado a $
summary(bd$price) %>%
  as.matrix() %>%
  as.data.frame()%>%
  mutate(V1 = scales::dollar(V1))

# Histograma estandarizado en $ 
dist_precio <- ggplot(bd, aes(x = price)) +
  geom_histogram(fill = "darkblue", alpha = 0.5) +
  labs(x = "price", y = "Cantidad") +
  scale_x_log10(labels = scales::dollar)+
  theme_bw()
ggplotly(dist_precio)

# Relación entre precios y el tipo de propiedad 
ggplot(bd, aes(x = price))+
  geom_freqpoly(aes(color = property_type), binwidth = 5000, linewidth = 0.75) +
  scale_x_continuous(labels = scales::dollar_format()) 

# Relación del precio y el número de habitaciones 
ggplot(bd, aes(x = rooms, y = price)) + 
  geom_boxplot(aes(group = cut_width(rooms, 0.1)))

# Relación del precio y el número de baños 
ggplot(bd, aes(x = bathrooms, y = price)) + 
  geom_boxplot(aes(group = cut_width(bathrooms, 0.1)))

# Visualizar localizacion variables OSM -----------------------------------


# Encontrammos el queremos que sea el centro del mapa  . bd es la base principal
latitud_central  <- mean(bd$lat)
longitud_central <- mean(bd$lon)

# GRAFICAR CORRECTAMENTE , iterando a traves de geometria.osm que tiene los poligonos, y coordenadas.x.centroides
# con coordenadas.y.centroides que tienen las coordenadas de centroides
leaflet() %>%
  addTiles() %>%
  setView(lng = longitud_central, lat = latitud_central, zoom = 12) %>%
  addPolygons(data = geometria.osm[[3]], col = "red",weight = 10,
              opacity = 0.8, popup = geometria.osm[[1]]$name) %>%
  addCircles(lng = coordenadas.x.centroides[[3]], 
             lat = coordenadas.y.centroides[[3]], 
             col = "darkblue", opacity = 0.5, radius = 1)


# Grafica variables distancias --------------------------------------------
# Distribución de la variable 

#restaurante
distancia_restaurantes <- ggplot(bd, aes(x = distancia_restaurante)) +
  geom_histogram(bins = 50, fill = "darkblue", alpha = 0.4) +
  labs(x = "Distancia mínima a un restaurante en metros", y = "Cantidad",
       title = "Distribución de la distancia a restaurantes") +
  theme_bw()
ggplotly(distancia_restaurantes)
#parques
distancia_parques <- ggplot(bd, aes(x = distancia_parques)) +
  geom_histogram(bins = 50, fill = "darkblue", alpha = 0.4) +
  labs(x = "Distancia mínima a un parque en metros", y = "Cantidad",
       title = "Distribución de la distancia a parques") +
  theme_bw()
ggplotly(distancia_parques)
#estaciones
distancia_estaciones <- ggplot(bd, aes(x = distancia_estaciones_tp)) +
  geom_histogram(bins = 50, fill = "darkblue", alpha = 0.4) +
  labs(x = "Distancia mínima a una estación de transporte público en metros", y = "Cantidad",
       title = "Distribución de la distancia a una estación de transporte público") +
  theme_bw()
ggplotly(distancia_estaciones)
#mall
distancia_mall <- ggplot(bd, aes(x = distancia_mall)) +
  geom_histogram(bins = 50, fill = "darkblue", alpha = 0.4) +
  labs(x = "Distancia mínima a un centro comercial en metros", y = "Cantidad",
       title = "Distribución de la distancia a un centro comercial") +
  theme_bw()
ggplotly(distancia_mall)
#ciclovias
distancia_ciclovia <- ggplot(bd, aes(x = distancia_ciclovias)) +
  geom_histogram(bins = 50, fill = "darkblue", alpha = 0.4) +
  labs(x = "Distancia mínima a una ciclovia en metros", y = "Cantidad",
       title = "Distribución de la distancia a una ciclovia") +
  theme_bw()
ggplotly(distancia_ciclovia)
# centro servicios
distancia_servicios<- ggplot(bd, aes(x = distancia_centro_servicios)) +
  geom_histogram(bins = 50, fill = "darkblue", alpha = 0.4) +
  labs(x = "Distancia mínima a centros de servicio en metros", y = "Cantidad",
       title = "Distribución de la distancia a centro de servicios") +
  theme_bw()
ggplotly(distancia_servicios)


