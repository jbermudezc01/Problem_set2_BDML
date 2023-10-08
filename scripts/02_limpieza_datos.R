##########################################################
# Prediccion precios de vivienda
# Autores: Juan Pablo Bermudez. Lina Bautista. Esteban Meza. Pharad Sebastian Escobar
##########################################################

## limpieza y transformación de datos ##

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
       xgboost,
       scals) # Muestreo espacial para modelos de aprendizaje automático

# cargar base de datos 

bd <- read.csv('https://raw.githubusercontent.com/jbermudezc01/Problem_set2_BDML/main/stores/bd.csv')
View(bd)

# identificar variables de bd 
str(bd)

# identificar las observaciones para las categorias casa y apartamento 
bd %>%
  count(property_type)

# verificamos valores faltantes 

# Calcular cantidad y porcentaje de NA por columna

resumen_na <- bd %>%
  summarise(across(everything(),
                   list(cantidad_NA = ~sum(is.na(.)),
                        porcentaje_NA = ~mean(is.na(.)) * 100))) %>%
  pivot_longer(everything(),
               names_to = c("variable", ".measure"),
               names_pattern = "(.*)_(.*)")

# Ver los resultados
print(resumen_na, n = Inf)

# tratamos las variables con valores faltantes: romms, bathrooms, surface_total y surface_covered

# tratamiento de la variable surface_total con el objetivo de analizar la distribución espacial de los datos

bd <- bd %>%
  mutate(surface_total = replace_na(mean(surface_total)),
         surface_covered = replace_na(mean(surface_covered)))

# identificamos la moda de rooms y bathromms
bd %>%
  count(rooms)  
bd %>%
  count(bathrooms)

# Imputamos la moda 
bd <- bd %>%
  mutate(rooms = replace_na(rooms, 3), 
         bathrooms = replace_na(bathrooms, 2))
         

# tratamiento de la variable precio 

# descrición y escalamiento de la variable precio
summary(bd$price) %>%
  as.matrix() %>%
  as.data.frame()%>%
  mutate(V1 = scales::dollar(V1)) 

# calculamos el precio por m2
bd <- bd %>%
  mutate(precio_por_mt2 = round(price / surface_total, 0))

# descrición y escalamiento de la variable precio por m2
summary(bd$precio_por_mt2) %>%
  as.matrix() %>%
  as.data.frame()%>%
  mutate(V1 = scales::dollar(V1)) 

# visualizamos la distribución de nuestra variable de interés 
pm <- ggplot(bd, aes(x = precio_por_mt2)) +
  geom_histogram(fill = "darkblue", alpha = 0.5) +
  labs(x = "Valor de venta (log-scale)", y = "Cantidad") +
  scale_x_log10(labels = scales::dollar)
  theme_bw()
ggplotly(pm)

# tratar valores atipicos de la variable precio que pueden afectar predicción 
bd <- bd %>%
  filter(between(precio_por_mt2, 1000000, 15e6) | is.na(precio_por_mt2))


# visualizamos la distribución de la variable precio 
 p <- ggplot(bd, aes(x = price)) +
  geom_histogram(fill = "darkblue", alpha = 0.4) +
  labs(x = "Valor de venta (log-scale)", y = "Cantidad") +
  scale_x_log10(labels = scales::dollar) +
  theme_bw()
ggplotly(p)


### analizar el patrón espacial ###

# distribución de los puntos 

leaflet() %>%          # Inicia la creación de un mapa leaflet
  addTiles() %>%       # Añade las teselas (tiles) básicas al mapa (esto podría ser un mapa base de OpenStreetMap, por ejemplo)
  addCircles(          # Añade círculos al mapa en las coordenadas especificadas
    lng = bd$lon,      # Define la longitud de los círculos utilizando la columna 'lon' del dataframe 'db'
    lat = bd$lat       # Define la latitud de los círculos utilizando la columna 'lat' del dataframe 'db'
  )

# creamos un mapa mas preciso con las siguientes caracateristicas: 

# 1. Vamos a utilizar el color del marcador para designar si es casa o apartamento
# 2. definir el tamaño del marcador según el valor del metro cuadrado
# 3. vamos a crear un pop-up
# 4. vamos a ajustar el zoom y centrar el mapa

# Escalamos para que se pueda graficar, utilizando min max sclaing 

bd <- bd %>%
  mutate(precio_por_mt2_sc = (precio_por_mt2 - min(precio_por_mt2, na.rm = TRUE)) / 
           (max(precio_por_mt2, na.rm = TRUE) - min(precio_por_mt2, na.rm = TRUE)))

# creamos variable color que depende de apto o casa 

bd <- bd %>%
  mutate(color = case_when(property_type == "Apartamento" ~ "#2A9D8F",
                           property_type == "Casa" ~ "#F4A261"))

# Encontram,os el queremos que sea el centro del mapa 
latitud_central <- mean(bd$lat)
longitud_central <- mean(bd$lon)

# Creamos el plot
leaflet() %>% # inicia la creacion de un mapa leaflet
  addTiles() %>% # añade las capas basicas del mapa : calles, edificios, etc
  setView(lng = longitud_central, lat = latitud_central, zoom = 12) %>% # centra el mapa en las coordendas medias de lon y lat
  addCircles(lng = bd$lon, # añade circulos al mapa en las coordenas de capa inmueble 
             lat = bd$lat, 
             col = bd$color, # define el color de los circulos según la variable color 
             fillOpacity = 1, # Define la opacidad del relleno de los círculos como 1 (completamente opacos)
             opacity = 1, # Define la opacidad del borde de los círculos como 1 (completamente opacos)
             radius = bd$precio_por_mt2_sc*10) # Define el radio de los círculos basándose en la columna precio_por_mt2_sc del dataframe db, escalado por un factor de 10
             
# Tratamiento de la variables descripción 

# normalizar el texto 
# Todo en minuscula
bd <- bd %>%
  mutate(description = str_to_lower(description))
# Eliminamos tildes
bd <- bd %>%
  mutate(description = iconv(description, from = "UTF-8", to = "ASCII//TRANSLIT"))
# Eliminamos caracteres especiales
bd <- bd %>%
  mutate(description = str_replace_all(description, "[^[:alnum:]]", " "))
# Eliminamos espacios extras
bd <- bd %>%
  mutate(description = str_trim(gsub("\\s+", " ", description)))

# creamos variables a partir del texto 

# variable parqueadero 
bd <- bd %>%
  mutate(parqueadero = as.numeric(grepl("parqueadero", description)))
bd %>%
  count(parqueadero)

# variable terraza 
  bd <- bd %>%
  mutate(terraza = as.numeric(grepl("terraza|balcon", description, ignore.case = TRUE)))
  bd %>%
    count(terraza)
  
# variable ascensor 
bd <- bd %>%
  mutate(ascensor = as.numeric(grepl("ascensor", description)))
bd %>%
  count(ascensor)
bd %>%
  count(vigilancia)

# variable deposito
bd <- bd %>%
  mutate(deposito = as.numeric(grepl("bodega|deposito", description, ignore.case = TRUE)))
bd %>%
  count(deposito)

# variable vigilancia 
bd <- bd %>%
  mutate(vigilancia = as.numeric(grepl("vigilancia|porteria", description, ignore.case = TRUE)))
bd %>%
  count(vigilancia)

# variable de cocina integral
bd <- bd %>%
  mutate(cocina_integral = as.numeric(grepl("cocina integral", description)))
bd %>%
  count(cocina_integral)


# creación de variables espaciale #

# definir la ubicación geografica de bogotá 

bogota <- opq(bbox = getbb ("Bogotá Colombia"))
bogota

# tomamos nuestros datos geoespaciales y los convertimos al formato sf (simple features)
db_sf <- st_as_sf(bd, coords = c("lon", "lat")) 

# Especificamos el sistema de coordenadas
st_crs(db_sf) <- 4326

# identificar categorias 

available_features() %>% 
  head(500)

# extraer los datos abiertos para cada categoria 

# restaurantes
restaurantes <- bogota %>%
  add_osm_feature(key = "amenity", value= "restaurant")%>%
  osmdata_sf()
#parques 
parques <- opq( 
  bbox = getbb("Bogotá Colombia")) %>% 
  add_osm_feature(key = "leisure" , value = "park")%>%
  osmdata_sf()
# estaciones de buses
estaciones <- bogota%>%
  add_osm_feature(key = "public_transport", value= "platform")%>%
  osmdata_sf()
# mall
mall <-  bogota%>%
  add_osm_feature(key = "shop", value= "mall")%>%
  osmdata_sf()
#ciclovias 
ciclovias <- bogota%>%
  add_osm_feature(key = "highway", value= "cycleway")%>%
  osmdata_sf()
# centro urbanos de servicios
centro_servicios <- bogota %>%
  add_osm_feature(key = "landuse", value= "commercial")%>%
  osmdata_sf()

# visulizar su localización 

# crear puntos

#restaurantes
puntos_restaurantes <- restaurantes$osm_points
head(puntos_restaurantes)
#parques
puntos_parques <- parques$osm_points
head(puntos_parques)
#estaciones
puntos_estaciones <-estaciones$osm_points
head(puntos_estaciones)
#mall
puntos_mall <- mall$osm_points
head(puntos_mall)
#ciclovia
puntos_ciclovia <- ciclovias$osm_points
head(puntos_ciclovia)
# centros_servicios
puntos_servicios <- centro_servicios$osm_points
head(puntos_servicios)

# crear grafico de localización 

#restuarantes
ggplot()+
  geom_sf(data=puntos_restaurantes)+
  theme_bw()
#parques
ggplot()+
  geom_sf(data=puntos_parques)+
  theme_bw()
#estaciones
ggplot()+
  geom_sf(data=puntos_estaciones)+
  theme_bw()
#mall
ggplot()+
  geom_sf(data=puntos_mall)+
  theme_bw()
#ciclovias
ggplot()+
  geom_sf(data=puntos_ciclovia)+
  theme_bw()
#servicios
ggplot()+
  geom_sf(data=puntos_servicios)+
  theme_bw()

# rescatamos la geometria 

# restaurantes
restaurantes_geometria <- restaurantes$osm_polygons%>%
  select(osm_id, name)
#parques
parques_geometria <- parques$osm_polygons%>%
  select(osm_id, name)
#estaciones
estaciones_geometria <- estaciones$osm_polygons%>%
  select(osm_id, name)
#mall
mall_geometria <- mall$osm_polygons%>%
  select(osm_id, name)
#ciclovias
ciclovia_geometria <- ciclovias$osm_polygons%>%
  select(osm_id, name)
#servicios
servicios_geometria <- centro_servicios$osm_polygons%>%
  select(osm_id, name)

# calculamos el centroide 

#restaurantes
centroides_restaurantes <- gCentroid( 
  as(restaurantes_geometria$geometry, "Spatial"),  
  byid = T)
#parques
centroides_parques <- gCentroid( 
  as(parques_geometria$geometry, "Spatial"),  
  byid = T)
#estaciones 
centroides_estaciones <- gCentroid( 
  as(estaciones_geometria$geometry, "Spatial"),  
  byid = T)
#mall
centroides_mall <- gCentroid( 
  as(mall_geometria$geometry, "Spatial"),  
  byid = T)
#ciclovias
centroides_ciclovias<- gCentroid( 
  as(ciclovia_geometria$geometry, "Spatial"),  
  byid = T)
#servicios 
centroides_servicios <- gCentroid( 
  as(servicios_geometria$geometry, "Spatial"),  
  byid = T)

# convertimos los centroides a formato sf(simple features)

#restaurantes
centroides_restaurantes_sf <- st_as_sf(centroides_restaurantes, coords = c("x", "y")) 
#parques
centroides_parques_sf <- st_as_sf(centroides_parques, coords = c("x", "y")) 
#estaciones
centroides_estaciones_sf <- st_as_sf(centroides_estaciones, coords = c("x", "y")) 
#mall
centroides_mall_sf <- st_as_sf(centroides_mall, coords = c("x", "y")) 
#ciclovias
centroides_ciclovias_sf <- st_as_sf(centroides_ciclovias, coords = c("x", "y"))
#servicios
centroides_servicios_sf <- st_as_sf(centroides_servicios, coords = c("x", "y"))

# Calculamos las diatnacias para cada inmueble

#restaurantes
dist_restaurante<- st_distance(x = db_sf, y = centroides_restaurantes_sf)
#parques 
dist_parques <- st_distance(x = db_sf, y = centroides_parques_sf)
#estaciones 
dist_estaciones <- st_distance(x = db_sf, y = centroides_estaciones_sf)
#mall
dist_mall <- st_distance(x = db_sf, y = centroides_mall_sf)
#ciclovias
dist_ciclovia <- st_distance(x = db_sf, y = centroides_ciclovias_sf)
#servicios
dist_servicios <- st_distance(x = db_sf, y = centroides_servicios_sf)

# Encontramos la distancia mínima a un parque

#restaurantes
dist_min_restaurante<- apply( 
  dist_restaurante, 1, 
  min) 
#parques
dist_min_parque<- apply( 
  dist_parques, 1, 
  min)
#estaciones
dist_min_estaciones <- apply( 
  dist_estaciones, 1, 
  min)
#mall
dist_min_mall <- apply( 
  dist_mall, 1, 
  min)
#ciclovias
dist_min_ciclovias <- apply( 
  dist_ciclovia, 1, 
  min)
#servicios
dist_min_servicios <- apply(
  dist_servicios, 1,
  min)

# La agregamos como variablea nuestra base de datos original 

bd <- bd %>% 
  mutate(distancia_restaurante = dist_min_restaurante,
         distancia_parques = dist_min_parque,
         distancia_estaciones_tp = dist_min_estaciones,
         distancia_mall= dist_min_mall,
         distancia_ciclovias =dist_min_ciclovias,
         distancia_centro_servicios = dist_min_servicios)

# distribución de la variable 

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


# exportar a csv
write_csv(bd, "base_datos_tratada.csv")
