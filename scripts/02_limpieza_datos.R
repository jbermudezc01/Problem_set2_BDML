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

# seleccinamos variables de interes

bd <- bd %>%
  select(-city,-operation_type,-title)

# Identificamos la distribución de las variables de interés

# tipo de propiedad

bd %>% count(property_type)

# descripción de la variable price 

# convertir variable a log para escalar graficos 

bd <- bd%>%
  mutate(log_price=log(price))

# resumen de la distibución de la variable escalado a $

summary(bd$price) %>%
  as.matrix() %>%
  as.data.frame()%>%
  mutate(V1 = scales::dollar(V1))

# hstrograma  estandarizado en $ 

dist_precio <- ggplot(bd, aes(x = price)) +
  geom_histogram(fill = "darkblue", alpha = 0.5) +
  labs(x = "price", y = "Cantidad") +
  scale_x_log10(labels = scales::dollar)+
theme_bw()
ggplotly(dist_precio)

# relación entre precios y el tipo de propiedad 

ggplot(bd, aes(x = price))+
  geom_freqpoly(aes(color = property_type), binwidth = 5000, linewidth = 0.75) +
  scale_x_continuous(labels = scales::dollar_format()) 

# relación del precio y el número de habitaciones 

ggplot(bd, aes(x = rooms, y = price)) + 
  geom_boxplot(aes(group = cut_width(rooms, 0.1)))

# relación del precio y el número de baños 

ggplot(bd, aes(x = bathrooms, y = price)) + 
  geom_boxplot(aes(group = cut_width(bathrooms, 0.1)))


# verificamos valores faltantes 

# Calcular cantidad y porcentaje de NA por columna

resumen_na <- bd %>%
  summarise(across(everything(),
                   list(cantidad_NA = ~sum(is.na(.)),
                        porcentaje_NA = ~mean(is.na(.)) * 100))) %>%
  pivot_longer(everything(),
               names_to = c("variable", ".measure"),
               names_pattern = "(.*)_(.*)")
print(resumen_na, n = Inf)

# tratamos los valores faltantes en las variables roomms y bathrooms

# identificamos la moda de rooms y bathromms
bd %>%
  count(rooms)  
bd %>%
  count(bathrooms)

# Imputamos la moda 
bd <- bd %>%
  mutate(rooms = replace_na(rooms, 3), 
         bathrooms = replace_na(bathrooms, 2))
         

# Tratamiento de la variable description 


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

# variable piso laminado 

bd <- bd %>%
  mutate(piso_laminado = as.numeric(grepl("piso laminado", description)))
bd %>%
  count(piso_laminado)

# transformar variables nuemricas binarias a cetegoricas 

#mutación de factores
bd<-bd %>% 
  mutate(parqueadero=factor(parqueadero,levels=c(0,1),labels=c("no tine","Si tiene")),
         terraza=factor(terraza,levels=c(0,1),labels=c("no tiene", "si tiene")),
         ascensor=factor(ascensor,levels=c(0,1),labels=c("no tiene", "si tiene")),
         deposito=factor(deposito,levels=c(0,1),labels=c("no tiene","si tiene")),
         vigilancia=factor(vigilancia,levels=c(0,1),labels=c("no tiene","si tiene")),
         cocina_integral=factor(cocina_integral,levels=c(0,1),labels=c("no tiene","si tiene")),
         piso_laminado=factor(piso_laminado,levels=c(0,1),labels = c("no tiene", "si tiene")),
         property_type = as.factor(property_type))
                          
# creación de variables espaciales #

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

# Calculamos las distancias para cada inmueble

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

# creamos variables distancia polinomicas 

bd <- bd%>%
  mutate(distancia_parques2 = distancia_parques^2,
         distancia_restaurante2 = distancia_restaurante^2,
         distancia_estaciones_tp2 = distancia_estaciones_tp^2,
         distancia_mall2 = distancia_mall^2,
         distancia_ciclovia2 = distancia_ciclovias^2,
         distancia_servicios2= distancia_centro_servicios^2,
         distancia_parques3 = distancia_parques^3,
         distancia_restaurante3 = distancia_restaurante^3,
         distancia_estaciones_tp3 = distancia_estaciones_tp^3,
         distancia_mall3 = distancia_mall^3,
         distancia_ciclovia3 = distancia_ciclovias^3,
         ditancia_servecios3= distancia_centro_servicios^3,
         distancia_parques4 = distancia_parques^4,
         distancia_restaurante4 = distancia_restaurante^4,
         distancia_estaciones_tp4 = distancia_estaciones_tp^4,
         distancia_mall4 = distancia_mall^4,
         distancia_ciclovia4 = distancia_ciclovias^4,
         ditancia_servecios4= distancia_centro_servicios^4,
         distancia_parques5 = distancia_parques^5,
         distancia_restaurante5 = distancia_restaurante^5,
         distancia_estaciones_tp5 = distancia_estaciones_tp^5,
         distancia_mall5 = distancia_mall^5,
         distancia_ciclovia5 = distancia_ciclovias^5,
         ditancia_servecios5= distancia_centro_servicios^5)

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

write_csv(bd, file = "/Users/apple/Documents/GitHub/Problem_set2_BDML/stores/base_datos_tratada.csv")
