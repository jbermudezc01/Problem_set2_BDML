##########################################################
# Prediccion precios de vivienda
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

# Lectura base de datos ---------------------------------------------------
raw.bd <- read.csv(paste0(stores,'raw_complete_base.csv'))

# identificar variables de bd 
str(raw.bd)

# seleccinamos variables de interes
bd <- raw.bd %>%
  select(-city,-operation_type,-title)

# Tratar la variable description ------------------------------------------
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

# Valores faltantes -------------------------------------------------------
# Calcular cantidad y porcentaje de NA por columna
# Primero vemos la cantidad
apply(bd, 2, function(x) sum(is.na(x)))
# Ahora el porcentaje
apply(bd, 2, function(x) round(sum(is.na(x)/length(x))*100,2))
# Vemos que las variables mas problematicas van a ser <surface_total>, <surface_covered>, <rooms> y <bathrooms>. Por lo que vamos a intentar encontrar maneras para 
# completar los NA.

# En primer lugar, analizando la variable <surface_total> encontramos una propiedad que puede ser problematica en el momento de ver el MAE de prueba
# ya que es una finca de 108.800m2 que se encuentra cerca a Medellin. Se procede a eliminar esta observacion de los datos
bd <- bd[-which.max(bd$surface_total),]

# Para tratar los valores faltantes de <bathrooms> y <rooms> es importante ver que <bedrooms> si esta completa. Por lo que una buena idea seria imputar la moda
# dependiendo del numero de alcobas que tenga el apto/casa.
# Por ejemplo, supongamos que la moda de banos para los apartamentos que tienen 4 alcobas sea 3, entonces esa moda sera imputada por aquellos aptos de 4 alcobas
# que tengan valor faltante.
# Lo anterior se va a realizar para todos los valores de numeros de alcobas, y para las dos variables que faltan, es decir <bathrooms> y <rooms>
moda <- function(x) {
  return(as.numeric(names(which.max(table(x)))))
}

# Imputar variables con la moda 
bd <- bd %>%
  group_by(bedrooms) %>%
  mutate(bathrooms = ifelse(is.na(bathrooms), moda(bathrooms), bathrooms)) %>%
  mutate(rooms = ifelse(is.na(rooms), moda(rooms),rooms)) %>% 
  ungroup()

# Creacion de variables segun <description> -------------------------------
# Variable piso
bd <- bd %>%
  mutate(piso_info= str_extract(description, "(\\w+|\\d+) piso (\\w+|\\d+)"))

# Transformación de número escritos a númericos 
numeros_escritos <- c("uno|primero|primer", "dos|segundo|segund", "tres|tercero|tercer", "cuatro|cuarto", 
                      "cinco|quinto", "seis|sexto", "siete|septimo|sptimo", "ocho|octavo", "nueve|noveno", 
                      "diez|decimo|dei|dcimo",'once','doce','trece','catorce','quince','dieciseis','diecisiete',
                      'diecioho','diecinueve','veinte')
numeros_numericos <- as.character(1:20)
bd<- bd %>%
  mutate(piso_info = str_replace_all(piso_info, setNames(numeros_numericos,numeros_escritos)))

# Extracción de números 
bd<- bd %>%
  mutate(piso_numerico = as.integer(str_extract(piso_info, "\\d+")))

# Limpieza de datos del piso, ya que hay muchos outliers 
bd <- bd %>%
  mutate(piso_numerico = ifelse(piso_numerico > 20, NA, piso_numerico))

# Imputar valores faltantes segun el tipo de propiedad 
bd2 <- bd %>%
  group_by(property_type) %>%
  mutate(piso_numerico = ifelse(is.na(piso_numerico), moda(piso_numerico), piso_numerico)) %>%
  ungroup()

# Variable de parqueadero 
bd <- bd %>%
  mutate(parqueadero = as.numeric(grepl("parqueadero|garaje", description)))
bd %>%
  count(parqueadero)

# variable terraza 
bd <- bd %>%
  mutate(terraza = as.numeric(grepl("terraza|balcon", description, ignore.case = TRUE)))
bd %>%
    count(terraza)
  
# variable Ascensor 
bd <- bd %>%
  mutate(ascensor = as.numeric(grepl("ascensor", description)))
bd %>%
  count(ascensor)

# Variable vigilancia
bd <- bd %>%
  mutate(vigilancia = as.numeric(grepl("seguridad|vigilancia|porteria", description)))
bd %>%
  count(vigilancia)

# Variable deposito
bd <- bd %>%
  mutate(deposito = as.numeric(grepl("bodega|deposito", description, ignore.case = TRUE)))
bd %>%
  count(deposito)

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

# Transformacion de variables ---------------------------------------------

# Transformar variables numericas binarias a categoricas 

#mutación de factores
bd<-bd %>% 
  mutate(parqueadero     = factor(parqueadero,levels=c(0,1),labels=c("No","Si")),
         terraza         = factor(terraza,levels=c(0,1),labels=c("No", "Si")),
         ascensor        = factor(ascensor,levels=c(0,1),labels=c("No", "Si")),
         deposito        = factor(deposito,levels=c(0,1),labels=c("No","Si")),
         vigilancia      = factor(vigilancia,levels=c(0,1),labels=c("No","Si")),
         cocina_integral = factor(cocina_integral,levels=c(0,1),labels=c("No","Si")),
         piso_laminado   = factor(piso_laminado,levels=c(0,1),labels = c("No", "Si")),
         property_type   = as.factor(property_type))
                          
# Creacion Variables espaciales ----------------------------------------------------

# Ubicación geografica de bogotá 
bogota <- opq(bbox = getbb ("Bogotá Colombia"))

# tomamos nuestros datos geoespaciales y los convertimos al formato sf (simple features)
db_sf <- st_as_sf(bd, coords = c("lon", "lat")) 

# Especificamos el sistema de coordenadas
st_crs(db_sf) <- 4326

# Identificar categorias 
available_features() %>% 
  head(500)

# Extraer los datos abiertos para cada categoria 

# Restaurantes
restaurantes <- bogota %>%
  add_osm_feature(key = "amenity", value= "restaurant")%>%
  osmdata_sf()

leaflet() %>%
  addTiles() %>%
  addCircles(lng = as.numeric(unlist(purrr::map(restaurantes$osm_points$geometry, ~.x[1]))), 
             lat = as.numeric(unlist(purrr::map(restaurantes$osm_points$geometry, ~.x[2]))))

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

# unir variables construidas con bases de datos espaciales de bogotá 
# cargar datos de variables espaciales
ve <- read.csv("https://raw.githubusercontent.com/jbermudezc01/Problem_set2_BDML/main/stores/variables_espaciales.csv")
# unir a bd 
df <- merge(bd, ve, by = "property_id")


# exportar a csv

write_csv(df, file = "/Users/apple/Documents/GitHub/Problem_set2_BDML/stores/base_datos_tratada.csv")
