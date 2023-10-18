##########################################################
# Limpieza datos + datos espaciales OSM + datos extraidos de descripcion
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
bd <- bd %>%
  group_by(property_type) %>%
  mutate(piso_numerico = ifelse(is.na(piso_numerico), moda(piso_numerico), piso_numerico)) %>%
  ungroup()

# Variable de parqueadero 
bd <- bd %>%
  mutate(parqueadero = as.numeric(grepl("parqueadero|garaje", description)))

# variable terraza 
bd <- bd %>%
  mutate(terraza = as.numeric(grepl("terraza|balcon", description, ignore.case = TRUE)))

# variable Ascensor 
bd <- bd %>%
  mutate(ascensor = as.numeric(grepl("ascensor", description)))

# Variable vigilancia
bd <- bd %>%
  mutate(vigilancia = as.numeric(grepl("seguridad|vigilancia|porteria", description)))

# Variable deposito
bd <- bd %>%
  mutate(deposito = as.numeric(grepl("bodega|deposito", description, ignore.case = TRUE)))

# variable de cocina integral
bd <- bd %>%
  mutate(cocina_integral = as.numeric(grepl("cocina integral", description)))

# variable piso laminado 
bd <- bd %>%
  mutate(piso_laminado = as.numeric(grepl("piso laminado", description)))

#Variable surface


bd <- bd %>%
  mutate(surface = ifelse(!is.na(surface_covered),surface_covered, 
                          gsub(".*\\s(\\d+)m2.*", "\\1", description)))
bd <- bd %>%
  mutate(surface = ifelse(nchar(surface)>5,
                          gsub(".*\\s(\\d+)\\smt.*", "\\1", description), surface))
bd <- bd %>%
  mutate(surface = ifelse(nchar(surface)>5,
                          gsub(".*\\s(\\d+)\\sm2.*", "\\1", description), surface))
bd <- bd %>%
  mutate(surface = ifelse(nchar(surface)>5, 
                          gsub(".*\\s(\\d+)mt.*", "\\1", description), surface))
bd <- bd %>%
  mutate(surface = ifelse(nchar(surface)>5, 
                          gsub(".*\\s(\\d+)m.*", "\\1", description), surface))
bd <- bd %>%
  mutate(surface = ifelse(nchar(surface)>5, 
                          gsub(".*\\s(\\d+)\\sm.*", "\\1", description), surface))

bd <- bd %>%
  mutate(surface = ifelse(nchar(surface)>5, NA, surface))

bd$surface<-as.numeric(bd$surface)

bd <- bd %>%
  group_by(bedrooms) %>%
  mutate(surface = ifelse(is.na(surface), mean(surface),surface))%>%
  ungroup()
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

# Extraer datos OSM -------------------------------------------------------

datos.osm <- list()
# Restaurantes
datos.osm[[1]] <- bogota %>%
  add_osm_feature(key = "amenity", value= "restaurant")%>%
  osmdata_sf()

# Parques 
datos.osm[[2]] <- bogota %>% 
  add_osm_feature(key = "leisure" , value = "park")%>%
  osmdata_sf()

# Mall
datos.osm[[3]] <-  bogota %>%
  add_osm_feature(key = "shop", value= "mall")%>%
  osmdata_sf()

# Ciclovias 
datos.osm[[4]] <- bogota %>%
  add_osm_feature(key = "highway", value= "cycleway")%>%
  osmdata_sf()

# centro urbanos de servicios
datos.osm[[5]] <- bogota %>%
  add_osm_feature(key = "landuse", value= "commercial")%>%
  osmdata_sf()

names(datos.osm) <- c('Restaurantes','Parques', 'Mall', 'Ciclovias', 'Centro urbanos de servicios')

# Geometria variables OSM -------------------------------------------------
geometria.osm <- lapply(datos.osm, function(x) x$osm_polygons %>% select(osm_id, name))

# Calculamos los centroides
centroides.osm <- lapply(geometria.osm, function(x){
  elementos.st <- st_as_sf(as(x$geometry,'Spatial'))
  centroides   <- st_centroid(elementos.st)
  return(centroides)
})

coordenadas.x.centroides <- lapply(centroides.osm, function(x) unlist(purrr::map(x$geometry, ~.x[1])))
coordenadas.y.centroides <- lapply(centroides.osm, function(x) unlist(purrr::map(x$geometry, ~.x[2])))

# Matrices de distancias para cada observacion a los centroides en <centroides.osm>
matrix.distancias.osm  <- lapply(centroides.osm, function(x) st_distance(x=db_sf, y =x))
  
# Distancias minimas 
distancias.minimas.osm <- lapply(matrix.distancias.osm, function(x) apply(x,1,min))
  
# Agregar las distancias minimas a la base de datos
name.cols <- c('restaurante','parques', 'mall', 'ciclovias', 'centro_servicios')
for(i in seq_along(distancias.minimas.osm)){
  nombre.columna <- paste0('distancia_',name.cols[i])
  bd <- bd %>% mutate(!!nombre.columna := distancias.minimas.osm[[i]])
}

# Transmilenio y SITP ------------------------------------------------------------
# Para los datos de transmilenio y SITP se confia en los datos abiertos de transmilenio, y usamos la API que ofrecen ellos en la pagina oficial
# La API se encuentra en formato geojson por lo que usamos el paquete <geojsonR> y generamos dataframes con las longitudes y latitudes de las estaciones para 
# luego medir la distancia 
transmilenio           <- FROM_GeoJson(url_file_string = "https://gis.transmilenio.gov.co/arcgis/rest/services/Troncal/consulta_estaciones_troncales/FeatureServer/1/query?outFields=*&where=1%3D1&f=geojson")
geometria.transmilenio <- purrr::map_df(transmilenio$features, ~.x$properties[c('nombre_estacion','latitud_estacion','longitud_estacion')])

sitp                   <- FROM_GeoJson(url_file_string = "https://gis.transmilenio.gov.co/arcgis/rest/services/Zonal/consulta_paraderos/MapServer/0/query?outFields=*&where=1%3D1&f=geojson")
geometria.sitp         <- purrr::map_df(sitp$features, ~.x$properties[c('nombre','latitud','longitud')])

# Volvemos a <geometria.transmilenio> y <geometria.sitp> en objetos sf con crs igual a <db_sf>
transmilenio.sf <- st_as_sf(x = geometria.transmilenio, coords = c('longitud_estacion','latitud_estacion'),
                            crs = st_crs(db_sf))
sitp.sf         <- st_as_sf(x = geometria.sitp, coords = c('longitud','latitud'),crs = st_crs(db_sf))

# Matrices de distancias para transmilenio y sitp
matrix.distancias.tm    <- st_distance(x=db_sf, y = transmilenio.sf)
matrix.distancias.sitp  <- st_distance(x=db_sf, y = sitp.sf)

# Distancias minimas y agregar a base de datos
bd$distancia_tm   <- apply(matrix.distancias.tm, 1, min) 
bd$distancia_sitp <- apply(matrix.distancias.sitp, 1, min)

# Variables de distancia polinomicas

bd <- bd%>%
  mutate(distancia_parques2     = distancia_parques^2,
         distancia_restaurante2 = distancia_restaurante^2,
         distancia_sitp2        = distancia_sitp^2,
         distancia_tm2          = distancia_tm^2,
         distancia_mall2        = distancia_mall^2,
         distancia_ciclovias2   = distancia_ciclovias^2,
         distancia_servicios2   = distancia_centro_servicios^2,
         distancia_parques3     = distancia_parques^3,
         distancia_restaurante3 = distancia_restaurante^3,
         distancia_mall3        = distancia_mall^3,
         distancia_ciclovias3   = distancia_ciclovias^3,
         ditancia_servicios3    = distancia_centro_servicios^3,
         distancia_sitp3        = distancia_sitp^3,
         distancia_tm3          = distancia_tm^3,
         distancia_parques4     = distancia_parques^4,
         distancia_restaurante4 = distancia_restaurante^4,
         distancia_mall4        = distancia_mall^4,
         distancia_ciclovias4   = distancia_ciclovias^4,
         ditancia_servicios4    = distancia_centro_servicios^4,
         distancia_sitp4        = distancia_sitp^4,
         distancia_tm4          = distancia_tm^4,
         distancia_parques5     = distancia_parques^5,
         distancia_restaurante5 = distancia_restaurante^5,
         distancia_mall5        = distancia_mall^5,
         distancia_ciclovias5   = distancia_ciclovias^5,
         ditancia_servicios5    = distancia_centro_servicios^5,
         distancia_sitp5        = distancia_sitp^5,
         distancia_tm5          = distancia_tm^5)


# Exportar a csv y .RData -------------------------------------------------
write_csv(bd, file = paste0(stores,'base_datos_tratada.csv'))
save(bd, geometria.osm, coordenadas.x.centroides, coordenadas.y.centroides, file = paste0(stores,'Datos_limpios.RData'))

# En //Datos_limpios.RData van a guardarse la base de datos bd, la geometria.osm, coordenadas.x.centroides y 
# coordenadas.y.centroides