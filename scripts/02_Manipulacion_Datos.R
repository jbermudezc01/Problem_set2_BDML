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
datos.bogota <- paste0(stores,'Bases_Bogota/')

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

# Para tratar los valores faltantes de <bathrooms> y <rooms> es importante ver que <bedrooms> si esta completa. Por lo que una buena idea seria imputar la moda
# dependiendo del numero de alcobas que tenga el apto/casa. Sin embargo, se encuentra que cuando bedrooms es 0 rooms es NA, por lo que se busca una estrategía para extraer el número de 
# alcobas o cuartos

#Ya que no se pudo extraer el número de alcobas de todas las descripciones imputaremos por la moda, 
moda <- function(x) {
  return(as.numeric(names(which.max(table(x)))))
}

if(0){
  #Cuartos/rooms/bedrooms: Extraigamos el número de habitaciones de la descripción 
   bd <- bd %>%
     mutate(bedrooms = ifelse(!is.na(rooms),bedrooms,
                             gsub(".*\\s(\\d+)cuartos.*", "\\1", description))) %>%
     mutate(bedrooms = ifelse(nchar(bedrooms)>5,
                             gsub(".*\\s(\\d+)\\salcobas.*", "\\1", description), bedrooms)) %>%
     mutate(bedrooms = ifelse(nchar(bedrooms)>5,
                             gsub(".*\\s(\\d+)\\shabitaciones.*", "\\1", description), bedrooms)) %>%
     mutate(bedrooms = ifelse(nchar(bedrooms)>5,
                              gsub(".*\\s(\\d+)\\shab.*", "\\1", description), bedrooms)) %>%
     mutate(bedrooms = ifelse(nchar(bedrooms)>5,
                             gsub(".*\\s\\scuartos*", "\\1", description), bedrooms))%>%
     mutate(bedrooms = ifelse(nchar(bedrooms)>5,
                             gsub(".*\\s\\salcobas.*", "\\1", description), bedrooms)) %>%
     mutate(bedrooms = ifelse(nchar(bedrooms)>5,
                             gsub(".*\\s\\habitaciones.*", "\\1", description), bedrooms))

   bd <- bd %>%
     mutate(bedrooms = ifelse(nchar(bedrooms)>5|nchar(bedrooms)==0,"NA", bedrooms))%>%
     mutate(bedrooms = as.numeric(bedrooms))%>%
     mutate(bedrooms = ifelse(!is.na(bedrooms), bedrooms, moda(bedrooms)))
}

# Además al observar la variable de baños encontramos un comportamiento similar, supongamos que la moda de banos para los apartamentos que tienen 4 alcobas sea 3, entonces esa moda sera imputada por aquellos aptos de 4 alcobas
# que tengan valor faltante.
# Lo anterior se va a realizar para todos los valores de numeros de alcobas, y para las dos variables que faltan, es decir <bathrooms> y <rooms>


# Imputar variables con la moda 
bd <- bd %>%
  group_by(bedrooms) %>%
  mutate(bathrooms = ifelse(is.na(bathrooms), moda(bathrooms), bathrooms))%>% 
  ungroup()

# Sin embargo, la variable <rooms> sigue con valores NA,por lo que vamos a imputar por la moda en general
bd <- bd %>% 
  mutate(rooms = ifelse(is.na(rooms),moda(rooms),rooms))

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
  mutate(surface = ifelse(nchar(surface)>5, "NA", surface)) %>%
  mutate(surface=as.numeric(surface))

bd <- bd %>%
  group_by(bedrooms) %>%
  mutate(surface2 = ifelse(is.na(surface), mean(surface, na.rm=TRUE),surface))%>%
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
available_features()

# Extraer datos OSM -------------------------------------------------------

datos.osm1 <- list()

# Mall
datos.osm1[[1]] <-  bogota %>%
  add_osm_feature(key = "shop", value= "mall")%>%
  osmdata_sf()

# Ciclovias 
datos.osm1[[2]] <- bogota %>%
  add_osm_feature(key = "highway", value= "cycleway")%>%
  osmdata_sf()

# centro urbanos de servicios
datos.osm1[[3]] <- bogota %>%
  add_osm_feature(key = "landuse", value= "commercial")%>%
  osmdata_sf()

nombres.datos.osm <- c('mall','cycleway','commercial')
names(datos.osm1) <- nombres.datos.osm

# Alternativa datos OSM iterando ------------------------------------------

# Se va a iterar a traves de todos los objetos en <amenity> y en <leisure>, y 
# se van a guardar los objetos no vacios en una lista. 
amenities <- available_tags('amenity')
leisures   <- available_tags('leisure')
amenities.leisures <- rbind(amenities, leisures)
# Para evitar problemas con el codigo, se renombra el  <Value> de la primera fila, a first_aid_school
amenities.leisures[1,'Value'] <- 'first_aid_school'

datos.osm2 <- list()
names.osm2 <- list()
indice <- 1
for(k in 1:nrow(amenities.leisures)){
  key <- as.character(amenities.leisures[k,'Key'])
  datos.temp <- bogota %>%
    add_osm_feature(key = key, 
                    value= as.character(amenities.leisures[k,'Value']))%>%
    osmdata_sf()
  # Revisar si tiene datos geograficos. El elemento <datos.temp> tiene una matriz de poligonos
  # que se accede con <$osm_polygons>, y si esa matriz tiene 0 filas, no se debe guardar porque no 
  # hay informacion para Bogota
  if(nrow(datos.temp$osm_polygons) == 0) next
  
  # Si tiene al menos un dato entonces el loop continua
  datos.osm2[[indice]] <- datos.temp
  # Aparte, es necesario agregar el nombre del amenity / leisure. La forma mas eficiente para acceder
  # a ello es con el codigo en la siguiente linea
  vgrepl <- Vectorize(grepl, 'pattern')
  nombres.posibles  <- as.character(amenities.leisures$Value)[vgrepl(as.character(amenities.leisures$Value), datos.temp$overpass_call)]
  # En <nombres.posibles> puede haber patrones como 'first_aid_school' y aparte 'school', pero la idea es solamente encontrar la llave unica, que 
  # es aquel valor en <nombres.posibles> con el mayor numero de caracteres
  names.osm2[[indice]] <- nombres.posibles[which.max(nchar(nombres.posibles))]
  indice <- indice + 1
}
# Asignar los nombres
names(datos.osm2) <- names.osm2

# Juntar los dos objetos de datos de OSM, <datos.osm> y <datos.osm2>
datos.osm         <- c(datos.osm1, datos.osm2)
nombres.datos.osm <- names(datos.osm)

# Geometria variables OSM -------------------------------------------------
geometria.osm <- lapply(datos.osm, function(x) x$osm_polygons %>% select(osm_id))

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
for(i in seq_along(distancias.minimas.osm)){
  nombre.columna <- paste0('distancia_',nombres.datos.osm[i])
  bd <- bd %>% mutate(!!nombre.columna := distancias.minimas.osm[[i]])
}

# Transmilenio y SITP ------------------------------------------------------------
# Para los datos de transmilenio y SITP se confia en los datos abiertos de transmilenio, y usamos la API que ofrecen ellos en la pagina oficial
# La API se encuentra en formato geojson por lo que usamos el paquete <geojsonR> y generamos dataframes con las longitudes y latitudes de las estaciones para 
# luego medir la distancia 
transmilenio           <- geojson_read(paste0(stores,'Estaciones_Troncales_de_TRANSMILENIO.geojson'))
geometria.transmilenio <- purrr::map_df(transmilenio$features, ~.x$properties[c('nombre_estacion','latitud_estacion','longitud_estacion')])

sitp            <- geojson_read(paste0(stores,'Paraderos_Zonales_del_SITP.geojson'))
geometria.sitp  <- purrr::map_df(sitp$features, ~.x$properties[c('nombre','latitud','longitud')])

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

# Bases de datos de Bogota ------------------------------------------------

manzanas         <- st_read(paste0(datos.bogota,'manzanas_bogota/MANZ.shp')) # De manzanas
valor_referencia <- st_read(paste0(datos.bogota,'valor_catastral_referencia_2022_bogota/Valor_ref_2022.shp')) # Valor catastral de referencia 
area_construida  <- st_read(paste0(datos.bogota,'Area_construida_residencial_manzana_bogota/AconstruidaResidencial.shp')) # Area construida residencial 
cantidad_predios <- st_read(paste0(datos.bogota,'cantidad_predios_manzana_bogota/CantidadPM.shp')) # Cantidad de predios 
localidades      <- st_read(paste0(datos.bogota,'localidades_bogota/Loca.shp')) # Localidades
upz              <- st_read(paste0(datos.bogota,'Espacio público verde por UPZ/geo_export_45102fe6-a33e-449a-a508-84ddb3705a97.shp'))

# Convertir base de datos del taller en objeto sf 
bd <- st_as_sf(bd, coords = c("lon","lat"), crs = 4326)

# Identificar el tipo de geometria 
geom_type <- st_geometry_type(localidades)
print(table(geom_type))

# Joint espacial ----------------------------------------------------------
# Corregir las geometrias invalidas
area_construida_valid  <- st_make_valid(area_construida)
valor_referencia_valid <- st_make_valid(valor_referencia)
cantidad_predios_valid <- st_make_valid(cantidad_predios)
manzanas_valid         <- st_make_valid(manzanas)
localidades_valid      <- st_make_valid(localidades)
upz_valid              <- st_make_valid(upz)
upz_valid              <- st_transform(upz_valid, crs(bd))

# Realizar la union
# Area superficie
bd <- st_join(bd, area_construida_valid, join = st_within)

# valor catastral de refencia 
bd <- st_join(bd, valor_referencia_valid, join = st_within)

# Cantidad de predios
bd <- st_join(bd,cantidad_predios_valid, join=st_within)

# Convertir a sistema de coordenadas de bd 
bd <- st_transform(bd, crs = st_crs(localidades_valid))

# Localidad
bd <- st_join(bd,localidades_valid,join = st_within)

# UPZ
upz_valid <- st_transform(upz_valid, crs(bd))
bd        <- st_join(bd, upz_valid, join = st_within)

# Eliminar los valores que quedaron duplicados 
bd <- bd %>% 
  distinct(property_id, geometry, .keep_all = TRUE)

# Limpiar base de datos 
bd <- bd %>%
  select(-c(OBJECTID,OBJECTID.x,OBJECTID.y,MANCODIGO.x,MANCODIGO.y,
            ANO.x,ANO.y,ANO,SHAPE_AREA.x,SHAPE_LEN.x,SHAPE_Area.x,SHAPE_Leng.x,
            SHAPE_AREA.y,SHAPE_LEN.y,SHAPE_Leng.y,SHAPE_Area.y,LocAAdmini,LocArea))

# Renombrar variables 
bd <- bd%>%
  rename(area_residencial_manzana = AREA_RESID,
         valor_catastral_referencia_2022 = V_REF,
         codigo_manzana = MANCODIGO,
         numero_predios_manzana = N_PREDIOS,
         nombre_localidad = LocNombre,
         codigo_localidad = LocCodigo,
         UPZ = name)

# Transformar a factor variable localidad 
bd <- bd%>%
  mutate(nombre_localidad = as.factor(nombre_localidad))

# Identificar los NA para las variables agregadas
bd %>% 
  summarise(
    na_count_area = sum(is.na(area_residencial_manzana)), # 37,7 %
    na_count_valor =sum(is.na(valor_catastral_referencia_2022)),
    na_count_predios = sum(is.na(numero_predios_manzana)), # 33,4 %
    na_count_localidad =sum(is.na(codigo_localidad))) 

# Imputar variables con manzans de la misma UPZ/localidad -----------------------------
# La idea va a ser la siguiente: para cada observacion NA de cualqueira de estas tres variables:
# <area_residencial_manzana>, <valor_catastral_referencia_2022>, <numero_predios_manzana>, se va a imputar
# por la media de la UPZ
bd <- bd %>%
  group_by(UPZ) %>%
  mutate(area_residencial_manzana        = ifelse(is.na(area_residencial_manzana), mean(area_residencial_manzana, na.rm=TRUE),area_residencial_manzana))%>%
  mutate(valor_catastral_referencia_2022 = ifelse(is.na(valor_catastral_referencia_2022), mean(valor_catastral_referencia_2022, na.rm=TRUE),valor_catastral_referencia_2022)) %>% 
  mutate(numero_predios_manzana          = ifelse(is.na(numero_predios_manzana), mean(numero_predios_manzana, na.rm=TRUE),numero_predios_manzana)) %>% 
  ungroup()

# Por ultimo, para aquellos que todavia son NA, podemos imputar la media a un nivel mas grande, es decir por la localidad a donde pertenecen
bd <- bd %>%
  group_by(nombre_localidad) %>%
  mutate(area_residencial_manzana        = ifelse(is.na(area_residencial_manzana), mean(area_residencial_manzana, na.rm=TRUE),area_residencial_manzana))%>%
  mutate(valor_catastral_referencia_2022 = ifelse(is.na(valor_catastral_referencia_2022), mean(valor_catastral_referencia_2022, na.rm=TRUE),valor_catastral_referencia_2022)) %>% 
  mutate(numero_predios_manzana          = ifelse(is.na(numero_predios_manzana), mean(numero_predios_manzana, na.rm=TRUE),numero_predios_manzana)) %>% 
  ungroup()

# crear variable area construida_residencial_predio 

bd <- bd%>%
  mutate(area_construida_residencial_predio = area_residencial_manzana/numero_predios_manzana)

# aproximar area_residencial_vivienda con la variable area construida_residencual_predio 

# Crear una función llamada reasignar_valores
reasignar_valores <- function(bd) {
  # Definir las nuevas asignaciones para cada localidad
  nuevas_asignaciones <- c(USAQUEN = 66, CHAPINERO = 65, SANTA_FE = 72, SAN_CRISTOBAL = 134,
                           USME = 113, TUNJUELITO = 166, BOSA = 104, KENNEDY = 99, FONTIBON = 71,
                           ENGATIVA = 106, SUBA = 73, BARRIOS_UNIDOS = 117, TEUSAQUILLO = 78,
                           LOS_MARTIRES = 105, ANTONIO_NARINO = 166, PUENTE_ARANDA = 157,
                           LA_CANDELARIA = 85, RAFAEL_URIBE_URIBE = 142, CIUDAD_BOLIVAR = 112)
  
  # Reasignar los valores basados en los nombres de las localidades y el criterio en area_construida_residencial_predio
  bd <- bd %>%
    mutate(area_construida_residencial_predio = ifelse(area_construida_residencial_predio > 216 & 
                                                         nombre_localidad %in% names(nuevas_asignaciones),
                                                       nuevas_asignaciones[nombre_localidad],
                                                       area_construida_residencial_predio))
  
  # Retornar el dataframe modificado
  return(bd)
}

# ajustar en el data frame 
bd <- reasignar_valores(bd)

bd <- bd%>%
  mutate(valor_catastral_vivienda = area_construida_residencial_predio*valor_catastral_referencia_2022)

# seleccionar base de datos 
# Quitar codigo_manzana, codigo_localidad, descrptio, 
bd <- bd%>%
  select(-c('codigo_manzana','codigo_localidad','descriptio'))


# Ultimos detalles antes de estimacion ------------------------------------
# La variable dependiente sera log(price)
bd <- bd %>% 
  mutate(log_price = log(price))

# Variable UPZ es factor
bd <- bd %>% 
  mutate(UPZ = as.factor(UPZ))

# Exportar a csv y .RData -------------------------------------------------
write_csv(bd, file = paste0(stores,'base_datos_tratada.csv'))
save(bd, geometria.osm, coordenadas.x.centroides, coordenadas.y.centroides, file = paste0(stores,'Datos_limpios.RData'))

# En //Datos_limpios.RData van a guardarse la base de datos bd, la geometria.osm, coordenadas.x.centroides y 
# coordenadas.y.centroides