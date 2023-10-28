##########################################################
# Analisis descriptivo
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
       webshot,
       htmlwidgets,
       geojsonR,
       geojsonio) 

# Directorios -------------------------------------------------------------
stores    <- paste0(getwd(),'/stores/') # Directorio de base de datos
views     <- paste0(getwd(),'/views/')  # Directorio para guardar imagenes
templates <- paste0(getwd(),'/templates/') # Directorio para crear templates

# Cargar base de datos ----------------------------------------------------
load(paste0(stores,'Datos_limpios.RData'))


# Histograma de variables de interes --------------------------------------
# Del precio
plot.precio <-bd %>% 
  dplyr::filter(!is.na(price)) %>% 
  ggplot(aes(x = price)) +
  geom_histogram(fill = "darkblue", alpha = 0.5,color='black') +
  labs(x = "Precio", y = "Frecuencia") +
  scale_x_continuous(labels = unit_format(unit='',scale=1e-6),
                     breaks = seq(min(bd$price,na.rm=T),max(bd$price,na.rm=T),by=135000000))+
  ggtitle("Histograma de los precios (en millones de pesos)")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5, size = 16))
# Guardar el grafico en la carpeta <Views>
ggsave(paste0(views,'graficas_descriptivas/histograma_precio.png'), plot.precio, width = 8, height = 6, units = "in")

# Histogramas distancia ---------------------------------------------------
variables.distancia <- colnames(bd)[grepl('distancia',colnames(bd))]
histogramas.distancia <- lapply(variables.distancia, function(name){
  histograma.distancia <- bd %>% 
    ggplot(aes(x=!!sym(name))) +
    geom_histogram(fill='darkblue', alpha=0.5, color='black') +
    labs(x= paste0(str_to_title(unlist(str_split(name,'_'))),collapse=' '),
         y = 'Frecuencia') +
    ggtitle(paste0('Histograma de ', paste0(str_to_title(unlist(str_split(name,'_'))),collapse=' '))) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, size =16))
  ggsave(paste0(views,'graficas_descriptivas/histograma_',name,'.png'), histograma.distancia, width = 8, height = 6 , units='in')
})

# Graficas mapas ----------------------------------------------------------
# Encontrammos el queremos que sea el centro del mapa  . bd es la base principal
raw.csv <- read_csv(paste0(stores,"raw_complete_base.csv"))
latitud_central  <- as.numeric(quantile(raw.csv$lat, 0.30))
longitud_central <- as.numeric(quantile(raw.csv$lon, 0.1))

# Iterar a traves de geometria.osm que tiene los poligonos, y coordenadas.x.centroides
# con coordenadas.y.centroides que tienen las coordenadas de centroides
mapas <- lapply(names(geometria.osm), function(name){
  map <- leaflet() %>% 
    addTiles() %>% 
    setView(lng=longitud_central,lat=latitud_central, zoom =12) %>% 
    #addPolygons(data = geometria.osm[[name]], col='red', weight = 10,
    #            opacity = 0.8) %>% 
    addCircles(lng = coordenadas.x.centroides[[name]],
               lat = coordenadas.y.centroides[[name]],
               col = 'darkblue', opacity = 1, radius = 2, fill = 'darkblue')
  # Guardar el archivo primero en html
  saveWidget(map, paste0(views, 'html_temp/',name,'.html'))
  # Ahora en png
  webshot(paste0(views, 'html_temp/',name,'.html'), file = paste0(views, 'mapas/',name,'.png'))
})

# Falta agregar los mapas de la geografia de transmilemio 
# Para los datos de transmilenio y SITP se confia en los datos abiertos de transmilenio, y usamos la API que ofrecen ellos en la pagina oficial
# La API se encuentra en formato geojson por lo que usamos el paquete <geojsonR> y generamos dataframes con las longitudes y latitudes de las estaciones para 
# luego medir la distancia 
transmilenio           <- FROM_GeoJson(url_file_string = "https://gis.transmilenio.gov.co/arcgis/rest/services/Troncal/consulta_estaciones_troncales/FeatureServer/1/query?outFields=*&where=1%3D1&f=geojson")
geometria.transmilenio <- purrr::map_df(transmilenio$features, ~.x$properties[c('nombre_estacion','latitud_estacion','longitud_estacion')])

sitp            <- geojson_read(paste0(stores,'Paraderos_Zonales_del_SITP.geojson'))
geometria.sitp  <- purrr::map_df(sitp$features, ~.x$properties[c('nombre','latitud','longitud')])

# Mapa transmilenio
mapa.tm <- leaflet() %>% 
  addTiles() %>% 
  setView(lng=longitud_central,lat=latitud_central, zoom =12) %>% 
  #addPolygons(data = geometria.osm[[name]], col='red', weight = 10,
  #            opacity = 0.8) %>% 
  addCircles(lng = geometria.transmilenio$longitud_estacion,
             lat = geometria.transmilenio$latitud_estacion,
             col = 'darkblue', opacity = 1, radius = 2, fill = 'darkblue')
# Guardar el archivo primero en html
saveWidget(mapa.tm, paste0(views, 'html_temp/transmilenio.html'))
# Ahora en png
webshot(paste0(views, 'html_temp/transmilenio.html'), file = paste0(views, 'mapas/transmilenio.png'))

# Mapa sitp
mapa.sitp <- leaflet() %>% 
  addTiles() %>% 
  setView(lng=longitud_central,lat=latitud_central, zoom =12) %>% 
  #addPolygons(data = geometria.osm[[name]], col='red', weight = 10,
  #            opacity = 0.8) %>% 
  addCircles(lng = geometria.sitp$longitud,
             lat = geometria.sitp$latitud,
             col = 'darkblue', opacity = 1, radius = 2, fill = 'darkblue')
# Guardar el archivo primero en html
saveWidget(mapa.sitp, paste0(views, 'html_temp/sitp.html'))
# Ahora en png
webshot(paste0(views, 'html_temp/sitp.html'), file = paste0(views, 'mapas/sitp.png'))
