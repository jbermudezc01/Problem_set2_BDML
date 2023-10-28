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
       htmlwidgets) 

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
latitud_central  <- mean(raw.csv$lat)
longitud_central <- mean(raw.csv$lon)

# Iterar a traves de geometria.osm que tiene los poligonos, y coordenadas.x.centroides
# con coordenadas.y.centroides que tienen las coordenadas de centroides
plot <- leaflet() %>%
  addTiles() %>%
  setView(lng = longitud_central, lat = latitud_central, zoom = 11.6) %>%
  addPolygons(data = geometria.osm[[3]], col = "red",weight = 10,
              opacity = 0.8, popup = geometria.osm[[1]]$name) %>%
  addCircles(lng = coordenadas.x.centroides[[3]], 
             lat = coordenadas.y.centroides[[3]], 
             col = "darkblue", opacity = 0.5, radius = 1)
# Guardar el archivo
saveWidget(plot, paste0(views,'html_temp/plot.html'))
webshot(paste0(views,'html_temp/plot.html'), file = "leaflet_map.png",zoom = 2)
