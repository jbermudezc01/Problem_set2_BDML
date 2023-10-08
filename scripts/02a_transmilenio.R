# Creacion variable geografica de transmilenio. Falta integrarla con el codigo prinicipal

library(geojsonR)
library(tidyverse)

transmilenio = FROM_GeoJson(url_file_string = "https://gis.transmilenio.gov.co/arcgis/rest/services/Troncal/consulta_estaciones_troncales/FeatureServer/1/query?outFields=*&where=1%3D1&f=geojson")
geometria.transmilenio <- purrr::map_df(transmilenio$features, ~.x$properties[c('nombre_estacion','latitud_estacion','longitud_estacion')])
