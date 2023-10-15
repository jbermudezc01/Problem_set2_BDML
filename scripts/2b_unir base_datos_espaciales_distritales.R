# creación de nuevos predictores con bases de datos espaciales de Bogotá

## limpieza y transformación de datos ##

#Limpieza area de trabajo 
rm(list=ls())
cat('\014')

# cargar paquetes 
install.packages("pacman")
library(pacman)
# cargar librerias 
p_load(tidyverse, # Manipular dataframes
       plotly, # Gráficos interactivos
       leaflet, # Mapas interactivos
       rgeos, # Calcular centroides de un polígono
       units, # unidades
       sf, # Leer/escribir/manipular datos espaciales
       osmdata, # Obtener datos de OpenStreetMap (OSM)
       tidymodels, # Modelado de datos limpios y ordenados
       rattle, # Interfaz gráfica para el modelado de datos
       spatialsample)

# cargar bases de datos 

setwd("/Users/apple/Desktop/En proceso/Taller 2 /Datos de bogotá /Mapas de bogotá/manz")
manzanas <- st_read("MANZ.shp")

setwd("/Users/apple/Desktop/En proceso/Taller 2 /Datos de bogotá /Mapas de bogotá/Valor_ref_2022")
valor_referencia <- st_read("Valor_ref_2022.shp")

setwd("/Users/apple/Desktop/En proceso/Taller 2 /Datos de bogotá /Mapas de bogotá/AconstruidaResidencial")
area_construida <- st_read("AconstruidaResidencial.shp")

setwd("/Users/apple/Desktop/En proceso/Taller 2 /Datos de bogotá /Mapas de bogotá/CantidadSHP")
cantidad_predios <- st_read("CantidadPM.shp")


df <- read.csv('https://raw.githubusercontent.com/jbermudezc01/Problem_set2_BDML/main/stores/base_datos_tratada.csv')

# convertir base de datos del taller en objeto sf 
 
df <- st_as_sf(bd, coords = c("lon","lat"), crs = 4326)

# identificar el tipo de geometria 

geom_type <- st_geometry_type(cantidad_predios)
print(table(geom_type))

# hacer join espacial 

#  corregir las geometrías inválidas

area_construida_valid <- st_make_valid(area_construida)
valor_referencia_valid <- st_make_valid(valor_referencia)
cantidad_predios_valid <- st_make_valid(cantidad_predios)

# convertir al mismo sistem de coordenadas 


#  realizar la unión 
# area superficie
df <- st_join(df, area_construida_valid, join = st_within)

# valor catastral de refencia 
df <- st_join(df, valor_referencia_valid, join = st_within)

# cantidad de predios
df <- st_join(df,cantidad_predios_valid, join=st_within)

# eliminar los valores duplicados 

# area construida residencial 

df <- df %>% 
  distinct(property_id, geometry, .keep_all = TRUE)


# limpiar base de datos 

df <- df %>%
  select(-c(MANCODIGO.y,MANCODIGO,ANO.x,ANO.y,ANO,
            OBJECTID,OBJECTID.x,OBJECTID.y,SHAPE_AREA.x,SHAPE_Area,
            SHAPE_LEN.x,SHAPE_Leng,SHAPE_AREA.y,SHAPE_LEN.y))

# identificar los NA para las variables agregadas

df %>% 
  summarise(
    na_count_area = sum(is.na(AREA_RESID)), # 37,7 %
    na_count_valor_ref = sum(is.na(V_REF))) # 34,3 %
    
# Imputar NA por la media 
df <- df %>%
  mutate(
    area_residencial_manzana = ifelse(is.na(AREA_RESID), mean(AREA_RESID, na.rm = TRUE), AREA_RESID),
   valor_referencia = ifelse(is.na(V_REF), mean(V_REF, na.rm = TRUE), V_REF),
   numero_predios_manzana = ifelse(is.na(N_PREDIOS), mean(N_PREDIOS, na.rm = TRUE), N_PREDIOS)
  )

df <- df%>%
  mutate(area_superfice_vivienda = area_residencial_manzana/numero_predios_manzana)

# guardar en github 

write_csv(df, file = "/Users/apple/Documents/GitHub/Problem_set2_BDML/stores/base_datos_espaciales_Bogotá.csv")


 