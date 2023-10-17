# Al final del codigo sale una base de datos juntada con la que sale de 02_limpieza_datos.R
if(0){
  # unir variables construidas con bases de datos espaciales de bogotá 
  # cargar datos de variables espaciales
  ve <- read.csv("https://raw.githubusercontent.com/jbermudezc01/Problem_set2_BDML/main/stores/variables_espaciales.csv")
  # unir a bd 
  df <- merge(bd, ve, by = "property_id")
}


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

# cargar bases de datos espaciales de bogota 

# Cargar datos espaciales 

#manzanas
setwd("/Users/apple/Documents/GitHub/Problem_set2_BDML/stores/Bases de datos Bogotá/manzanas_bogota")
manzanas <- st_read("MANZ.shp")

# valor catastral de referencia 
setwd("/Users/apple/Documents/GitHub/Problem_set2_BDML/stores/Bases de datos Bogotá/valor_catastral_referencia_2022_bogota")
valor_referencia <- st_read("Valor_ref_2022.shp")

#area construida residencial 
setwd("/Users/apple/Documents/GitHub/Problem_set2_BDML/stores/Bases de datos Bogotá/Area_construida_residencial_manzana_bogota")
area_construida <- st_read("AconstruidaResidencial.shp")

#cantidad de predios 
setwd("/Users/apple/Documents/GitHub/Problem_set2_BDML/stores/Bases de datos Bogotá/cantidad_predios_manzana_bogota")
cantidad_predios <- st_read("CantidadPM.shp")

#localidades
setwd("/Users/apple/Documents/GitHub/Problem_set2_BDML/stores/Bases de datos Bogotá/localidades_bogota")
localidades <- st_read("Loca.shp")

# base de datos taller 
df <- read.csv('https://raw.githubusercontent.com/jbermudezc01/Problem_set2_BDML/main/stores/bd.csv')


# convertir base de datos del taller en objeto sf 
 
df <- st_as_sf(df, coords = c("lon","lat"), crs = 4326)

# identificar el tipo de geometria 

geom_type <- st_geometry_type(localidades)
print(table(geom_type))

# hacer join espacial 

#  corregir las geometrías inválidas

area_construida_valid <- st_make_valid(area_construida)
valor_referencia_valid <- st_make_valid(valor_referencia)
cantidad_predios_valid <- st_make_valid(cantidad_predios)
manzanas_valid <- st_make_valid(manzanas)
localidades_valid <- st_make_valid(localidades)


#  realizar la unión 

# area superficie
df <- st_join(df, area_construida_valid, join = st_within)

# valor catastral de refencia 
df <- st_join(df, valor_referencia_valid, join = st_within)

# cantidad de predios
df <- st_join(df,cantidad_predios_valid, join=st_within)

# localidad 
# convertir a sistema de coordenadas de df 
df <- st_transform(df, crs = st_crs(localidades_valid))
#unir base de datos 
df <- st_join(df,localidades_valid,join = st_within)

# eliminar los valores duplicados 
df <- df %>% 
  distinct(property_id, geometry, .keep_all = TRUE)

# limpiar base de datos 

df <- df %>%
  select(-c(OBJECTID,OBJECTID.x,OBJECTID.y,MANCODIGO.x,MANCODIGO.y,
            ANO.x,ANO.y,ANO,SHAPE_AREA.x,SHAPE_LEN.x,SHAPE_Area.x,SHAPE_Leng.x,
            SHAPE_AREA.y,SHAPE_LEN.y,SHAPE_Leng.y,SHAPE_Area.y,LocAAdmini,LocArea))

# renombrar variables 

df <- df%>%
  rename(area_residencial_manzana = AREA_RESID,
         valor_catastral_referencia_2022 = V_REF,
         codigo_manzana = MANCODIGO,
         numero_predios_manzana = N_PREDIOS,
         nombre_localidad = LocNombre,
         codigo_localidad = LocCodigo)

# transformar a factor variable localidad 
df <- df%>%
mutate(nombre_localidad = as.factor(nombre_localidad))


# identificar los NA para las variables agregadas

df %>% 
  summarise(
    na_count_area = sum(is.na(area_residencial_manzana)), # 37,7 %
    na_count_valor =sum(is.na(valor_catastral_referencia_2022)),
    na_count_predios = sum(is.na(numero_predios_manzana)), # 33,4 %
    na_count_localidad =sum(is.na(codigo_localidad))) 
    

# Imputar NA por la media 

df <- df %>%
  mutate(
    area_residencial_manzana = ifelse(is.na(area_residencial_manzana), mean(area_residencial_manzana, na.rm = TRUE), area_residencial_manzana),
    valor_catastral_referencia_2022 = ifelse(is.na(valor_catastral_referencia_2022), mean(valor_catastral_referencia_2022, na.rm = TRUE), valor_catastral_referencia_2022),
   numero_predios_manzana = ifelse(is.na(numero_predios_manzana), mean(numero_predios_manzana, na.rm = TRUE), numero_predios_manzana)
  )

# crear variable area construida_residencual_predio 

df <- df%>%
  mutate(area_construida_residencial_predio = area_residencial_manzana/numero_predios_manzana)

# aproximar area_residencial_vivienda con la variable area construida_residencual_predio 

# Crear una función llamada reasignar_valores
reasignar_valores <- function(df) {
  # Definir las nuevas asignaciones para cada localidad
  nuevas_asignaciones <- c(USAQUEN = 66, CHAPINERO = 65, SANTA_FE = 72, SAN_CRISTOBAL = 134,
                           USME = 113, TUNJUELITO = 166, BOSA = 104, KENNEDY = 99, FONTIBON = 71,
                           ENGATIVA = 106, SUBA = 73, BARRIOS_UNIDOS = 117, TEUSAQUILLO = 78,
                           LOS_MARTIRES = 105, ANTONIO_NARINO = 166, PUENTE_ARANDA = 157,
                           LA_CANDELARIA = 85, RAFAEL_URIBE_URIBE = 142, CIUDAD_BOLIVAR = 112)
  
  # Reasignar los valores basados en los nombres de las localidades y el criterio en area_construida_residencial_predio
  df <- df %>%
    mutate(area_construida_residencial_predio = ifelse(area_construida_residencial_predio > 216 & 
                                                         nombre_localidad %in% names(nuevas_asignaciones),
                                                       nuevas_asignaciones[nombre_localidad],
                                                       area_construida_residencial_predio))
  
  # Retornar el dataframe modificado
  return(df)
}

# ajustar en el data frame 
df <- reasignar_valores(df)

df <- df%>%
    mutate(valor_catastral_vivienda = area_construida_residencial_predio*valor_catastral_referencia_2022)

# seleccionar base de datos 

df <- df%>%
  select(property_id,area_residencial_manzana,area_construida_residencial_predio,numero_predios_manzana,
         valor_catastral_referencia_2022,valor_catastral_vivienda,nombre_localidad)

write_csv(df, file = "/Users/apple/Documents/GitHub/Problem_set2_BDML/stores/variables_espaciales.csv")