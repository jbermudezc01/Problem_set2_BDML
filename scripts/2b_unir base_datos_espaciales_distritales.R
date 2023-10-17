##########################################################
# Merge base de datos del codigo 02_Limpieza_Datos.R con mas datos espaciales
# Creacion de nuevos predictores con bases de datos espaciales de Bogotá
# Autores: Juan Pablo Bermudez. Lina Bautista. Esteban Meza. Pharad Sebastian Escobar
##########################################################

# Al final del codigo sale una base de datos juntada con la que sale de 02_limpieza_datos.R
if(0){
  # unir variables construidas con bases de datos espaciales de bogotá 
  # cargar datos de variables espaciales
  ve <- read.csv("https://raw.githubusercontent.com/jbermudezc01/Problem_set2_BDML/main/stores/variables_espaciales.csv")
  # unir a bd 
  bd <- merge(bd, ve, by = "property_id")
}

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

# Bases de datos de Bogota ------------------------------------------------

manzanas <- st_read(paste0(datos.bogota,'manzanas_bogota/MANZ.shp')) # De manzanas
valor_referencia <- st_read(paste0(datos.bogota,'valor_catastral_referencia_2022_bogota/Valor_ref_2022.shp')) # Valor catastral de referencia 
area_construida <- st_read(paste0(datos.bogota,'Area_construida_residencial_manzana_bogota/AconstruidaResidencial.shp')) # Area construida residencial 
cantidad_predios <-st_read(paste0(datos.bogota,'cantidad_predios_manzana_bogota/CantidadPM.shp')) # Cantidad de predios 
localidades <- st_read(paste0(datos.bogota,'localidades_bogota/Loca.shp')) # Localidades

# Base de datos del taller ------------------------------------------------
load(paste0(stores,'Datos_limpios.RData'))
# Al cargar 'Datos_limpios.RData', obtenemos una base de datos que se llama bd

# Convertir base de datos del taller en objeto sf 
bd <- st_as_sf(bd, coords = c("lon","lat"), crs = 4326)

# Identificar el tipo de geometria 
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
bd <- st_join(bd, area_construida_valid, join = st_within)

# valor catastral de refencia 
bd <- st_join(bd, valor_referencia_valid, join = st_within)

# cantidad de predios
bd <- st_join(bd,cantidad_predios_valid, join=st_within)

# localidad 
# convertir a sistema de coordenadas de bd 
bd <- st_transform(bd, crs = st_crs(localidades_valid))
#unir base de datos 
bd <- st_join(bd,localidades_valid,join = st_within)

# eliminar los valores duplicados 
bd <- bd %>% 
  distinct(property_id, geometry, .keep_all = TRUE)

# limpiar base de datos 

bd <- bd %>%
  select(-c(OBJECTID,OBJECTID.x,OBJECTID.y,MANCODIGO.x,MANCODIGO.y,
            ANO.x,ANO.y,ANO,SHAPE_AREA.x,SHAPE_LEN.x,SHAPE_Area.x,SHAPE_Leng.x,
            SHAPE_AREA.y,SHAPE_LEN.y,SHAPE_Leng.y,SHAPE_Area.y,LocAAdmini,LocArea))

# renombrar variables 

bd <- bd%>%
  rename(area_residencial_manzana = AREA_RESID,
         valor_catastral_referencia_2022 = V_REF,
         codigo_manzana = MANCODIGO,
         numero_predios_manzana = N_PREDIOS,
         nombre_localidad = LocNombre,
         codigo_localidad = LocCodigo)

# transformar a factor variable localidad 
bd <- bd%>%
mutate(nombre_localidad = as.factor(nombre_localidad))


# identificar los NA para las variables agregadas

bd %>% 
  summarise(
    na_count_area = sum(is.na(area_residencial_manzana)), # 37,7 %
    na_count_valor =sum(is.na(valor_catastral_referencia_2022)),
    na_count_predios = sum(is.na(numero_predios_manzana)), # 33,4 %
    na_count_localidad =sum(is.na(codigo_localidad))) 
    

# Imputar NA por la media 

bd <- bd %>%
  mutate(
    area_residencial_manzana = ifelse(is.na(area_residencial_manzana), mean(area_residencial_manzana, na.rm = TRUE), area_residencial_manzana),
    valor_catastral_referencia_2022 = ifelse(is.na(valor_catastral_referencia_2022), mean(valor_catastral_referencia_2022, na.rm = TRUE), valor_catastral_referencia_2022),
   numero_predios_manzana = ifelse(is.na(numero_predios_manzana), mean(numero_predios_manzana, na.rm = TRUE), numero_predios_manzana)
  )

# crear variable area construida_residencual_predio 

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

bd <- bd%>%
  select(property_id,area_residencial_manzana,area_construida_residencial_predio,numero_predios_manzana,
         valor_catastral_referencia_2022,valor_catastral_vivienda,nombre_localidad)

write_csv(bd, file = "/Users/apple/Documents/GitHub/Problem_set2_BDML/stores/variables_espaciales.csv")