##########################################################
# Prediccion precios de vivienda
# Autores: Juan Pablo Bermudez. Lina Bautista. Esteban Meza. Pharad Sebastian Escobar
##########################################################


# Cargar pacman (contiene la función p_load)
library(pacman) 

# Cargar las librerías listadas e instalarlas en caso de ser necesario
p_load(tidyverse, # Manipular dataframes
       rio, # Import data easily
       plotly, # Gráficos interactivos
       leaflet, # Mapas interactivos
       rgeos, # Calcular centroides de un poligono
       tmaptools, # geocode_OSM()
       sf, # Leer/escribir/manipular datos espaciales
       osmdata, # Get OSM's data 
       tidymodels) #para modelos de ML

# cargar base de datos 

bd <- read.csv('https://raw.githubusercontent.com/jbermudezc01/Problem_set2_BDML/main/stores/base_datos_tratada.csv')

# crear subset de entrenamiento
train <-  bd %>%
  subset(type_data == 1)

# crear subset de testeo
test <- bd %>%
  subset(type_data == 2)


# Especificación del modelo
#Mixture puede tomar valores entre 0 y 1, sin embargo tenemos que hacer CV para saber cuál usar
elastic_net_spec <- linear_reg(penalty = lambda, mixture = .5) %>%
  set_engine("glmnet")




# Primera receta
rec_1 <- recipe(price ~ distancia_parque + area_parque + rooms + bathrooms + property_type +parqueadero, data = db) %>%
  step_interact(terms = ~ distancia_parque:property_type+area_parque:property_type) %>% 
  step_novel(all_nominal_predictors()) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors()) %>% 
  step_normalize(all_predictors())

# Segunda receta 
rec_2 <- recipe(price ~ distancia_parque + area_parque + bathrooms + property_type +parqueadero+surface_total, data = db) %>%
  step_interact(terms = ~ distancia_parque:property_type+area_parque:property_type) %>% 
  step_poly(distancia_parque, area_parque, degree = 2) %>%
  step_novel(all_nominal_predictors()) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors()) %>% 
  step_normalize(all_predictors())



# Crear un flujo de trabajo que incluye la receta de preprocesamiento y el modelo
workflow_1.1 <- workflow() %>%
  add_recipe(rec_1) %>%
  add_model(ridge_spec)

workflow_1.2 <- workflow() %>%
  add_recipe(rec_1) %>%
  add_model(lasso_spec)

workflow_1.3 <- workflow() %>%
  add_recipe(rec_1) %>%
  add_model(elastic_net_spec)


workflow_2.1 <- workflow() %>%
  add_recipe(rec_2) %>%
  add_model(ridge_spec)

workflow_2.2 <- workflow() %>%
  add_recipe(rec_2) %>%
  add_model(lasso_spec)

workflow_2.3 <- workflow() %>%
  add_recipe(rec_2) %>%
  add_model(elastic_net_spec)



##ACÁ SIGUE FIT Y PREDICT