##########################################################
# Modelo Random Forest
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
       glmnet) 

# Directorios -------------------------------------------------------------
stores    <- paste0(getwd(),'/stores/') # Directorio de base de datos
views     <- paste0(getwd(),'/views/')  # Directorio para guardar imagenes
templates <- paste0(getwd(),'/templates/') # Directorio para crear templates

# Cargar base de datos ----------------------------------------------------
load(paste0(stores,'Datos_limpios.RData'))

# crear subset de entrenamiento
train <-  bd %>%
  subset(type_data == 'train')

# crear subset de testeo
test <- bd %>%
  subset(type_data == 'test')


# Modelo Random Forest ----------------------------------------------------
# especificar el modelo 

rf_spec<- rand_forest(
  mtry = tune(),              # Hiperparámetro: Número de variables a considerar en cada división
  min_n = tune(),             # Hiperparámetro: Profundidad mínima del árbol
  trees = tune(),
) %>%
  set_engine("randomForest") %>%
  set_mode("regression")       # Cambiar a modo de regresión

# crear grilla de parametros 

rf_grid<- grid_random(mtry(range = c(4,6)), # El hiperparámetro mtry determina el número de variables seleccionadas aleatoriamente en cada división al construir un árbol. En este caso, se consideran valores de 2 a 4.
                              min_n(range = c(1, 10)), # El hiperparámetro min_n determina el número mínimo de observaciones que debe haber en un nodo para que se produzca una división. En este caso, se están considerando valores entre 1 y 10.
                              trees(range = c(100, 300)), size = 5)# El hiperparámetro trees determina el número de árboles en el bosque. Aquí, se están considerando valores entre 100 y 300 árboles. size determina el núnmero de combinaciones de hiperparametros 

# Receta  -----------------------------------------------------------------
# Para tener la receta primero necesitamos una formula adecuada
variables.exogenas  <-  c('rooms','bathrooms','property_type','piso_numerico','parqueadero','terraza',
                          'ascensor','vigilancia','deposito','cocina_integral','piso_laminado',
                          'surface2','area_residencial_manzana','valor_catastral_referencia_2022',
                          'numero_predios_manzana','nombre_localidad','area_construida_residencial_predio',
                          'valor_catastral_vivienda') 
variables.distancia <- colnames(bd)[grep('distancia_', colnames(bd))]
formula.elastic       <- as.formula(paste('log_price', paste(c(variables.exogenas, variables.distancia),collapse ='+'),
                                          sep='~'))

bd.seleccion <- bd %>% 
  select(-c(setdiff(colnames(bd),c(variables.exogenas, variables.distancia)))) %>% 
  mutate(log_price = bd$log_price)
bd.seleccion <- as_tibble(bd.seleccion)
bd.seleccion <- bd.seleccion %>% 
  select(-c('geometry'))

# Ya podemos añadir las variables necesarias para la estimacion
receta <- recipe(formula = log_price ~ ., data = bd.seleccion) %>%
  step_poly(all_of(variables.distancia), degree = 3) %>% 
  step_novel(all_nominal_predictors()) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors()) %>% 
  step_normalize(all_predictors())
# especificar el flujo de trabajo

workflow <- workflow() %>%
  add_recipe(receta) %>%
  add_model(rf_spec)

# validación cruzada espacial 

# definimos el data set train como  sf
train_sf <- st_as_sf( 
  train,
  coords = c("lon", "lat"), 
  crs = 4326
)

# realizar una validación cruzada de bloques espaciales

set.seed(123)
block_folds <- spatial_block_cv(train_sf, v = 5) 

# visualización de plieges 
autoplot(block_folds) # visualización de los plieges 

# Crear pliegues para la validación cruzada
# estimar el modelo - long time ....

tune_forest <- tune_grid(
  workflow, # specifica un modelo de randomn forest
  resamples = block_folds, # Usa los pliegues de validación cruzada definidos previamente en block_folds
  grid = rf_grid, # Especifica una grilla de parámetros a probar en la afinación
  metrics = metric_set(mae) # Usa el Error Absoluto Medio (MAE) como métrica para evaluar los modelos
)

# visualizacion los resultados de la busqueda de hiperparametros 
autoplot(tune_forest)

# seleccionar las mejores estimaciones de parametros

best_parms_forest <- select_best(tune_forest, metric = "mae")
best_parms_forest

# actulizar parametros con finalize_workflow() 

forest_final <- finalize_workflow(workflow, best_parms_forest)

#ajustar el modelo con los datos de test

forest_final_fit <- fit(forest_final, data = train)

# predecimos el precio para los datos de test 

test <- test %>%
  mutate(price = predict(forest_final_fit, new_data = test)$.pred)

# Exportar a CSV
test %>% 
  select(property_id, price) %>% 
  write.csv(file = "01_random_forest.csv", row.names = F)


