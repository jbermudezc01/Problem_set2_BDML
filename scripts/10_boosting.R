##########################################################
# Modelo boosting
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

# Especificacion del modelo -----------------------------------------------
boost_spec <- boost_tree(
  trees = tune(),
  tree_depth = tune(),
  learn_rate = tune()
) %>%
  set_mode("regression")  

# Tune grid regular para el modelo de boost
boost_grid <- grid_regular(
  trees(range = c(100, 130)),
  tree_depth(range = c(1,1)), # Siempre nos daba el mejor tree_depth 1
  learn_rate(range = c(0.001, 0.005)),
  levels = 20
)

# Receta  -----------------------------------------------------------------
# Para tener la receta primero necesitamos una formula adecuada
variables.exogenas  <-  c('rooms','bathrooms','property_type','piso_numerico','parqueadero','terraza',
                          'ascensor','vigilancia','deposito','cocina_integral','piso_laminado',
                          'surface2','area_residencial_manzana','valor_catastral_referencia_2022',
                          'numero_predios_manzana','nombre_localidad','area_construida_residencial_predio',
                          'valor_catastral_vivienda') 
variables.distancia <- colnames(bd)[grep('distancia_', colnames(bd))]

variables.interaccion <- colnames(bd)[grep('inter_', colnames(bd))]

formula.boosting       <- as.formula(paste('log_price', paste(c(variables.exogenas, variables.distancia, variables.interaccion),collapse ='+'),
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

# Workflow ----------------------------------------------------------------
workflow <- workflow() %>%
  add_recipe(receta) %>%
  add_model(boost_spec)

# Busqueda hiperparametros CV espacial ------------------------------------
train_sf <- st_as_sf( 
  train,
  coords = c("lon", "lat"), 
  crs = crs(bd)
)

# Realizar una validación cruzada de bloques espaciales
set.seed(123)
block_folds <- spatial_block_cv(train_sf, v = 10) 

# estimar el modelo -
tune_boost <- tune_grid(
  workflow, # specifica un modelo de randomn forest
  resamples = block_folds, # Usa los pliegues de validación cruzada definidos previamente en block_folds
  grid = boost_grid, # Especifica una grilla de parámetros a probar en la afinación
  metrics = metric_set(mae) # Usa el Error Absoluto Medio (MAE) como métrica para evaluar los modelos
)

# visualizacion los resultados de la busqueda de hiperparametros 
autoplot(tune_boost)

# Mejores estimaciones de parametros --------------------------------------
best_parms_boost <- select_best(tune_boost, metric = "mae")
best_parms_boost

# Actualizar parametros con finalize_workflow() 
boost_final <- finalize_workflow(workflow, best_parms_boost)

# Ajustar el modelo con los datos de entrenamiento
boost_final_fit <- fit(boost_final, data = train)

# predecimos el precio para los datos de test 
test <- test %>%
  mutate(log_price = predict(boost_final_fit, new_data = test)$.pred)

collect_metrics(tune_boost)
augment(boost_final_fit, new_data = bd) %>%
  mae(truth = log_price, estimate = .pred)

template.kagle <- test %>% 
  select(property_id, log_price) %>% 
  mutate(price = exp(log_price)) %>% 
  select(property_id, price) %>% 
  st_drop_geometry()


# Exportar a CSV en la carpeta de templates
write.csv(template.kagle, 
          file= paste0(templates,'boosting_trees',round(best_parms_boost$trees,4),'_depth',round(best_parms_boost$tree_depth,4),
                       '_learnrate',round(best_parms_boost$learn_rate,4),'.csv'),
          row.names = F)

