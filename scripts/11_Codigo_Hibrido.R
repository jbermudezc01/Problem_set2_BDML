##########################################################
# MODELO HIBRIDO - LASSO BOOSTING
# En este codigo vamos a usar lasso en primer lugar para eliminar variables no significativas,
# posteriormente con las variables restantes se estima el boosting
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

# Especificacion del modelo -----------------------------------------------
lasso_spec <- linear_reg(penalty = tune(), mixture = 1) %>%
  set_mode("regression") %>%
  set_engine("glmnet") 

# Definir la grilla para los parametros 
penalty_grid <- grid_regular(penalty(), levels = 1000)

# Receta  -----------------------------------------------------------------
# Para tener la receta primero necesitamos una formula adecuada
variables.exogenas  <-  c('rooms','bathrooms','property_type','piso_numerico','parqueadero','terraza',
                          'ascensor','vigilancia','deposito','cocina_integral','piso_laminado',
                          'surface2','area_residencial_manzana','valor_catastral_referencia_2022',
                          'numero_predios_manzana','nombre_localidad','area_construida_residencial_predio',
                          'valor_catastral_vivienda', 'estrato')#, 'surface_covered_knn', 'surface_total_knn') 
variables.distancia <- colnames(bd)[grep('distancia_', colnames(bd))]

variables.interaccion <- colnames(bd)[grep('inter_', colnames(bd))]

bd.seleccion <- bd %>% 
  select(-c(setdiff(colnames(bd),c(variables.exogenas, variables.distancia, variables.interaccion)))) %>% 
  mutate(log_price = bd$log_price)
bd.seleccion <- as_tibble(bd.seleccion)
bd.seleccion <- bd.seleccion %>% 
  select(-c('geometry'))

# crear subset de entrenamiento
train <-  bd %>%
  subset(type_data == 'train') %>% 
  select(c('type_data',colnames(bd.seleccion)))

# crear subset de testeo
test <- bd %>%
  subset(type_data == 'test') %>% 
  select(c('type_data','property_id',colnames(bd.seleccion)))

# Ya podemos añadir las variables necesarias para la estimacion
receta <- recipe(formula = log_price ~ ., data = bd.seleccion) %>%
  step_novel(all_nominal_predictors()) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors()) %>% 
  step_normalize(all_predictors())

# Workflow ----------------------------------------------------------------
lasso_workflow <- workflow() %>%
  add_recipe(receta) %>%
  add_model(lasso_spec)

# Busqueda hiperparametros CV espacial ------------------------------------
train_sf <- st_as_sf( 
  train,
  coords = c("lon", "lat"), 
  crs = crs(bd)
)

# Realizar una validación cruzada de bloques espaciales
set.seed(123)
block_folds <- spatial_block_cv(train_sf, v = 10) 

# Visualización de pliegues 
# autoplot(block_folds) # visualización de los pliegues 

# Excluimos una zona (a), entrenamos las demás ( b,c,d,e) y validamos con (a) 
# walk(block_folds$splits, function(x) print(autoplot(x)))

# Los bloques espaciales se encuentran en <block_folds>
# Buscamos los hiperparametros basado en el MAE y usando los bloques espaciales
tune.lasso <- tune_grid(
  lasso_workflow,         # El flujo de trabajo que contiene: receta y especificación del modelo
  resamples = block_folds,  # Folds de validación cruzada
  grid = penalty_grid,        # Grilla de valores de penalización
  metrics = metric_set(mae) # Utilizamos la metrica MAE
)

# Podemos graficar los hiperparametros
autoplot(tune.lasso)

# Utilizamos 'collect_metrics' para extraer las métricas de rendimiento de la 
# busqueda de hiperparámetros
collect_metrics(tune.lasso)

# Utilizar 'select_best' para seleccionar el mejor valor de penalización
best_penalty <- select_best(tune.lasso, metric = "mae")

# Finalizar el modelo -----------------------------------------------------
# Actulizar parametros con finalize_workflow() 
lasso_final <- finalize_workflow(lasso_workflow, best_penalty)

# Ajustar el modelo con los datos de train
lasso_final_fit <- fit(lasso_final, data = train)

# Extraer los coeficientes
coeficientes <- lasso_final_fit %>% extract_fit_parsnip() %>% tidy()
coeficientes.no.significativos <- coeficientes %>% 
  filter(estimate == 0)

# Vector de variables no significativas, sacando las localidades
variables.no.significativas <- coeficientes.no.significativos$term[!grepl('nombre_localidad',coeficientes.no.significativos$term)]

# Hacer seleccion de variables para Boosting ------------------------------
bd.boosting <- bd.seleccion %>% 
  select(-c(variables.no.significativas))
test.boosting  <- test %>% 
  select(-c(variables.no.significativas))
train.boosting <- train %>% 
  select(-c(variables.no.significativas))

# Especificacion modelo boosting ------------------------------------------
boost_spec <- boost_tree(
  trees = tune(),
  tree_depth = tune(),
  learn_rate = tune()
) %>%
  set_mode("regression")  

# Tune grid regular para el modelo de boost
boost_grid <- grid_regular(
  trees(range = c(144, 151)),
  tree_depth(range = c(1,1)), # Siempre nos daba el mejor tree_depth 1
  learn_rate(range = c(-0.06,-0.059)),
  levels = 20
)

# Seleccionar variables de distancia restantes
variables.distancia.restantes <- setdiff(variables.distancia, variables.no.significativas)

# Ya podemos añadir las variables necesarias para la estimacion
receta <- recipe(formula = log_price ~ ., data = bd.boosting) %>%
  step_poly(all_of(variables.distancia.restantes), degree = 3) %>% 
  step_novel(all_nominal_predictors()) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors()) %>% 
  step_normalize(all_predictors())

# Workflow ----------------------------------------------------------------
workflow <- workflow() %>%
  add_recipe(receta) %>%
  add_model(boost_spec)

# Estimar el modelo -------------------------------------------------------
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
boost_final_fit <- fit(boost_final, data = train.boosting)

# predecimos el precio para los datos de test.boosting 
test.boosting <- test.boosting %>%
  mutate(log_price = predict(boost_final_fit, new_data = test.boosting)$.pred)

collect_metrics(tune_boost)

augment(boost_final_fit, new_data = bd.boosting) %>%
  mae(truth = log_price, estimate = .pred)

template.kagle <- test.boosting %>% 
  select(property_id, log_price) %>% 
  mutate(price = exp(log_price)) %>% 
  select(property_id, price) %>% 
  st_drop_geometry()

# Exportar a CSV en la carpeta de templates
write.csv(template.kagle, file= paste0(templates,'lassoboosting_penalty',round(best_penalty$penalty,4),'_trees',round(best_parms_boost$trees,4),'_depth',round(best_parms_boost$tree_depth,4),
                                       '_learnrate',round(best_parms_boost$learn_rate,4),'.csv'),
          row.names = F)
