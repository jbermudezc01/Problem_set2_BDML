##########################################################
# Modelo Ridge
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
ridge_spec <- linear_reg(penalty = tune(), mixture = 0) %>%
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
                          'valor_catastral_vivienda') 
variables.distancia <- colnames(bd)[grep('distancia_', colnames(bd))]
formula.ridge       <- as.formula(paste('log_price', paste(c(variables.exogenas, variables.distancia),collapse ='+'),
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
  #step_interact(terms = as.formula(paste(' ~ ',paste0(paste0(variables.distancia,':','property_type'),collapse =' + ')))) %>% 
  step_novel(all_nominal_predictors()) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors()) %>% 
  step_normalize(all_predictors())

#Nueva receta

receta<-recipe(formula.ridge, data = bd.seleccion) %>%
  step_poly(all_of(variables.distancia), degree = 5) %>%
  step_novel(all_nominal_predictors()) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors()) %>% 
  step_normalize(all_predictors())

# Workflow ----------------------------------------------------------------
ridge_workflow <- workflow() %>%
  add_recipe(receta) %>%
  add_model(ridge_spec)

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
tune.ridge <- tune_grid(
  ridge_workflow,         # El flujo de trabajo que contiene: receta y especificación del modelo
  resamples = block_folds,  # Folds de validación cruzada
  grid = penalty_grid,        # Grilla de valores de penalización
  metrics = metric_set(mae) # Utilizamos la metrica MAE
)

# Podemos graficar los hiperparametros
autoplot(tune.ridge)

# Utilizamos 'collect_metrics' para extraer las métricas de rendimiento de la 
# busqueda de hiperparámetros
collect_metrics(tune.ridge)

# Utilizar 'select_best' para seleccionar el mejor valor de penalización
best_penalty <- select_best(tune.ridge, metric = "mae")

# Finalizar el modelo -----------------------------------------------------
# Actulizar parametros con finalize_workflow() 
ridge_final <- finalize_workflow(ridge_workflow, best_penalty)

# Ajustar el modelo con los datos de test
ridge_final_fit <- fit(ridge_final, data = train)

# Este modelo final se puede aplicar al conjunto de datos de prueba para validar el rendimiento
# Evaluar el modelo de regresión ridge en datos de prueba. Utilizar 'augment' para generar predicciones en datos de prueba y 
# combinar con las respuestas reales
augment(ridge_final_fit, new_data = bd) %>%
  mae(truth = log_price, estimate = .pred)

# Predecir el log(precio) para la base test
test <- test %>%
  mutate(log_price = predict(ridge_final_fit, new_data = test)$.pred)

template.kagle <- test %>% 
  select(property_id, log_price) %>% 
  mutate(price = exp(log_price)) %>% 
  select(property_id, price) %>% 
  st_drop_geometry()

# Exportar a CSV en la carpeta de templates
write.csv(template.kagle, file= paste0(templates,'Ridge_penalty',round(best_penalty$penalty,4),'.csv'),
          row.names = F)
