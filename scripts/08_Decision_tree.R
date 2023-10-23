##########################################################
# Modelo Arboles de Decision
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

# Especificacion modeo ----------------------------------------------------
tree_model <- decision_tree(tree_depth = tune(),
                            min_n = tune()) %>% 
  set_engine("rpart") %>% 
  set_mode("regression") 

# Grilla de parametros
tree_grid <- grid_random(
  tree_depth(range = c(1, 30), trans= NULL),# modificar para cada modelo 
  min_n(range = c(3,15 ), trans = NULL),# modificar para cada modelo 
  size = 100
)
head(tree_grid, n=20)
dim(tree_grid)

#-----Nueva receta que incluye surface y las variables de OSM

## Obtener los nombres de las variables que contienen "distancia"

variables.distancia <- colnames(bd)[grep('distancia_', colnames(bd))]
outcome <- 'price'
exo     <- c('rooms','bathrooms','property_type','piso_numerico','parqueadero','terraza',
             'ascensor','vigilancia','deposito','cocina_integral','piso_laminado',
             'surface2','area_residencial_manzana','valor_catastral_referencia_2022',
             'numero_predios_manzana','nombre_localidad','area_construida_residencial_predio',
             'valor_catastral_vivienda') 
formula <- as.formula(paste('price', paste(c(exo, variables.distancia),collapse = '+'),sep='~'))


bd.seleccion <- bd %>% 
  select(-c(setdiff(colnames(bd),c(exo, variables.distancia)))) %>% 
  mutate(log_price = bd$log_price)
bd.seleccion <- as_tibble(bd.seleccion)
bd.seleccion <- bd.seleccion %>% 
  select(-c('geometry'))

receta <- recipe(formula = log_price ~ ., data = bd.seleccion) %>%
  step_poly(all_of(variables.distancia), degree = 3) %>%
  step_novel(all_nominal_predictors()) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors()) %>% 
  step_normalize(all_predictors())

# crear flujo de trabajo

workflow <- workflow() %>%
  add_recipe(receta) %>%
  add_model(tree_model)

# validación cruzada espacial para entrenamiento de parametros

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

# excluimos una zona (a), entrenamos las demás ( b,c,d,e) y validamos con (a) 

walk(block_folds$splits, function(x) print(autoplot(x)))


# estimar el modelo - long time con de entrenamiento ....

tune_tree <- tune_grid(
  workflow, # specifica un modelo de árbol de decisión
  resamples = block_folds, # Usa los pliegues de validación cruzada definidos previamente en block_folds
  grid = tree_grid, # Especifica una grilla de parámetros a probar en la afinación
  metrics = metric_set(mae) # Usa el Error Absoluto Medio (MAE) como métrica para evaluar los modelos
)

# visualizacion los resultados de la busqueda de hiperparametros 
autoplot(tune_tree)
collect_metrics(tune_tree)

# seleccionar las mejores estimaciones de parametros

best_parms_tree <- select_best(tune_tree, metric = "mae")
best_parms_tree
# actulizar parametros con finalize_workflow() 

tree_final <- finalize_workflow(workflow, best_parms_tree)

#ajustar el modelo con los datos de test
tree_final_fit <- fit(tree_final, data = train)

#MAE de la nueva predicción
augment(tree_final_fit, new_data = bd) %>%
  mae(truth = log_price, estimate = .pred)
# predecimos el precio para los datos de test 

test <- test %>%
  mutate(log_price = predict(tree_final_fit, new_data = test)$.pred)


template.kagle <- test %>% 
  select(property_id, log_price) %>% 
  mutate(price = exp(log_price)) %>% 
  select(property_id, price) %>% 
  st_drop_geometry()

# Exportar a CSV en la carpeta de templates
write.csv(template.kagle, file= paste0(templates,'Decision_tree_depth',round(best_parms_tree$tree_depth,4),'min_n',round(best_parms_tree$min_n),'.csv'),
          row.names = F)
