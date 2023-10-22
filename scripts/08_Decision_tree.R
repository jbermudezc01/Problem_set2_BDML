### modelo de predicción: Arboles de decisión ##### 

## limpieza y transformación de datos ##

#Limpieza area de trabajo 
rm(list=ls())
cat('\014')

# cargar paquetes 
install.packages("pacman")
library(pacman)
# cargar librerias 
p_load(tidyverse, # Manipular dataframes
       rio, # Importar datos fácilmente
       plotly, # Gráficos interactivos
       leaflet, # Mapas interactivos
       rgeos, # Calcular centroides de un polígono
       units, # unidades
       sf, # Leer/escribir/manipular datos espaciales
       osmdata, # Obtener datos de OpenStreetMap (OSM)
       tidymodels, # Modelado de datos limpios y ordenados
       randomForest, # Modelos de bosque aleatorio
       rattle, # Interfaz gráfica para el modelado de datos
       spatialsample,
       xgboost,
       scals,
       purr) # Muestreo espacial para modelos de aprendizaje automático

# cargar base de datos 
bd <-load(paste0(getwd(),'/stores/Datos_limpios.RData'))

# crear subset de entrenamiento
train <-  bd %>%
  subset(type_data == "train")

# crear subset de testeo
test <- bd %>%
  subset(type_data == "test")

### modelo de arboles de regresión con tidymodelss ###

# especificación del modelo 
tree_model <- decision_tree(tree_depth = tune(),
                                        min_n      = tune()) %>% 
  set_engine("rpart") %>% 
  set_mode("regression") 

# crear grilla de parametros 

tree_grid <- grid_random(
  tree_depth(range = c(1, 20), trans= NULL),# modificar para cada modelo 
  min_n(range = c(20,3000 ), trans = NULL),# modificar para cada modelo 
  size = 100
)
head(tree_grid, n=20)
dim(tree_grid)

#-----Nueva receta que incluye surface y las variables de OSM

## Obtener los nombres de las variables que contienen "distancia"

variables.distancia <- grep('distancia_',colnames(bd))
outcome <- 'price'
exo     <- c('bathrooms','bedrooms','parqueadero','terraza','ascensor','deposito','vigilancia','cocina_integral', 'surface2', 'piso_numerico')  
columnas.distancia <- colnames(bd)[variables.distancia]
formula <- as.formula(paste(outcome, paste(c(exo, columnas.distancia),collapse = '+'),sep='~'))



receta <-recipe(formula, data = bd) %>%
  step_novel(all_nominal_predictors()) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors())

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

# Crear pliegues para la validación cruzada

df_fold <- vfold_cv(train, v = 5)

# estimar el modelo - long time con de entrenamiento ....

tune_tree <- tune_grid(
  workflow, # specifica un modelo de árbol de decisión
  resamples = block_folds, # Usa los pliegues de validación cruzada definidos previamente en block_folds
  grid = tree_grid, # Especifica una grilla de parámetros a probar en la afinación
  metrics = metric_set(mae) # Usa el Error Absoluto Medio (MAE) como métrica para evaluar los modelos
)

# visualizacion los resultados de la busqueda de hiperparametros 
autoplot(tune_tree)

# seleccionar las mejores estimaciones de parametros

best_parms_tree <- select_best(tune_tree, metric = "mae")
best_parms_tree

# actulizar parametros con finalize_workflow() 

tree_final <- finalize_workflow(workflow, best_parms_tree)

#ajustar el modelo con los datos de test
tree_final_fit <- fit(tree_final, data = train)

# predecimos el precio para los datos de test 

test <- test %>%
  mutate(price = predict(tree_final_fit, new_data = test)$.pred)

# Exportar a CSV
test %>% 
  select(property_id, price) %>% 
  write.csv(file = "04_decision_tree.csv", row.names = F)

