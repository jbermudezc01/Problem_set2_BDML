### modelo de predicción: ridge  ##### 

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
       spatialsample, # Muestreo espacial para modelos de aprendizaje automático
       xgboost,
       scals,
       purr,
       glmnet) 

# cargar base de datos 

bd <- read.csv('https://raw.githubusercontent.com/jbermudezc01/Problem_set2_BDML/main/stores/base_datos_tratada.csv')

# crear subset de entrenamiento
train <-  bd %>%
  subset(type_data == 1)

# crear subset de testeo
test <- bd %>%
  subset(type_data == 2)

# modelo lasso 

# especificación del modelo 

ridge_spec <- linear_reg(penalty = tune(), mixture = 0) %>%
  set_mode("regression") %>%
  set_engine("glmnet") 

# definir intervalo de parametros

penalty_grid <- grid_regular(penalty(range = c(-2, 2)), levels = 20)

# receta de preprocesamiento 

receta <- recipe(formula = price ~ bathrooms+bedrooms+
                   rooms+property_type+parqueadero+
                   terraza+ascensor+deposito+vigilancia+cocina_integral+piso_laminado, data = bd) %>% 
  step_novel(all_nominal_predictors()) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors()) %>% 
  step_normalize(all_predictors())

# crear flujo de trabajo 

ridge_workflow <- workflow() %>%
  add_recipe(receta) %>%
  add_model(ridge_spec)

# realizar busqueda de hiperparametros utilizando validación cruzada 

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

# busqueda de hiperparametros 

tune_ridge <- tune_grid(
  ridge_workflow,
  resamples = df_fold, 
  grid = penalty_grid,
  metrics = metric_set(rmse)
)
# grafica de parametros 

autoplot(tune_rigde)

# seleccionar las mejores estimaciones de parametros

best_parms_ridge <- select_best(tune_ridge, metric = "rmse")
best_parms_ridge

# actulizar parametros con finalize_workflow() 

ridge_final <- finalize_workflow(ridge_workflow, best_parms_ridge)

#ajustar el modelo con los datos de test

ridge_final_fit <- fit(ridge_final, data = train)

# predecimos el precio para los datos de test 

test <- test %>%
  mutate(price = predict(ridge_final_fit, new_data = test)$.pred)

# Exportar a CSV
test %>% 
  select(property_id, price) %>% 
  write.csv(file = "03_ridge.csv", row.names = F)
